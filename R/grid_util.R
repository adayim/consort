# The current device's drawable area in inches, or NULL if there is no
# device open yet (or its size can't be determined) -- callers treat NULL
# as "no constraint".
#' @keywords internal
#' @importFrom grDevices dev.size
get_device_size_in <- function() {
  if (dev.cur() == 1L) return(NULL)
  sz <- tryCatch(dev.size("in"), error = function(e) NULL)
  if (is.null(sz) || length(sz) != 2L || any(!is.finite(sz)) || any(sz <= 0))
    return(NULL)
  sz
}

# Factor (0, 1] to shrink a layout measuring `width_chars` x `height_chars`
# (in "char" units, as returned by calc_coords()) so that it fits within the
# current device. Returns 1 (no shrink) when the layout already fits, or
# when the device size can't be determined.
#' @keywords internal
calc_shrink_factor <- function(width_chars, height_chars, margin = 0.98) {
  dev_sz <- get_device_size_in()
  if (is.null(dev_sz)) return(1)

  width_in  <- convertWidth(unit(width_chars, "char"), "in", valueOnly = TRUE)
  height_in <- convertHeight(unit(height_chars, "char"), "in", valueOnly = TRUE)
  if (!is.finite(width_in) || !is.finite(height_in) || width_in <= 0 || height_in <= 0)
    return(1)

  min(1, dev_sz[1] * margin / width_in, dev_sz[2] * margin / height_in)
}

# Return a copy of a textbox grob with its text scaled by `factor`. Used to
# shrink an already-built box (and its cached measurement) rather than
# rebuilding it from scratch.
#' @keywords internal
rescale_textbox <- function(box, factor) {
  old_cex <- if (is.null(box$txt_gp$cex)) 1 else box$txt_gp$cex
  new_gp <- box$txt_gp
  new_gp$cex <- old_cex * factor
  editGrob(box, txt_gp = new_gp, hw_cache = new.env(parent = emptyenv()))
}

# Apply rescale_textbox() to every node's box in a consort-plot-like list
# (main nodes or label nodes), refreshing the cached box_hw. List-level
# attributes (nodes.list etc.) are untouched by the element replacement.
#' @keywords internal
rescale_nodes <- function(plot_list, factor) {
  for (nm in names(plot_list)) {
    nd <- plot_list[[nm]]
    if (!is.null(nd$box)) {
      nd$box <- rescale_textbox(nd$box, factor)
      nd$box_hw <- get_size(nd$box)
      plot_list[[nm]] <- nd
    }
  }
  plot_list
}

# Stack rows top-to-bottom, with extra padding at split/merge transitions
#' @keywords internal
calc_y_coords <- function(consort_plot, nodes_layout, pad_u) {
  nd_y <- vector("list", length = length(nodes_layout))
  prev_bt <- 0

  for (i in seq_along(nodes_layout)) {
    heights <- sapply(consort_plot[nodes_layout[[i]]], function(x)
      get_size(x$box)$height
    )

    if (i == 1) {
      nd_y[[i]] <- heights / 2 + pad_u / 2
      prev_bt <- max(heights)
    } else {
      # Extra padding when column count changes (split/merge transition)
      extra_pad <- if (length(heights) != length(nd_y[[i - 1]])) 2 * pad_u else pad_u
      nd_y[[i]] <- prev_bt + extra_pad + heights / 2
      prev_bt <- prev_bt + extra_pad + max(heights)
    }
    names(nd_y[[i]]) <- names(heights)
  }

  list(nd_y = nd_y, total_height = prev_bt + pad_u)
}

# Calculate X positions for multi-column rows, accounting for sidebox extents.
# main_col_widths: max node width per column from non-sidebox rows
# sb_wd_mat: matrix of widths from sidebox rows (rows x columns), or NULL
# sb_sd_mat: matrix of sides ("left"/"right") from sidebox rows, or NULL
#' @keywords internal
calc_column_x <- function(main_col_widths, sb_wd_mat, sb_sd_mat, pad_u) {
  n_cols <- length(main_col_widths)
  has_sb <- !is.null(sb_sd_mat)

  # Distance from a column's center to its outer edge on `side`: at least
  # the column's own half-width, further out if a sidebox sits there (a
  # sidebox on `side` is centered at half its width + pad_u/2 past the
  # column center, see place_sideboxes()).
  col_extent <- function(j, side) {
    extent <- main_col_widths[j] / 2
    if (has_sb) {
      on_side <- sb_sd_mat[, j] == side
      if (any(on_side)) {
        extent <- max(extent, max(sb_wd_mat[on_side, j]) + pad_u / 2)
      }
    }
    extent
  }

  pos <- numeric(n_cols)
  for (j in seq_len(n_cols)) {
    if (j > 1) {
      pos[j] <- pos[j - 1] + col_extent(j - 1, "right") + pad_u + col_extent(j, "left")
    }
  }

  pos - mean(pos)
}

# Offset sidebox nodes from their parent column's X position
#' @keywords internal
place_sideboxes <- function(col_x, sb_widths, sb_sides, pad_u) {
  n_cols <- length(col_x)
  sb_x <- numeric(n_cols)
  for (k in seq_len(n_cols)) {
    sb_x[k] <- if (sb_sides[k] == "right") {
      col_x[k] + sb_widths[k] / 2 + pad_u / 2
    } else {
      col_x[k] - sb_widths[k] / 2 - pad_u / 2
    }
  }
  sb_x
}

# Recenter parent nodes at the midpoint of their children after a second split
#' @keywords internal
adjust_multisplit <- function(nd_x, nd_type, nodes_layout, consort_plot) {
  n_splits <- sum(nd_type == "splitbox")
  if (n_splits <= 1) return(nd_x)
  if (n_splits > 2) stop("More than two splits are not supported.")

  split_idx <- which(nd_type == "splitbox")[-1]
  prev_nodes <- sapply(unlist(nodes_layout[split_idx]), function(y) {
    consort_plot[[y]]$prev_node
  }, simplify = FALSE)
  prev_nodes <- unlist(prev_nodes)

  for (parent in unique(prev_nodes)) {
    children_x <- nd_x[[split_idx]][names(prev_nodes[prev_nodes == parent])]
    nd_x[[split_idx - 1]][parent] <- mean(range(children_x))
  }

  nd_x
}

# Shift all X coordinates so minimum is 0 and compute final bounds
#' @keywords internal
normalize_x <- function(nd_x, nd_wd, nodes_layout) {
  nd_minmax <- lapply(seq_along(nodes_layout), function(i) {
    n_cols <- length(nodes_layout[[i]])
    if (n_cols > 1) {
      wd_mat <- do.call(rbind, nd_wd[i])
      x_mat  <- do.call(rbind, nd_x[i])
      c(minx = min(x_mat[, 1] - wd_mat[, 1] / 2),
        maxx = max(x_mat[, n_cols] + wd_mat[, n_cols] / 2))
    } else {
      c(minx = nd_x[[i]] - nd_wd[[i]] / 2,
        maxx = nd_x[[i]] + nd_wd[[i]] / 2)
    }
  })

  bounds <- do.call(rbind, Filter(Negate(is.null), nd_minmax))
  min_val <- min(bounds[, 1])
  max_val <- max(bounds[, 2])

  for (i in seq_along(nodes_layout)) {
    nd_x[[i]] <- nd_x[[i]] - min_val
  }

  list(nd_x = nd_x, max_width = max_val - min_val)
}

# Calculate coordinates
#' @keywords internal
#' @importFrom stats setNames
calc_coords <- function(consort_plot) {

  nodes_layout <- attr(consort_plot, "nodes.list")

  # Node type per row
  nd_type <- sapply(nodes_layout, function(x)
    unique(sapply(consort_plot[x], "[[", "node_type"))
  )

  if (nd_type[length(nd_type)] == "sidebox")
    stop("The last node can not be a side box.")

  pad_u <- consort_opt("pad_u")
  if (!is.numeric(pad_u) || length(pad_u) != 1L || is.na(pad_u))
    stop("`pad_u` must be a single, non-NA numeric value.")

  # --- Phase 1: Y coordinates ---
  y_result <- calc_y_coords(consort_plot, nodes_layout, pad_u)
  nd_y <- y_result$nd_y

  # --- Phase 2: Gather node widths and sides ---
  nd_wd <- lapply(nodes_layout, function(nd) {
    sapply(consort_plot[nd], function(x) get_size(x$box)$width)
  })

  nd_sides <- lapply(nodes_layout, function(nd) {
    unlist(sapply(consort_plot[nd], function(x) x$side))
  })

  # --- Phase 3: X coordinates ---
  nd_x <- vector("list", length = length(nodes_layout))
  col_counts <- sapply(nodes_layout, length)
  row_groups <- gp_consecutive(col_counts)

  for (gp in unique(row_groups)) {
    gp_rows <- which(row_groups == gp)
    n_cols <- unique(col_counts[gp_rows])
    gp_types <- nd_type[gp_rows]

    if (n_cols == 1) {
      # Single-column: main nodes at center, sideboxes offset
      for (r in gp_rows) {
        if (nd_type[r] != "sidebox") {
          nd_x[[r]] <- 0
        } else {
          nd_x[[r]] <- ifelse(nd_sides[[r]] == "right",
                              nd_wd[[r]] / 2 + pad_u,
                              -nd_wd[[r]] / 2 - pad_u)
        }
        names(nd_x[[r]]) <- nodes_layout[[r]]
      }
    } else {
      # Multi-column
      wd_mat <- do.call(rbind, nd_wd[gp_rows])
      is_sb <- gp_types == "sidebox"

      if (any(is_sb)) {
        # Column positions with sidebox-aware spacing
        main_col_widths <- apply(wd_mat[!is_sb, , drop = FALSE], 2, max)
        sb_wd_mat <- wd_mat[is_sb, , drop = FALSE]
        sb_sd_mat <- do.call(rbind, nd_sides[gp_rows][is_sb])

        col_x <- calc_column_x(main_col_widths, sb_wd_mat, sb_sd_mat, pad_u)

        for (r in gp_rows) {
          if (nd_type[r] != "sidebox") {
            nd_x[[r]] <- col_x
          } else {
            nd_x[[r]] <- place_sideboxes(col_x, nd_wd[[r]], nd_sides[[r]], pad_u)
          }
          names(nd_x[[r]]) <- nodes_layout[[r]]
        }
      } else {
        # No sideboxes: simple equal spacing
        col_widths <- apply(wd_mat, 2, max)
        col_x <- col_widths / 2 + c(0, cumsum(col_widths[-length(col_widths)] + 4 * pad_u))
        col_x <- col_x - mean(col_x)

        for (r in gp_rows) {
          nd_x[[r]] <- col_x
          names(nd_x[[r]]) <- nodes_layout[[r]]
        }
      }
    }
  }

  # --- Phase 4: Multiple split adjustment ---
  nd_x <- adjust_multisplit(nd_x, nd_type, nodes_layout, consort_plot)

  # --- Phase 5: Normalize to positive coordinates ---
  x_result <- normalize_x(nd_x, nd_wd, nodes_layout)

  list(
    x = unlist(x_result$nd_x),
    y = unlist(nd_y),
    nodes_hw = nd_wd,
    nd_x = x_result$nd_x,
    nd_y = nd_y,
    max_width = x_result$max_width,
    max_height = y_result$total_height
  )
}

# Calculate coordinates
#' @keywords internal
#'
calc_coords_label <- function(label_plot, node_y, max_h){

  lab_wd <- sapply(label_plot, function(x){
    sz <- get_size(x$box)
    c(w = sz$width, h = sz$height)
  })
  
  lab_pos <- sapply(label_plot, function(x){
    x$prev_node
  })
  
  lab_y <- node_y[lab_pos]
  lab_y <- sapply(lab_y, mean)
  lab_y <- (max_h - lab_y)/max_h
  names(lab_y) <- colnames(lab_wd)

  lab_x <- (lab_wd["w",]/2)
  names(lab_x) <- colnames(lab_wd)

  return(list(width = max(lab_wd["w",]),
              x = lab_x, # Put inside
              y = lab_y))
  
}

# Create groups if consecutive 
#' @keywords internal
#'
#'
gp_consecutive <- function(x){
  int <- 1
  gp <- vector("character", length = length(x))
  gp[1] <- as.character(int)
  if(length(x) > 1){
    for(i in 2:length(x)){
      if(x[i] != x[i-1])
        int <- int + 1
      gp[i] <- as.character(int)
    }
  }
  return(gp)
}