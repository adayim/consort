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

# Layout parents of every main (vertbox/splitbox) node, as a named list of
# node names. Usually the declared `prev_node`. When a node's text is empty,
# `add_box()` records the placeholder's own parent in the next row instead,
# but the next node still sits in the placeholder's column, so the node at the
# same position in the preceding main row is its layout parent.
#' @keywords internal
layout_parents <- function(consort_plot, nodes_layout, main_rows) {
  lay_par <- list()
  for (i in seq_along(main_rows)) {
    r <- main_rows[i]
    prev_row <- if (i > 1) nodes_layout[[main_rows[i - 1]]] else NULL

    for (pos in seq_along(nodes_layout[[r]])) {
      nm <- nodes_layout[[r]][pos]
      pn <- as.character(consort_plot[[nm]]$prev_node)
      if (length(pn) == 1L && !is.null(prev_row) && !pn %in% prev_row &&
          length(prev_row) == length(nodes_layout[[r]])) {
        pn <- prev_row[pos]
      }
      lay_par[[nm]] <- pn
    }
  }
  lay_par
}

# A contour holds, per row, the leftmost and rightmost x a subtree reaches
# (NA where it has nothing on that row).
#' @keywords internal
merge_contour <- function(a, b) {
  list(left = pmin(a$left, b$left, na.rm = TRUE),
       right = pmax(a$right, b$right, na.rm = TRUE))
}

#' @keywords internal
shift_contour <- function(cont, by) {
  list(left = cont$left + by, right = cont$right + by)
}

# Calculate X positions of all nodes as a tree layout (Reingold-Tilford).
#
# Main nodes form a forest through their layout parents. A node with no or
# several layout parents (the first row, or a merge) starts a new segment;
# each segment is laid out on its own and then centred under its parents.
# Within a segment, sibling subtrees are placed left to right, each one pushed
# right until its contour clears the contours of the siblings before it by
# `sib_gap` on every row, and a parent is centred over its first and last
# child. A side box is not a child: it widens its anchor's contour on the row
# it occupies, and is placed beside the anchor afterwards.
#
# Returns a list with one named numeric vector per row of `nodes_layout`.
#' @keywords internal
calc_tree_x <- function(consort_plot, nodes_layout, nd_type, nd_wd, pad_u) {
  sib_gap   <- 2 * pad_u  # minimum gap between neighbouring subtrees on a row
  sb_offset <- pad_u / 2  # gap between a column's line and its side box

  n_rows <- length(nodes_layout)
  all_nd <- unlist(nodes_layout)
  row_of <- setNames(rep(seq_len(n_rows), lengths(nodes_layout)), all_nd)
  width  <- setNames(unlist(nd_wd, use.names = FALSE), all_nd)

  main_rows <- which(nd_type %in% c("vertbox", "splitbox"))
  side_rows <- which(nd_type == "sidebox")
  main_nd   <- unlist(nodes_layout[main_rows])

  # Side boxes by anchor node
  side_of <- list()
  for (r in side_rows) {
    for (nm in nodes_layout[[r]]) {
      anchor <- consort_plot[[nm]]$prev_node
      if (length(anchor) != 1L || !anchor %in% nodes_layout[[r - 1L]])
        stop("A side box must be attached to a node in the row above it.")
      side_of[[anchor]] <- c(side_of[[anchor]], nm)
    }
  }

  lay_par <- layout_parents(consort_plot, nodes_layout, main_rows)

  children <- list()
  for (nm in main_nd) {
    if (length(lay_par[[nm]]) == 1L)
      children[[lay_par[[nm]]]] <- c(children[[lay_par[[nm]]]], nm)
  }

  x <- setNames(numeric(length(main_nd)), main_nd)
  members <- list()  # node -> all nodes in its subtree, itself included

  # Contour of `v` alone: its box, and the line and side box on the row below
  own_contour <- function(v) {
    left <- right <- rep(NA_real_, n_rows)
    r <- row_of[[v]]
    left[r]  <- x[[v]] - width[[v]] / 2
    right[r] <- x[[v]] + width[[v]] / 2

    for (sb in side_of[[v]]) {
      s <- row_of[[sb]]
      left[s] <- right[s] <- x[[v]]
      if (!is_empty(consort_plot[[sb]]$text)) {
        if (identical(consort_plot[[sb]]$side, "left")) {
          left[s] <- x[[v]] - sb_offset - width[[sb]]
        } else {
          right[s] <- x[[v]] + sb_offset + width[[sb]]
        }
      }
    }
    list(left = left, right = right)
  }

  # Lay out each subtree in `nodes` and place them side by side; returns the
  # merged contour of all of them
  place_siblings <- function(nodes) {
    combined <- NULL
    for (k in nodes) {
      cont <- layout_node(k)
      if (!is.null(combined)) {
        gap <- combined$right + sib_gap - cont$left
        shift <- if (all(is.na(gap))) 0 else max(gap, na.rm = TRUE)
        x[members[[k]]] <<- x[members[[k]]] + shift
        cont <- shift_contour(cont, shift)
      }
      combined <- if (is.null(combined)) cont else merge_contour(combined, cont)
    }
    combined
  }

  layout_node <- function(v) {
    kids <- children[[v]]
    if (is.null(kids)) {
      x[[v]] <<- 0
      members[[v]] <<- v
      return(own_contour(v))
    }

    combined <- place_siblings(kids)
    x[[v]] <<- mean(x[kids[c(1, length(kids))]])
    members[[v]] <<- c(v, unlist(members[kids], use.names = FALSE))
    merge_contour(combined, own_contour(v))
  }

  # Segments: nodes with no or several layout parents, grouped by parent set
  roots <- Filter(function(nm) length(lay_par[[nm]]) != 1L, main_nd)
  keys  <- vapply(roots, function(nm) paste(lay_par[[nm]], collapse = "|"), "")
  for (key in unique(keys)) {
    grp <- roots[keys == key]
    place_siblings(grp)

    parents <- lay_par[[grp[1]]]
    target  <- if (length(parents) == 0L) 0 else mean(x[parents])
    shift   <- target - mean(x[grp[c(1, length(grp))]])
    seg     <- unlist(members[grp], use.names = FALSE)
    x[seg]  <- x[seg] + shift
  }

  # Side boxes sit beside their anchor
  xs <- x
  for (anchor in names(side_of)) {
    for (sb in side_of[[anchor]]) {
      off <- sb_offset + width[[sb]] / 2
      xs[[sb]] <- if (identical(consort_plot[[sb]]$side, "left")) {
        x[[anchor]] - off
      } else {
        x[[anchor]] + off
      }
    }
  }

  setNames(lapply(nodes_layout, function(nd) xs[nd]), NULL)
}

# Shift all X coordinates so minimum is 0 and compute final bounds
#' @keywords internal
normalize_x <- function(nd_x, nd_wd) {
  lo <- min(unlist(Map(function(x, w) x - w / 2, nd_x, nd_wd)))
  hi <- max(unlist(Map(function(x, w) x + w / 2, nd_x, nd_wd)))

  list(nd_x = lapply(nd_x, function(x) x - lo), max_width = hi - lo)
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

  # --- Phase 2: Gather node widths ---
  nd_wd <- lapply(nodes_layout, function(nd) {
    sapply(consort_plot[nd], function(x) get_size(x$box)$width)
  })

  # --- Phase 3: X coordinates ---
  nd_x <- calc_tree_x(consort_plot, nodes_layout, nd_type, nd_wd, pad_u)

  # --- Phase 4: Normalize to positive coordinates ---
  x_result <- normalize_x(nd_x, nd_wd)

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
