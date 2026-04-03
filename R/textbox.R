#' Create a box with text
#'
#' Create a \link[grid]{grob} with text inside. To extract the units describing
#' grob boundary location can be accessed with \link[grid]{grobX} and
#'  \link[grid]{grobY}. The units describing width and height can be accessed with
#'  \link[grid]{grobWidth} and \link[grid]{grobHeight}.
#'
#' @param text A character text to be passed to \link[grid]{textGrob}.
#' @param x A number or unit object specifying x-location.
#' @param y A number or unit object specifying y-location.
#' @param just The justification of the text, `"left"`, `"right` and `"center"`.
#' See \link[grid]{textGrob} for more details.
#' @param txt_gp An object of class \link[grid]{gpar} style to be applied to the
#' text. Defaults are read from \code{\link{set_consort_defaults}}. For
#' example, to set a font size for all text inside boxes, use
#' \code{set_consort_defaults(txt_gp = gpar(cex = 0.8))}.
#' @param leftrotate If the text box will be rotated 90 degrees counter-clockwise.
#' Default is \code{FALSE}.
#' @param box_fn Function to create box for the text. Parameters of `x=0.5`,
#' `y=0.5` and `box_gp` will be passed to this function and return a \code{grob}
#' object.
#' @param box_gp An object of class \link[grid]{gpar} style to be applied to the
#' box.
#' @param name A character identifier.
#'
#' @return A text box grob. grid.textbox() returns the value invisibly.
#'
#' @export
#' @import grid
#' @rdname textbox
#'
#' @examples
#' fg <- textbox(text = "This is a test")
#' grid::grid.draw(fg)
#' grid.textbox(text = "This is a test")
textbox <- function(text,
                    x = unit(.5, "npc"),
                    y = unit(.5, "npc"),
                    just = c("center", "left", "right"),
                    txt_gp = consort_opt("txt_gp"),
                    leftrotate = FALSE,
                    box_fn = roundrectGrob,
                    box_gp = consort_opt("box_gp"),
                    name = "textbox") {
  just <- match.arg(just)

  if (!is.logical(leftrotate) || length(leftrotate) != 1 || is.na(leftrotate)) {
    stop("`leftrotate` must be a single TRUE/FALSE value.")
  }

  if (!is.unit(x)) x <- unit(x, units = "npc")
  if (!is.unit(y)) y <- unit(y, units = "npc")

  angle <- if (isTRUE(leftrotate)) 90 else 0

  # class(fg) <- union("box", class(fg))
  name <- paste(name, auto_index(), sep = ".")

  gTree(
    label = text, x = x, y = y, just = just,
    txt_gp = txt_gp, box_fn = box_fn,
    box_gp = box_gp, angle = angle,
    name = name,
    cl = "textbox"
  )
}


#' @param ... Parameters passed to \code{textbox}
#' @rdname textbox
#' @examples
#' grid.textbox(text = "This is a test")
#' @export
grid.textbox <- function(...) {
  grid::grid.draw(textbox(...))
}

get_hw <- function(x) {
  if (has_markup(x$label)) {
    return(get_hw_formatted(x))
  }

  t <- textGrob(label = x$label, gp = x$txt_gp)
  # Add padding
  padding <- unit(1 * ifelse(is.null(x$txt_gp$cex), 1,
    x$txt_gp$cex
  ), "char")
  # height <- grobHeight(t) + padding
  height <- convertHeight(grobHeight(t), "char") + padding
  # width <- grobWidth(t) + padding
  width <- convertWidth(grobWidth(t), "char") + padding

  list(width = width, height = height)
}

# Measure width/height for formatted (markup) text
get_hw_formatted <- function(x) {
  segments <- parse_markup(x$label)
  lines    <- split_segments_by_newline(segments)

  cex     <- ifelse(is.null(x$txt_gp$cex), 1, x$txt_gp$cex)
  padding <- unit(1 * cex, "char")

  # Width of each line in inches
  line_widths <- sapply(lines, function(line_segs) {
    sum(sapply(line_segs, function(seg) {
      gp <- segment_gpar(seg$style, x$txt_gp)
      t  <- textGrob(label = seg$text, gp = gp)
      convertWidth(grobWidth(t), "inches", valueOnly = TRUE)
    }))
  })

  width <- convertWidth(unit(max(line_widths), "inches"), "char") + padding

  # Height from a dummy multi-line textGrob (correct line spacing)
  n_lines    <- length(lines)
  dummy_text <- paste(rep("Xg", n_lines), collapse = "\n")
  dummy      <- textGrob(label = dummy_text, gp = x$txt_gp)
  height     <- convertHeight(grobHeight(dummy), "char") + padding

  # Extra room for super/subscript offsets
  has_ss <- any(sapply(segments, function(s)
    s$style %in% c("superscript", "subscript")))
  if (has_ss) {
    height <- height + unit(0.3 * cex, "char")
  }

  list(width = width, height = height)
}

#' @export
makeContext.textbox <- function(x) {
  hw <- get_hw(x)

  tbvp <- viewport(x$x, x$y,
    width = hw$width,
    height = hw$height,
    angle = x$angle,
    name = paste0(x$name, ".vp")
  )

  if (is.null(x$vp)) {
    x$vp <- tbvp
  } else {
    x$vp <- vpStack(x$vp, tbvp)
  }
  x
}

#' @export
makeContent.textbox <- function(x) {

  # Add padding
  padding <- unit(3 * ifelse(is.null(x$txt_gp$cex), 1, x$txt_gp$cex), "mm")

  if (has_markup(x$label)) {
    text_grobs <- make_formatted_content(x)
  } else {
    # Align text
    tx_x <- switch(x$just,
      "right" = unit(1, "npc") - 0.5 * padding,
      "left"  = 0.5 * padding,
      "center" = unit(.5, "npc")
    )

    t <- textGrob(
      label = x$label,
      x = tx_x,
      y = 0.5,
      just = x$just,
      gp = x$txt_gp,
      name = paste0(x$name, ".text")
    )
    text_grobs <- gList(t)
  }

  hw <- get_hw(x)

  rr <- do.call(x$box_fn, list(
    x = .5,
    y = .5,
    width = hw$width,
    height = hw$height,
    gp = x$box_gp,
    name = paste0(x$name, ".box")
  ))

  setChildren(x, do.call(gList, c(list(rr), as.list(text_grobs))))
}

# Render formatted (markup) text as positioned grid grobs
make_formatted_content <- function(x) {
  segments <- parse_markup(x$label)
  lines    <- split_segments_by_newline(segments)
  n_lines  <- length(lines)

  cex     <- ifelse(is.null(x$txt_gp$cex), 1, x$txt_gp$cex)
  padding <- unit(3 * cex, "mm")

  grob_list <- list()

  for (i in seq_along(lines)) {
    line_segs <- lines[[i]]

    # Vertical position: evenly distribute lines top-to-bottom
    y_frac <- 1 - (2 * i - 1) / (2 * n_lines)
    y_base <- unit(y_frac, "npc")

    # Measure each segment width (in inches, resolved now)
    seg_info <- lapply(seq_along(line_segs), function(j) {
      seg <- line_segs[[j]]
      gp  <- segment_gpar(seg$style, x$txt_gp)
      tg  <- textGrob(seg$text, gp = gp, just = "left")
      w   <- convertWidth(grobWidth(tg), "inches", valueOnly = TRUE)
      list(grob = tg, style = seg$style, width_in = w)
    })

    total_width_in <- sum(sapply(seg_info, "[[", "width_in"))

    # Horizontal start position based on justification
    start_x <- switch(x$just,
      "center" = unit(0.5, "npc") - unit(total_width_in / 2, "inches"),
      "left"   = padding * 0.5,
      "right"  = unit(1, "npc") - padding * 0.5 - unit(total_width_in, "inches"),
      unit(0.5, "npc") - unit(total_width_in / 2, "inches")
    )

    cum_x_in <- 0

    for (j in seq_along(seg_info)) {
      si <- seg_info[[j]]

      # Vertical offset for super/subscript
      y_off <- switch(si$style,
        "superscript" = unit(0.4 * cex, "char"),
        "subscript"   = unit(-0.3 * cex, "char"),
        unit(0, "mm")
      )

      si$grob$x    <- start_x + unit(cum_x_in, "inches")
      si$grob$y    <- y_base + y_off
      si$grob$name <- sprintf("%s.text.%d.%d", x$name, i, j)

      grob_list[[length(grob_list) + 1L]] <- si$grob

      # Draw underline beneath the segment
      if (si$style == "underline") {
        seg_w <- unit(si$width_in, "inches")
        ul_y  <- y_base - unit(0.55 * cex, "char")
        ul_x0 <- start_x + unit(cum_x_in, "inches")
        ul <- segmentsGrob(
          x0 = ul_x0, x1 = ul_x0 + seg_w,
          y0 = ul_y,  y1 = ul_y,
          gp = gpar(
            col = if (!is.null(x$txt_gp$col)) x$txt_gp$col else "black",
            lwd = max(0.5, 0.8 * cex)
          ),
          name = sprintf("%s.ul.%d.%d", x$name, i, j)
        )
        grob_list[[length(grob_list) + 1L]] <- ul
      }

      cum_x_in <- cum_x_in + si$width_in
    }
  }

  do.call(gList, grob_list)
}

#' @export
xDetails.textbox <- function(x, theta) {
  hw <- get_hw(x)
  rr <- do.call(x$box_fn, list(
    x = .5,
    y = .5,
    width = hw$width,
    height = hw$height
  ))
  grobX(rr, theta)
}

#' @export
yDetails.textbox <- function(x, theta) {
  hw <- get_hw(x)
  rr <- do.call(x$box_fn, list(
    x = .5,
    y = .5,
    width = hw$width,
    height = hw$height
  ))

  grobY(rr, theta)
}

#' @export
widthDetails.textbox <- function(x) {
  hw <- get_hw(x)
  rr <- do.call(x$box_fn, list(
    x = .5,
    y = .5,
    width = hw$width,
    height = hw$height
  ))
  grobWidth(rr)
}

#' @export
heightDetails.textbox <- function(x) {
  hw <- get_hw(x)
  rr <- do.call(x$box_fn, list(
    x = .5,
    y = .5,
    width = hw$width,
    height = hw$height
  ))
  grobHeight(rr)
}
