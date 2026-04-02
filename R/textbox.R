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

  hw <- get_hw(x)

  rr <- do.call(x$box_fn, list(
    x = .5,
    y = .5,
    width = hw$width,
    height = hw$height,
    gp = x$box_gp,
    name = paste0(x$name, ".box")
  ))

  setChildren(x, gList(rr, t))
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
