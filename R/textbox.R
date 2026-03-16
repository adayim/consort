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
#' text. This will also be read from global options of \code{"txt_gp"}. For
#' example, if one wants to set a font size for all the text inside box,
#' \code{options(txt_gp = gpar(cex = 0.8))} will do the trick.
#' @param box_fn Function to create box for the text. Parameters of `x=0.5`,
#' `y=0.5` and `box_gp` will be passed to this function and return a \code{grob}
#' object. This will also be read from global options of \code{"box_gp"}.
#' @param box_gp An object of class \link[grid]{gpar} style to be applied to the
#' box.
#' @param indent Extra left indentation for the text inside the box. Can be a
#' numeric value in millimetres or a \code{\link[grid]{unit}} object
#' (e.g., \code{unit(5, "mm")}, \code{unit(0.2, "in")}). Default is \code{0}
#' (no indentation). Can also be set globally with
#' \code{options(box_indent = 5)}.
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
#' # With left indentation (5mm)
#' fg2 <- textbox(text = "Indented text", just = "left", indent = 5)
#' grid::grid.draw(fg2)
textbox <- function(text,
                    x = unit(.5, "npc"),
                    y = unit(.5, "npc"),
                    just = c("center", "left", "right"),
                    txt_gp = getOption("txt_gp", default = gpar(
                      color = "black",
                      cex = 1
                    )),
                    box_fn = roundrectGrob,
                    box_gp = getOption("box_gp", default = gpar(fill = "white")),
                    indent = getOption("box_indent", default = 0),
                    name = "textbox") {
  just <- match.arg(just)

  if (!is.unit(x)) x <- unit(x, units = "npc")
  if (!is.unit(y)) y <- unit(y, units = "npc")

  # Convert indent to unit if numeric
  if (!is.unit(indent)) indent <- unit(indent, "mm")

  # class(fg) <- union("box", class(fg))
  name <- paste(name, auto_index(), sep = ".")

  gTree(
    label = text, x = x, y = y, just = just,
    txt_gp = txt_gp, box_fn = box_fn,
    box_gp = box_gp, indent = indent,
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

  # Add indent to width if present
  indent <- if (!is.null(x$indent)) x$indent else unit(0, "mm")
  indent_mm <- convertWidth(indent, "char", valueOnly = TRUE)
  if (indent_mm > 0) {
    width <- width + unit(indent_mm, "char")
  }

  list(width = width, height = height)
}

#' @export
makeContext.textbox <- function(x) {
  hw <- get_hw(x)

  tbvp <- viewport(x$x, x$y,
    width = hw$width,
    height = hw$height,
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

  # Get indent value
  indent <- if (!is.null(x$indent)) x$indent else unit(0, "mm")

  # Align text with indent applied to the left offset
  tx_x <- switch(x$just,
    "right" = unit(1, "npc") - 0.5 * padding,
    "left"  = 0.5 * padding + indent,
    "center" = unit(.5, "npc") + 0.5 * indent
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
