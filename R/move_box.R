
#' Move a box grob
#'
#' This function can be used to move the box to a given position with
#' \link[grid]{editGrob} changing the \code{x} and \code{y} value.
#'
#' @param obj A \code{box} object.
#' @param x A unit element or a number that can be converted to \code{npc}, see
#' \link[grid]{unit}.
#' @param y A unit element or a number that can be converted to \code{npc}, see
#' \link[grid]{unit}.
#' @param pos_type If the provided coordinates are \code{absolute} position the
#' box will be moved to or it's a \code{relative} position to it's current.
#'
#' @return A box object with updated x and y coordinates.
#'
#' @export
#'
#' @examples
#' fg <- textbox(text = "This is a test")
#' fg2 <- move_box(fg, 0.3, 0.3)
move_box <- function(obj, x = NULL, y = NULL, pos_type = c("absolute", "relative")) {
  pos_type <- match.arg(pos_type)

  if (!inherits(obj, "textbox")) {
    stop("Object obj must be textbox.")
  }

  args <- list(x = obj$x, y = obj$y, width = obj$width, height = obj$width)

  if (!is.null(x)) {
    if (!is.unit(x)) {
      x <- unit(x, units = "npc")
    }

    if (pos_type == "relative") {
      args$x <- args$x + x
    } else {
      args$x <- x
    }
  }

  if (!is.null(y)) {
    if (!is.unit(y)) {
      y <- unit(y, units = "npc")
    }

    if (pos_type == "relative") {
      args$y <- args$y + y
    } else {
      args$y <- y
    }
  }

  fg <- editGrob(obj, x = args$x, y = args$y)

  return(fg)
}
