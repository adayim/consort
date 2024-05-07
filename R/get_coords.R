#' Get the coordinates of the textbox object
#'
#' This function will get the coordinates of the textbox object.
#'
#' @param x A textbox object
#'
#' @return A list of coordinates will return:
#' \item{left}{Left (x-min) side coordinate.}
#' \item{right}{Right (x-max) side coordinate.}
#' \item{bottom}{Bottom (y-min) side coordinate.}
#' \item{top}{Top (y-max) side coordinate.}
#' \item{top.mid }{Coordinate vector of top middle, measured by grob.}
#' \item{left.mid}{Coordinate vector of left middle, measured by grob.}
#' \item{bottom.mid}{Coordinate vector of bottom middle, measured by grob.}
#' \item{right.mid}{Coordinate vector of right middle, measured by grob.}
#' \item{x}{X (center x) coordinate.}
#' \item{y}{Y (center y) coordinate.}
#' \item{width}{Width of the textbox, derived with \code{grobWidth}.}
#' \item{height}{Height of the textbox, derived with \code{grobHeight}.}
#' \item{half_width}{Half width of the box.}
#' \item{half_height}{Half height of the box.}
#'
#' @export
#'
#' @examples
#' fg <- textbox(text = "This is a test")
#' get_coords(fg)
get_coords <- function(x) {
  # if (!inherits(x, "textbox")) {
  #   stop("Object x must be textbox.")
  # }

  width <- convertWidth(grobWidth(x), "char", valueOnly = TRUE)
  height <- convertHeight(grobHeight(x), "char", valueOnly = TRUE)

  half_height <- unit(0.5 * height, "char")
  half_width <- unit(0.5 * width, "char")

  list(
    left = convertX(grobX(x, 90), "npc") - half_width,
    right = convertX(grobX(x, 90), "npc") + half_width,
    bottom = convertY(grobY(x, 0), "npc") - half_height,
    top = convertY(grobY(x, 0), "npc") + half_height,
    top.mid = unit.c(grobX(x, 90), grobY(x, 90)),
    left.mid = unit.c(grobX(x, 180), grobY(x, 180)),
    bottom.mid = unit.c(grobX(x, 90), grobY(x, 270)),
    right.mid = unit.c(grobX(x, 0), grobY(x, 0)),
    x = convertX(grobX(x, 90), "npc"),
    y = convertY(grobY(x, 0), "npc"),
    width = width,
    height = height,
    half_width = half_width,
    half_height = half_height
  )
}
