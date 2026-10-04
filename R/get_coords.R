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

  x_mid <- convertX(grobX(x, 90), "npc")
  y_mid <- convertY(grobY(x, 0), "npc")

  list(
    left = x_mid - half_width,
    right = x_mid + half_width,
    bottom = y_mid - half_height,
    top = y_mid + half_height,
    top.mid = unit.c(grobX(x, 90), grobY(x, 90)),
    left.mid = unit.c(grobX(x, 180), grobY(x, 180)),
    bottom.mid = unit.c(grobX(x, 90), grobY(x, 270)),
    right.mid = unit.c(grobX(x, 0), grobY(x, 0)),
    x = x_mid,
    y = y_mid,
    width = width,
    height = height,
    half_width = half_width,
    half_height = half_height
  )
}

# Width and height of a textbox in char units
# Lightweight alternative to `get_coords` for layout calculations: reads the
# cached text measurement without resolving grob x/y positions or building
# the box grob.
#' @keywords internal
get_size <- function(x) {
  # Custom box functions may have different natural dimensions; measure the
  # grob for real in that case
  if (!is_standard_box(x$box_fn)) {
    return(list(
      width = convertWidth(grobWidth(x), "char", valueOnly = TRUE),
      height = convertHeight(grobHeight(x), "char", valueOnly = TRUE)
    ))
  }
  hw <- get_hw(x)
  list(
    width = convertWidth(hw$width, "char", valueOnly = TRUE),
    height = convertHeight(hw$height, "char", valueOnly = TRUE)
  )
}
