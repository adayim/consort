#' Get the coordinates of the box object
#' 
#' This function will get the coordinates of the box object.
#'
#' @param x A box object
#'
#' @return A list of coordinates will return:
#' \item{left}{Left (x-min) side coordinate.}
#' \item{right}{Right (x-max) side coordinate.}
#' \item{bottom}{Bottom (y-min) side coordinate.}
#' \item{top}{Top (y-max) side coordinate.}
#' \item{x}{X (center x) coordinate.}
#' \item{y}{Y (center y) coordinate.}
#' \item{width}{Width of the box.}
#' \item{height}{Height of the box.}
#' \item{half_width}{Half width of the box.}
#' \item{half_height}{Half height of the box.}
#' 
#' @export
#'
#' @examples
#' fg <- textbox(text = "This is a test")
#' get_coords(fg)
#' 
get_coords <- function(x) {
  if(!inherits(x, "box"))
    stop("Object x must be box.")
  
  args <- attr(x, "args")
  args <- args[c("x", "y", "width", "height")]
  do.call(create_coords, args)
}


#' Convert position to coordinates
#' @keywords internal
create_coords <- function(x, y, width, height){
  
  width <- convertWidth(width, "mm", valueOnly = TRUE)
  height <- convertHeight(height, "mm", valueOnly = TRUE)
  
  half_height <- unit(height/2, "mm")
  half_width <- unit(width/2, "mm")
  
  list(left = x - half_width,
       right = x + half_width,
       bottom = y - half_height,
       top = y + half_height,
       x = x,
       y = y,
       width = width,
       height = height, 
       half_width = half_width,
       half_height = half_height)
}
