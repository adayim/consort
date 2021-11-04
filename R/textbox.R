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
#' 

textbox <- function(text, 
                    x = unit(.5, "npc"),
                    y = unit(.5, "npc"), 
                    just = c("center", "left", "right"),
                    txt_gp = getOption("txt_gp", default = gpar(color = "black",
                                                                cex = 1)),
                    box_fn  = roundrectGrob,
                    box_gp = getOption("box_gp", default = gpar(fill = "white")),
                    name = "textbox") {
  
  just <- match.arg(just)
  
  if (!is.unit(x)) x <- unit(x, units = "npc")
  if (!is.unit(y)) y <- unit(y, units = "npc")
  
  # class(fg) <- union("box", class(fg))
  name <- paste(name, auto_index(), sep = ".")
  
  gTree(label=text, x = x, y = y, just = just, 
        txt_gp = txt_gp, box_fn = box_fn, 
        box_gp = box_gp,
        name=name, 
        cl="textbox")
}


#' @param ... Parameters passed to \code{textbox}
#' @rdname textbox
#' @examples
#' grid.textbox(text = "This is a test")
#' @export
grid.textbox <- function(...) {
  grid::grid.draw(textbox(...))
}

get_hw <- function(x){
  t <- textGrob(label = x$label, gp = x$txt_gp)
  # Add padding
  padding <- unit(4 * ifelse(is.null(x$txt_gp$cex), 1,
                             x$txt_gp$cex), "mm")
  height <- grobHeight(t) + padding
  width <- grobWidth(t) + padding
  
  list(width = width, height = height)
}

#' @export
makeContext.textbox <- function(x) {
  
  hw <- get_hw(x)
  
  tbvp <- viewport(x$x, x$y,
                   width = hw$width,
                   height = hw$height,
                   name = paste0(x$name, ".vp"))
  
  if (is.null(x$vp))
    x$vp <- tbvp
  else
    x$vp <- vpStack(x$vp, tbvp)
  x
}

#' @export
makeContent.textbox <- function(x) {
  
  # Add padding
  padding <- unit(3 * ifelse(is.null(x$txt_gp$cex), 1, x$txt_gp$cex), "mm")
  
  # Align text
  tx_x <- switch(x$just,
                 "right" = unit(1, "npc") - padding/2,
                 "left"  = padding/2,
                 "center" = unit(.5, "npc"))
  
  t <- textGrob(label = x$label,
                x = tx_x,
                y = 0.5,
                just = x$just,
                gp = x$txt_gp,
                name = paste0(x$name, ".text"))
  
  hw <- get_hw(x)
  
  rr <- do.call(x$box_fn, list(x = .5,
                               y = .5, 
                               width = hw$width,
                               height = hw$height,
                               gp = x$box_gp,
                               name = paste0(x$name, ".box")))
  
  setChildren(x, gList(rr, t))
}

#' @export
xDetails.textbox <- function(x, theta) {
  hw <- get_hw(x)
  rr <- do.call(x$box_fn, list(x = .5,
                               y = .5, 
                               width = hw$width,
                               height = hw$height))
  grobX(rr, theta)
  
}

#' @export
yDetails.textbox <- function(x, theta) {
  hw <- get_hw(x)
  rr <- do.call(x$box_fn, list(x = .5,
                               y = .5, 
                               width = hw$width,
                               height = hw$height))
  
  grobY(rr, theta)
}

#' @export
widthDetails.textbox <- function(x){
  get_hw(x)$width
}

#' @export
heightDetails.textbox <- function(x){
  get_hw(x)$height
}





