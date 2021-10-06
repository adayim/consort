#' Create a box with text
#' 
#' Create a \link[grid]{grob} with text inside. The exact positions of the grob
#' is stored at the \code{attr(..., "args")}.
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
#' @param box_fn An object of class \link[grid]{gpar} style to be applied to the
#' box. 
#' @param box_gp Function to create box for the text. Parameters of `x=0.5`, 
#' `y=0.5` and `box_gp` will be passed to this function and return a \code{grob}
#' object. This will also be read from global options of \code{"box_gp"}.
#' 
#' @param name A character identifier.
#'
#' @return A grob
#' 
#' @export
#' @import grid 
#' @rdname textbox
#'
#' @examples
#' fg <- textbox(text = "This is a test")
#' 
textbox <- function(text, 
                    x = unit(.5, "npc"),
                    y = unit(.5, "npc"),
                    just = c("center", "left", "right"),
                    txt_gp = getOption("txt_gp", default = gpar(color = "black",
                                                                    cex = 1)),
                    box_fn  = roundrectGrob,
                    box_gp = getOption("box_gp", default = gpar(fill = "white")),
                    name = NULL){
  
  just <- match.arg(just)
  
  if(!is.unit(x))
    x <- unit(x, units = "npc")
  
  if(!is.unit(y))
    y <- unit(y, units = "npc")
  
  tx_x <- switch(just,
                 "left" = unit(2, "mm"),
                 "right" = unit(1, "npc") - unit(2, "mm"),
                 "center" = unit(.5, "npc"))
  
  tx <- textGrob(label = text,
                 x = tx_x,
                 y = 0.5,
                 just = just,
                 gp = txt_gp,
                 name = "label")
  
  bx <- do.call(box_fn, list(x = .5,
                             y = .5, 
                             gp = box_gp,
                             name = "box"))
  
  # Calculate ratio to address any text size change
  bx_height <- convertHeight(grobHeight(tx), "npc", valueOnly = TRUE)
  str_height <- convertHeight(stringHeight(text),  "npc", valueOnly = TRUE)
  ratio <- bx_height/str_height
  
  width <- grobWidth(tx) + unit(ratio * 4, "mm")                
  height <- grobHeight(tx) + unit(ratio * 4, "mm")
  
  args <- list(x = x,
               y = y,
               width = width,
               height = height)
  
  fg <- frameGrob(name = name, vp = do.call(viewport, args))
  fg <- packGrob(fg, bx)
  fg <- packGrob(fg, tx)
  
  class(fg) <- union("box", class(fg))
  
  structure(fg, args = args)
  
}

#' The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
#' @param x The grob to print/plot
#' @param ... Passed to \code{\link[grid]{grid.draw}}
#' @rdname textbox
#' @export
print.box <- function(x, ...) {
  grid.draw(x, ...)
}

