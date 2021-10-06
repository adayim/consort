#' Align list of box grobs horizontally
#' 
#' This function will align all box with the lowest.
#'
#' @param boxlist A list of box grobs.
#'
#' @return A list of box grobs
#' 
#' @keywords internal
#'
#' @examples
#' 
align_hori <- function(boxlist) {

  stopifnot(is.list(boxlist))

  cls <- sapply(boxlist, function(x)!inherits(x, "box"))
  if(any(cls))
    stop("The elements in the list must be a box object.")
  
  # Find the lowest box, and set as reference
  y_val <- sapply(boxlist, function(x){
    if(length(x) == 0){
      return(NA)
    }else{
      convertUnit(get_coords(x)$y, unitTo = "npc", valueOnly = TRUE)
    }
  })
  
  y_val <- which.min(y_val)
  y_oth <- base::setdiff(seq_along(boxlist), y_val)
  
  ref_positions <- get_coords(boxlist[[y_val]])
  
  # Align other boxes
  boxlist[y_oth] <- lapply(boxlist[y_oth],
                           FUN = function(box, ref_pos) {
                             if(length(box) == 0){
                               return(structure(list(),
                                                type =  attr(box, "type")))
                             }else{
                               box_pos <- get_coords(box)
                               new_y <- ref_pos$top - box_pos$half_height
                               out_box <- move_box(box, y = new_y)
                               
                               structure(out_box,
                                         type =  attr(box, "type"))
                             }
                             
                           },
                           ref_pos = ref_positions)
  
  return(boxlist)
  
}
