#' Add a side node
#'
#' Add an exclusion node on the right side. If the length of text label is two, then
#' the first one will be aligned on the left and the second on the right. Otherwise,
#' all the side nodes will be aligned on the right.
#'
#' @param prev_box Previous node object, the created new node will be aligned
#' at the right bottom of the `prev_box`.
#' @param side Position of the side box, `left` or `right` side of the terminal box.
#' Will be aligned on the left and right side if only two groups, right otherwise. 
#' @inheritParams add_box
#'
#' @seealso \code{\link{add_box}},\code{\link{add_split}}
#' 
#' @return A \code{consort.list} or \code{consort} object.
#'
#' @export
#'
#' @examples
#' txt1 <- "Population (n=300)"
#' txt1_side <- "Excluded (n=15): \n
#'               \u2022 MRI not collected (n=3)\n
#'               \u2022 Tissues not collected (n=4)\n
#'               \u2022 Other (n=8)"
#' 
#' node1 <- add_box(txt = txt1)
#' 
#' node3 <- add_side_box(node1, txt = txt1_side)    
#' 
#' node4 <- add_box(node3, txt = "Randomized (n=200)")
#' 
#' node1_sp <- add_split(node4, txt = c("Arm A (n=100)", "Arm B (n=100"))
#' side1_sp <- add_side_box(node1_sp, 
#'                          txt = c("Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)\n
#'                          \u2022 Other (n=8)", 
#'                          "Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)"))
#' 
#' node2_sp <- add_box(side1_sp, 
#'                     txt = c("Final analysis (n=100)",
#'                              "Final analysis (n=100")) 
#' node1
#' node3
#' node4
#' node1_sp
#' side1_sp
#' node2_sp
#' 

add_side_box <- function(prev_box, txt, side = NULL, dist = 0.02){
  
  if(!inherits(prev_box, c("consort.list", "consort")))
    stop("ref_box must be consort.list or consort object")
  
  if(inherits(prev_box, "consort.list") & length(txt) != length(prev_box))
    stop("The previous node must be a split box if multiple txt defined")
  
  if(!is.null(side) & length(side) != length(txt))
    stop("The length of side must have the same length with txt.")
  
  # One box on left, the other is right if only two groups given,
  # all will be on right side if not.
  if(is.null(side)){
    if(length(txt) == 2)
      side <- c("left", "right")
    else
      side <- rep("right", length(txt))
  }
  
  if(length(txt) > 1){
    
    # If more than one groups is given
    out_box <- lapply(seq_along(txt), function(i).add_side(prev_box[[i]],
                                                           txt = txt[i],
                                                           dist = dist,
                                                           side = side[i]))
    out_box <- align_hori(out_box) # Horizontal align
    
    # Re-connect
    for(i in seq_along(txt)){
      if(length(out_box[[i]]) == 0){
        connect <- NULL
      }else{
        connect_pos <- switch(side[i],
                              "right" = "bl",
                              "left"  = "br")

        connect <- connect_box(prev_box[[i]], out_box[[i]], connect = connect_pos, type = "p")
      }
      
      attr(out_box[[i]], "connect") <- connect
      attr(out_box[[i]], "prev_box") <- prev_box[[i]]
    }
    
    class(out_box) <- union("consort.list", class(out_box))
    
  }else{
    out_box <- .add_side(prev_box, txt = txt, dist = dist, side = side)
    class(out_box) <- union("consort", class(out_box))
    
  }
  
  return(out_box)
}


#' Create box grob at the right/left bottom of the previous node
#'
#' @param side Position of the side box.
#' @inheritParams add_box
#' @keywords internal

.add_side <- function(prev_box, txt, dist = 0.02, side = c("right", "left")){
  
  side <- match.arg(side)
  
  # Incase the txt is a list
  txt <- unlist(txt)
  
  if(is.null(txt))
    return(structure(list(),
                     connect  =  NULL,
                     prev_box =  prev_box,
                     type     = "side_box"))
  
  pre_cords <- get_coords(prev_box)
  
  # Define the name of the side box
  bx_name <- switch(side,
                    "right" = "right_side_box",
                    "left"  = "left_side_box")
  
  box <- textbox(txt, just = "left", name = bx_name, box_fn = rectGrob)
  y_cords <- pre_cords$bottom - get_coords(box)$half_height - unit(dist, "npc")
  
  if(side == "right")
    x <- pre_cords$x + get_coords(box)$half_width + unit(6, "mm")
  else
    x <- pre_cords$x - get_coords(box)$half_width - unit(6, "mm")
  
  out_box <- move_box(box, x = x, y = y_cords)

  connect_pos <- switch(side,
                        "right" = "bl",
                        "left"  = "br")
  
  connect <- connect_box(prev_box, out_box, connect = connect_pos, type = "p")
  
  class(out_box) <- union("consort", class(out_box))
  
  structure(out_box,
            connect  =  connect,
            prev_box =  prev_box,
            type     = "side_box")
}
