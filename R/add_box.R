#' Add nodes
#'
#' Create/add vertically aligned labeled nodes or side nodes.
#' 
#'
#' @param prev_box Previous node object, the created new node will be vertically
#' aligned with this node. Left this as `NULL` if this is the first node. The first
#' node will be aligned in the top center.
#' @param txt Text in the node. If the `prev_box` is a horizontally aligned multiple
#' nodes, a vector of with the same length must be provided.
#' @param just The justification for the text: left, center or right.
#' @param dist Distance between previous node, including the distance between the
#' side node.
#' 
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}}
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
#' 
#' node1
#' node3
#' node4
#' node1_sp
#' side1_sp
#' node2_sp
#'
add_box <- function(prev_box = NULL, txt, just = "center", dist = 0.02){
  
  if(!is.null(prev_box)){
    
    if(!inherits(prev_box, c("consort.list", "consort")))
      stop("ref_box must be consort.list or consort object")
    
    if(inherits(prev_box, "consort.list") & length(txt) != length(prev_box))
      stop("The previous node must be a split box if multiple txt defined")
    
    # No allocation split
    if(length(txt) == 1){
      out_box <- .add_box(prev_box, txt = txt, just = just, dist = dist)
      class(out_box) <- union("consort", class(out_box))
      
      # If allocation split
    }else{
      
      out_box <- lapply(seq_along(txt), function(i).add_box(prev_box[[i]],
                                                            txt = txt[i],
                                                            just = just,
                                                            dist = dist))
      out_box <- align_hori(out_box) # Horizontal align
      # Re-connect
      for(i in seq_along(txt)){
        if(attr(prev_box[[i]], "type") == "side_box"){
          vert_box <- attr(prev_box[[i]], "prev_box")
        }else{
          vert_box <- prev_box[[i]]
        }
        
        connect <- connect_box(vert_box, out_box[[i]], connect = "bt")
        attr(out_box[[i]], "connect") <- connect
        attr(out_box[[i]], "prev_box") <- prev_box[[i]]
        
      }
      
      class(out_box)<- union("consort.list", class(out_box))
      
    }
    
    return(out_box)
    
  }else{
    out_box <- textbox(txt, x = 0.5, y = 0.9, box_fn = rectGrob)
    class(out_box) <- union("consort", class(out_box))
    structure(out_box,
              connect =  NULL,
              type = "box")
  }
  
}

#' Create node vertically align with the previous one
#'
#' @inheritParams add_box
#' @keywords internal
.add_box <- function(prev_box, txt, just = "center", dist = 0.02){
  
  # Incase the txt is a list
  txt <- unlist(txt)
  
  # If the current box is blank
  if(length(prev_box) == 0)
    prev_box <- attr(prev_box, "prev_box")
  
  # If previous box is not a side box
  if(attr(prev_box, "type") == "side_box"){
    vert_box <- attr(prev_box, "prev_box")
  }else{
    vert_box <- prev_box
    dist <- 2*dist # Add more distance
  }
  
  
  pre_cords <- get_coords(prev_box)
  
  box <- textbox(txt, just = just, box_fn = rectGrob)
  y_cords <- pre_cords$bottom - get_coords(box)$half_height - unit(dist, "npc")
  x <- get_coords(vert_box)$x
  
  out_box <- move_box(box, x = x, y = y_cords)
  connect <- connect_box(vert_box, out_box, connect = "bt")
  
  class(out_box) <- union("consort", class(out_box))
  
  structure(out_box,
            connect =  connect,
            prev_box = prev_box,
            type = "box")
}
