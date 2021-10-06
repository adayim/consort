

#' Add a splitting box
#'
#' This function will create a horizontally aligned nodes. The horizontal coordinate
#' will be automatically calculated if the coordinates not provided.
#'
#' @param prev_box Previous node that the newly created split box will be aligned.
#' @param txt A vector of text labels for each nodes.
#' @param coords The horizontal coordinates of the boxes, see details.
#' @inheritParams add_box
#' 
#'
#' @details
#' The `coords` will be used to set the horizontal coordinates of the nodes. The
#' `coords` should be within 0 and 1 to avoid the nodes is aligned outside of the
#'  final figure. If the `coords` is `NULL`, not given. The function will calculate
#'  the `coords`. If the the length of the `txt` is two, then a coordinates of
#'  0.35 and 0.65 will be used. Once the split box is added, all the following nodes
#'  will be split accordingly.
#'
#' @seealso \code{\link{add_box}}, \code{\link{add_side_box}}
#' 
#' @return A \code{consort.list} object.
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
#'                              "Final analysis (n=100"))#' 
#' node1
#' node3
#' node4
#' node1_sp
#' side1_sp
#' node2_sp
#' 
#'
add_split <- function(prev_box, txt, coords = NULL, dist = 0.02){
  
  if(inherits(prev_box, "consort.list"))
    stop("Nested splits are not supported in the current version")
  
  if(inherits(prev_box, "consort.list") & length(prev_box) != 1)
    stop("The package does not support multiple split.")
  
  if(length(txt) == 1)
    stop("The length of txt should be larger than 1, please use add_box instead.")
  
  # Define coordinates for the splits
  if(is.null(coords)){
    n_gp <- length(txt)
    if(n_gp == 2){
      x_coords <- c(0.35, 0.65)
    }else{
      x_coords <- 0.5/n_gp
      x_coords <- c(x_coords, rep(2*x_coords, times = n_gp - 1))
      x_coords <- cumsum(x_coords)
    }
  }else{
    if(length(coords) != length(txt))
      stop("The length of the coords should match the length of txt.")
    
    if(any(coords > 1 | coords < 0))
      warning("The coordinates are not within 0 and 1 and might be cropped in the final output.")
    
    x_coords <- coords
  }
  
  .add_split <- function(prev_box, txt, x, dist){
    pre_cords <- get_coords(prev_box)
    
    out_box <- textbox(txt, x = x, box_fn = rectGrob)
    y_cords <- pre_cords$bottom - get_coords(out_box)$half_height - unit(dist+0.02, "npc")
    out_box <- move_box(out_box, y = y_cords)
    connect <- connect_box(prev_box, out_box, connect = "bt", type = "p")
    
    class(out_box) <- union("consort", class(out_box))
    
    structure(out_box,
              connect =  connect,
              type = "box")
  }
  
  out_box <- lapply(seq_along(txt), function(i).add_split(prev_box,
                                                          txt[i],
                                                          x_coords[i],
                                                          dist))
  class(out_box) <- union("consort.list", class(out_box))
  return(out_box)
}
