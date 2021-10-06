#' Add a vertically aligned label nodes on the left side.
#'
#' In a consort diagram, this can be used to indicate different stage.
#'
#' @param ref_box Reference node to which the label will be horizontally aligned.
#' @param txt Text in the node.
#' @param ... Other parameters pass to \link{textbox}, 
#' @export
#' 
#' @return A \code{consort} object.
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
#' lab1 <- add_label_box(node1, txt = "Screening")
#' lab2 <- add_label_box(node4, txt = "Randomized")
#' lab3 <- add_label_box(node2_sp, txt = "Final analysis")
#' build_consort(list(node1, node3, node4, node1_sp, side1_sp, node2_sp),
#'               list(lab1, lab2, lab3))
#'               
add_label_box <- function(ref_box,
                          txt,
                          ...){
  
  ot_input <- list(...)
  
  if(length(txt) > 1)
    stop("txt must of length one!")
  if(!inherits(ref_box, c("consort.list", "consort")))
    stop("ref_box must be consort.list or consort object")
  
  if(inherits(ref_box, "consort.list"))
    ref_box <- ref_box[[1]]
  
  ref_pos <- get_coords(ref_box)
  
  cex <- ifelse("cex" %in% names(getOption("txt_gp")), 
                getOption("txt_gp")$cex, 1)
  
  # Set default values
  args_list <- list()
  args_list$text <- txt
  args_list$txt_gp <- gpar(col = "#4F81BD", cex = cex, fontface = "bold")
  args_list$box_gp <- gpar(fill = "#A9C7FD")
  args_list$box_fn <- roundrectGrob
  
  if(is.null(names(ot_input))){
    out_box <- do.call(textbox, args_list)
    
  }else{
    # If some parameters are given
    args_list <- c(args_list[!names(args_list) %in% names(ot_input)],
                   ot_input)
    out_box <- do.call(textbox, args_list)
  }
  
  # Align with the reference box
  box_pos <- get_coords(out_box)
  out_box <- move_box(out_box,
                      x = unit(0.2, "npc") + box_pos$half_width,
                      y = ref_pos$top - box_pos$half_height)
  
  class(out_box) <- union("consort", class(out_box))
  
  return(out_box)
}