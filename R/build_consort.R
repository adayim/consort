#' Build consort diagram
#'
#' @param consort_list A list of nodes.
#' @param label_list A list of label nodes.
#'
#' @export
#' 
#' @return A \code{consort.plot} object.
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}}
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
#' lab1 <- add_label_box(node1, txt = "Screening")
#' lab2 <- add_label_box(node4, txt = "Randomized")
#' lab3 <- add_label_box(node2_sp, txt = "Final analysis")
#' build_consort(list(node1, node3, node4, node1_sp, side1_sp, node2_sp),
#'               list(lab1, lab2, lab3))
#'
#'
build_consort <- function(consort_list, label_list = NULL){
  
  stopifnot(is.list(consort_list))
  
  # Remove any blank box
  consort_list <- consort_list[lengths(consort_list) != 0]
  
  gl <- list(consort_list = consort_list,
             label_list   = label_list)
  class(gl) <- union("consort.plot", class(gl))
  
  return(gl)
  
}
