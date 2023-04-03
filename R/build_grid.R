#' Build consort diagram
#'
#' Build a \code{grob} consort diagram, use this if you want
#' to save plots with \code{\link[ggplot2]{ggsave}}. \code{build_grid}
#' does not support multiple split for the moment, please use 
#'  \code{\link{build_grviz}} or \code{plot(g, grViz = TRUE)} for 
#' multiple split nodes instead.
#'
#' @param x A conosrt object.
#' 
#' @return A \code{gList} object
#' @export 
#'
#' @seealso \code{\link[grid]{gList}}
#' @examples
#' txt1 <- "Population (n=300)"
#' txt1_side <- "Excluded (n=15): \n
#'               \u2022 MRI not collected (n=3)\n
#'               \u2022 Tissues not collected (n=4)\n
#'               \u2022 Other (n=8)"
#'
#' g <- add_box(txt = txt1)
#'
#' g <- add_side_box(g, txt = txt1_side)
#'
#' g <- add_box(g, txt = "Randomized (n=200)")
#' # g <- ggplot2::ggsave("consort_diagram.pdf", plot = build_grid(g))
#' 
build_grid <- function(x) {

  if (!inherits(x, c("consort")))
    stop("x must be consort object")
  
  if(any(grepl("label", names(x)))){
    consort_plot <- x[grepl("node", names(x))]
    attr(consort_plot, "nodes.list") <- attr(x, "nodes.list")
    label_plot <- x[grepl("label", names(x))]
  }else{
    consort_plot <- x
  }
  
  node_tp <- which(sapply(consort_plot, "[[", "node_type")  == "splitbox")
  if(!all(abs(diff(node_tp)) == 1))
    stop("Multiple splitts are not supported, use `grViz` method instead. See `build_grviz` for details.")
  
  # Coordination
  nodes_coord <- calc_coords(consort_plot)
  
  # Generate connection
  nodes_connect <- get_connect(consort_plot)
  
  # Change nodes coordinates
  nodes <- sapply(names(consort_plot), function(i){
    r <- move_box(consort_plot[[i]]$box,
                  x = unit(nodes_coord$x[i], "npc"),
                  y = unit(nodes_coord$y[i], "npc"))
    r$name <- i
    
    # Skep empty side box
    if(is_empty(consort_plot[[i]]$text))
      return(NULL)
      
    return(r)
  }, simplify = FALSE)
  
  # Remove empty node from connection
  # node_empty <- sapply(names(consort_plot), function(i)is_empty(consort_plot[[i]]$text))
  # node_empty <- names(node_empty)[node_empty]
  # node_sd <- which(sapply(consort_plot, "[[", "node_type")  == "sidebox")
  # if(length(node_empty) > 0){
  #   node_empty <- node_empty[!node_empty %in% names(node_sd)]
  #   for(i in node_empty){
  #     node_empty_prev <- nodes_connect[[i]]$node[2]
  #     node_empty_con <- sapply(nodes_connect, function(x)x$node[2] == i)
  #     node_empty_con <- names(node_empty_con)[node_empty_con]
  #     
  #     # If the next node is the last and empty
  #     if(length(node_empty_con) == 0){
  #       nodes_connect[node_empty_con] <- NULL
  #     }else{
  #       nodes_connect[[node_empty_con]]$node[2] <- node_empty_prev
  #     }
  #     
  #   }
  # }
  
  for (i in seq_along(nodes)) {
    if(is.null(nodes[[i]]))
      next
    
    if (i == 1) {
      grobs_list <- gList(gList(), nodes[[i]])
    } else {
      grobs_list <- gList(grobs_list, nodes[[i]])
    }
  }
  
  # Connections
  for(i in seq_along(nodes_connect)){
    nd <- nodes_connect[[i]]
    
    if(is.null(nodes[[nd$node[1]]]))
      next
    
    for(j in 2:length(nd$node)){
      connect_gb <- connect_box(nodes[[nd$node[j]]], nodes[[nd$node[1]]], 
                                connect = nd$connect, type = "p")
      grobs_list <- gList(grobs_list, connect_gb)
    }
  }
  
  if(any(grepl("label", names(x)))){
    label_coord <- calc_coords_label(label_plot, nodes_coord$nodes_hw)
    # width_vp <- label_coord$lab_nd_width/sum(label_coord$lab_nd_width)
    
    # Align labels
    for(i in seq_along(label_plot)){
      nam <- names(label_plot)[i]
      r <- move_box(label_plot[[nam]]$box,
                    x = unit(label_coord$x[nam], "npc"),
                    y = unit(label_coord$y[nam], "npc"))
      r$name <- nam
      
      if (i == 1) {
        lab_grobs <- gList(gList(), r)
      } else {
        lab_grobs <- gList(lab_grobs, r)
      }
    }
    
    grobs_list <- gList(grobs_list, lab_grobs)
    
  }
  
  grobTree(grobs_list, 
           name = "consort",
           vp = viewport(width = unit(0.98, "npc"),
                         height = unit(0.98, "npc")))
  
  # return(grobs_list)
  
}



