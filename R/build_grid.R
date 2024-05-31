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
#' \dontrun{
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
#' # g <- ggsave("consort_diagram.pdf", plot = build_grid(g))
#' }
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
  
  # Coordination
  nodes_coord <- calc_coords(consort_plot)
  
  # Generate connection
  nodes_connect <- get_connect(consort_plot)

  # Move all nodes to the left if there are labels nodes
  # based on the width of the label nodes
  vp_height <- nodes_coord$max_height
  vp_width <- nodes_coord$max_width
  nodes_coord$y <- (vp_height - nodes_coord$y)/vp_height

  if(any(grepl("label", names(x)))){
    label_coord <- calc_coords_label(label_plot, 
                                     nodes_coord$nd_y, 
                                     max_h = nodes_coord$max_height)
    vp_width <- sum(label_coord$width[1], vp_width)

    nodes_coord$x <- (nodes_coord$x + label_coord$width[1])/vp_width
    
  }else{
    nodes_coord$x <- (nodes_coord$x)/vp_width
  }
  
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
      if(is.null(nodes[[nd$node[j]]])){
        nd_name <- nodes_connect[[nd$node[j]]]$node[2]
      }else{
        nd_name <- nd$node[j]
      }
      connect_gb <- connect_box(nodes[[nd_name]], nodes[[nd$node[1]]], 
                                connect = nd$connect, type = "p")
      grobs_list <- gList(grobs_list, connect_gb)
    }
  }
  
  if(any(grepl("label", names(x)))){
    
    # Align labels
    for(i in seq_along(label_plot)){
      nam <- names(label_plot)[i]
      r <- move_box(label_plot[[nam]]$box,
                    x = unit(label_coord$x[nam], "char"),
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



