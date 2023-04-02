#' Build consort diagram
#'
#' Build a \code{grob} consort diagram, use this if you want
#' to save plots with \code{\link[ggplot2]{ggsave}}
#'
#' @param x A conosrt object.
#' 
#' @return A \code{Graphviz} code
#'
#' @seealso \code{\link[DiagrammeR]{grViz}}
#' @export 
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
#' plot(g, grViz = TRUE)
#' 
build_grviz <- function(x) {

  if (!inherits(x, c("consort")))
    stop("x must be consort object")
  
  # Get the maximum of height and width of each node
  nodes_layout <- attr(x, "nodes.list")
  
  if(any(grepl("label", names(x)))){
    consort_plot <- x[grepl("node", names(x))]
    label_plot <- x[grepl("label", names(x))]
    
    lab_txt <- lapply(names(label_plot), function(x){
      txt_lab <- mk_text_align(label_plot[[x]]$text, label_plot[[x]]$just)
      c(nam = x,
        node = paste(x, txt_lab),
        pos = label_plot[[x]]$prev_node)
    })
    lab_txt <- do.call(rbind, lab_txt)
    
    lab_gpar <- label_plot[[1]]$gpar
    
    lab_nd <- sprintf("node [shape = rectangle, style = 'rounded,filled', fillcolor = '%s' color = '%s']\n%s\n",
                      lab_gpar$box_gp$fill, 
                      lab_gpar$txt_gp$col, 
                      paste(lab_txt[,"node"], collapse = "\n"))
    lab_edge <- sprintf("edge[style=invis];\n%s;\n", 
                        paste(lab_txt[,"nam"], collapse = " -> "))
    lab_rnk <- nodes_layout[as.numeric(lab_txt[,"pos"])]
    names(lab_rnk) <- lab_txt[,"nam"]
    
  }else{
    consort_plot <- x
    lab_nd <- NULL
    lab_edge <- NULL
    lab_rnk <- NULL
  }
  
  # Reset the indexing to 0
  set_invs(0)
  
  # Node type of each
  nd_type <- sapply(nodes_layout, function(x) 
    unique(sapply(consort_plot[x], "[[", "node_type"))
  )
  
  if(nd_type[length(nd_type)] == "sidebox")
    stop("last box cannot be a side box.")
  
  main_txt <- lapply(names(consort_plot), function(nd){
    nd_num <- gsub("\\D", "", nd)
    txt_lab <- mk_text_align(consort_plot[[nd]]$text, consort_plot[[nd]]$just)
    c(nam = nd,
      node = paste(nd, txt_lab),
      lab = txt_lab)
  })
  main_txt <- do.call(rbind, main_txt)
  # Remove empty label
  null_indx <- is_empty(main_txt[,"lab"])
  main_txt <- main_txt[!null_indx,]
  
  rnk_nd <- vector("character") # Ranking
  inv_nd <- vector("character") # Invisible node
  con_nd <- vector("character") # Connections
  
  # Main nodes
  for(i in seq_along(nd_type)){
    
    nd <- nodes_layout[[i]]
    
    if(i == length(nd_type))
      next
    
    if(length(nodes_layout[[i+1]]) != length(nd)){
      nd_lst <- mk_invs_node(nd, nodes_layout[[i+1]])
      rnk_nd <- c(rnk_nd, nd_lst[["rank"]])
      inv_nd <- c(inv_nd, nd_lst[["invs"]])
      con_nd <- c(con_nd, nd_lst[["connect"]])
      next
    }
    
    # The next one and current one is not a side box
    if(length(nodes_layout[[i+1]]) == length(nd) & all(!c(nd_type[i], nd_type[i+1]) %in% "sidebox")){
      
      for(j in seq_along(nd)){
        
        if(is_empty(consort_plot[[nodes_layout[[i+1]][j]]]$text)){
          # No arrow if next one has blank text
          con_nd <- c(con_nd, sprintf("%s -> %s [arrowhead = none];",
                                      nodes_layout[[i]][j],
                                      nodes_layout[[i+1]][j]))
        }else{
          con_nd <- c(con_nd, sprintf("%s -> %s;", 
                                      nodes_layout[[i]][j],
                                      nodes_layout[[i+1]][j]))
        }
      }
      rnk_nd <- c(rnk_nd, mk_subgraph_rank(nd))
      rnk_nd <- c(rnk_nd, mk_subgraph_rank(nodes_layout[[i+1]]))
      next
    }
    
    if(nd_type[i] == "sidebox"){
      sp_box <- nd
      if(length(nd) == 1){
        # Strip white space
        if(is_empty(consort_plot[[nd]]$text)){
          con_nd <- c(con_nd, sprintf("%s -> %s;", 
                                      nodes_layout[[i-1]], 
                                      nodes_layout[[i+1]]))
        }else{
          nd_lst <- mk_invs_node(nodes_layout[[i-1]], nd, nodes_layout[[i+1]])
          rnk_nd <- c(rnk_nd, nd_lst[["rank"]])
          inv_nd <- c(inv_nd, nd_lst[["invs"]])
          con_nd <- c(con_nd, nd_lst[["connect"]])
        }
      }else{
        for(j in seq_along(nd)){

          # Skip if text is blank
          if(is_empty(consort_plot[[nd[j]]]$text)){
            nd_next <- nodes_layout[[i+1]][j]
            # No arrow if next link has no text
            if(is_empty(consort_plot[[nd_next]]$text)){
              con_nd <- c(con_nd, sprintf("%s -> %s [arrowhead = none];", 
                                          nodes_layout[[i-1]][j], 
                                          nodes_layout[[i+1]][j]))
            }else{
              con_nd <- c(con_nd, sprintf("%s -> %s;", 
                                          nodes_layout[[i-1]][j],
                                          nodes_layout[[i+1]][j]))
            }
            sp_box <- sp_box[-j]
          }else{
            nd_lst <- mk_invs_node(nodes_layout[[i-1]][j], 
                                   nd[j], 
                                   nodes_layout[[i+1]][j])
            
            rnk_nd <- c(rnk_nd, nd_lst[["rank"]])
            inv_nd <- c(inv_nd, nd_lst[["invs"]])
            con_nd <- c(con_nd, nd_lst[["connect"]])
          }
          
          if(length(nodes_layout[[i+1]]) > 1)
            rnk_nd <- c(rnk_nd, mk_subgraph_rank(nodes_layout[[i+1]]))
          
          if(length(sp_box) > 1)
            rnk_nd <- c(rnk_nd, mk_subgraph_rank(sp_box))
        }
      }
      
      next  
    }
  }
  
  rnk_nd <- unique(rnk_nd)
  inv_nd <- unique(inv_nd)
  con_nd <- unique(con_nd)
  
  if(!is.null(lab_rnk)){
    for(i in names(lab_rnk)){
      indx <- grepl(paste(sprintf("\\<%s\\>", lab_rnk[[i]]), collapse = "|"),
                    rnk_nd)
      if(any(indx)){
        repl_str <- sprintf("\\1 %s; \\2", i)
        rnk_nd[indx] <- gsub(rnk_nd[indx], 
                             pattern = "(.{39})(.*)",
                             replacement = repl_str)
      }else{
        rnk_nd <- c(rnk_nd, mk_subgraph_rank(c(i, lab_rnk[[i]])))
      }
      
    }
    
  }

  grviz_txt <- paste("digraph consort_diagram {
  graph [layout = dot]",
  lab_nd,
  lab_edge,
  "# node definitions with substituted label text
  node [shape = rectangle, fillcolor = Biege, style='', fillcolor = '', color = '']",
  paste(main_txt[,"node"], collapse = "\n"), # Node 
  "\n## Invisible point node for joints",
  "node [shape = point, width = 0]",
  paste(inv_nd, collapse = " "),             # Invisible node 
  paste(rnk_nd, collapse = "\n"),            # Ranks
  "edge[style=''];",
  paste(con_nd, collapse = "\n"),            # Connections
  "\n}\n",
  # paste(main_txt[,"txt"], collapse = "\n"),  # Text 
  sep = "\n\n")
  
  return(grviz_txt)
  
}


