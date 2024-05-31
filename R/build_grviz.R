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
#' # plot(g, grViz = TRUE)
#' }
build_grviz <- function(x) {

  if (!inherits(x, c("consort")))
    stop("x must be consort object")
  
  # Get the maximum of height and width of each node
  nodes_layout <- attr(x, "nodes.list")

  # Generate connection
  nodes_connect <- get_connect(x)
  nodes_connect <- sapply(nodes_connect, function(x)x$node[2])
  
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
    
    lab_nd <- sprintf("node [shape = rectangle, style = \"rounded,filled\", fillcolor = \"%s\" color = \"%s\"]\n%s\n",
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
    text <- consort_plot[[nd]]$text
    just <- consort_plot[[nd]]$just
    txt_lab <- mk_text_align(text, just)
    c(nam = nd, text = text, just = just, node = paste(nd, txt_lab))
  })
  main_txt <- do.call(rbind, main_txt)
  # Remove empty label
  null_indx <- is_empty(main_txt[,"text"]) & !main_txt[,"nam"] %in% unlist(nodes_layout[nd_type == "vertbox"])
  main_txt <- main_txt[!null_indx,]
  
  rnk_nd <- vector("character") # Ranking
  inv_nd <- vector("character") # Invisible node
  con_nd <- vector("character") # Connections
  
  # For multiple split
  if(sum(nd_type == "splitbox") > 2)
    stop("More than two splits are not supported.")
  
  # Group name
  # Number of nodes at each level
  nd_len <- unique(sapply(nodes_layout, length))
  names(nd_len) <- LETTERS[1:length(nd_len)]
  group_name <- lapply(nodes_layout, function(x){
    r <- names(nd_len[nd_len == length(x)])
    r <- paste0(r, seq_along(x))
    names(r) <- x
    return(r)
  })
  
  # Main nodes
  for(i in seq_along(nd_type)){
    
    nd <- nodes_layout[[i]]

    if(nd_type[i] != "sidebox"){
      # Update node labels
      main_txt <- update_node_label(main_txt = main_txt,
                                    node = nd,
                                    group_name = group_name[[i]])
    }
    
    # Skip if this is the last one
    if(i == length(nd_type))
      next
    
    if(length(nodes_layout[[i+1]]) != length(nd)){
      
      prev_node <- lapply(nodes_layout[[i+1]], function(ndx){
        fm_node <- consort_plot[[ndx]]$prev_node
        data.frame(from = fm_node,
                   to = rep(ndx, length(fm_node)))
      })
      prev_node <- do.call(rbind, prev_node)
      
      nd_lst <- mk_invs_split(from_node = prev_node$from, 
                              to_node = prev_node$to,
                              consort_plot = consort_plot,
                              current_group = group_name[[i]],
                              next_group = group_name[[i+1]])
      
      rnk_nd <- c(rnk_nd, nd_lst[["rank"]])
      inv_nd <- c(inv_nd, nd_lst[["invs"]])
      con_nd <- c(con_nd, nd_lst[["connect"]])
      
      next
    }
    
    # The next one and current one is not a side box
    if(length(nodes_layout[[i+1]]) == length(nd) & all(!c(nd_type[i], nd_type[i+1]) %in% "sidebox")){
      
      for(j in seq_along(nd)){

        current_nd <- nodes_layout[[i]][j]
        if(current_nd %in% nodes_connect & !is_empty(consort_plot[[current_nd]]$text)){
          connect_to <- max(which(nodes_connect == current_nd))
          connect_to <- names(nodes_connect[connect_to])
          if(!is_empty(consort_plot[[connect_to]]$text))
            con_nd <- c(con_nd, sprintf("%s -> %s;", 
                                          current_nd,
                                          connect_to))
          else
            con_nd <- c(con_nd, sprintf("%s -> %s [arrowhead = none];", 
                                          current_nd,
                                          connect_to))
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
          nd_lst <- mk_invs_side(nodes_layout[[i-1]], 
                                 nd, 
                                 nodes_layout[[i+1]],
                                 group = group_name[[i]])
          rnk_nd <- c(rnk_nd, nd_lst[["rank"]])
          inv_nd <- c(inv_nd, nd_lst[["invs"]])
          con_nd <- c(con_nd, nd_lst[["connect"]])
        }
      }else{
        for(j in seq_along(nd)){

          # Skip if text is blank
          if(is_empty(consort_plot[[nd[j]]]$text)){
            # nd_next <- nodes_layout[[i+1]][j]
            nd_next <- max(which(nodes_connect == nodes_layout[[i-1]][j]))
            nd_next <- names(nodes_connect[nd_next])
            # No arrow if next link has no text
            if(!is_empty(consort_plot[[nd_next]]$text)){
              con_nd <- c(con_nd, sprintf("%s -> %s;", 
                                          nodes_layout[[i-1]][j],
                                          nd_next))
            }
            sp_box <- sp_box[-j]
          }else{
            nd_lst <- mk_invs_side(nodes_layout[[i-1]][j], 
                                   nd[j], 
                                   nodes_layout[[i+1]][j],
                                   group = group_name[[i]][j])
            
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
    
    if(nd_type[i] == "vertbox"){
      rnk_nd <- c(rnk_nd, mk_subgraph_rank(nodes_layout[[i]]))
    }
  }
  
  # Make sure all connections are linked
  if(any(!grepl(paste(nodes_connect, collapse = "|"), con_nd))){
    nd_none <- sapply(nodes_connect, function(x){
      !grepl(x, con_nd)
    })
    nd_none <- apply(nd_none, 2, all)
    nd_none <- nodes_connect[nd_none]
    
    con_nd_extra <- sapply(seq_along(nd_none), function(idx){
      con_from <- nd_none[idx]
      con_to <- names(nd_none[idx])
      if(consort_plot[[con_to]]$node_type == "sidebox"){
        return("")
      }else{
        if(!is_empty(consort_plot[[con_to]]$text))
          sprintf("%s -> %s;", con_from, con_to)
        else
          sprintf("%s -> %s [arrowhead = none];", con_from, con_to)
      }
    })
    
  }else{
    con_nd_extra <- ""
  }

  
  rnk_nd <- unique(rnk_nd)
  inv_nd <- unique(inv_nd)
  con_nd <- unique(c(con_nd, con_nd_extra))
  
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
  
  # Remove empty labels
  # main_txt <- main_txt[!is_empty(main_txt[,"text"]),]
  
  # Build dot

  grviz_txt <- paste("digraph consort_diagram {
  graph [layout = dot, splines=ortho]",
  lab_nd,
  lab_edge,
  "# node definitions with substituted label text
  node [shape = rectangle, fillcolor = Biege, style=\"\", fillcolor = \"\", color = \"\"]",
  paste(main_txt[,"node"], collapse = "\n"), # Node 
  "\n## Invisible point node for joints",
  "node [shape = point, width = 0, style=invis]",
  paste(inv_nd, collapse = "\n"),             # Invisible node 
  paste(rnk_nd, collapse = "\n"),            # Ranks
  "edge[style=\"\"];",
  paste(con_nd, collapse = "\n"),            # Connections
  "\n}\n",
  # paste(main_txt[,"txt"], collapse = "\n"),  # Text 
  sep = "\n\n")
  
  return(grviz_txt)
  
}


# Update node label
#' @keywords internal

update_node_label <- function(main_txt, node, group_name){
  
  for(j in seq_along(node)){
    idx_row <- main_txt[,1] == node[j]
    main_txt[idx_row, "node"] <- paste(node[j],
                                       mk_text_align(main_txt[idx_row, "text"], 
                                                     main_txt[idx_row, "just"],
                                                     group = group_name[j]))
  }
  
  return(main_txt)
}

