
# Make subgraph same rank
#' @keywords internal
mk_subgraph_rank <- function(x){
  sprintf("subgraph {
  rank = same; rankdir = LR; %s;
  }", paste(x, collapse = "; "))
}

# Make invisible connections
#' @keywords internal
mk_invs_connect <- function(x, width){
  sprintf("%s [arrowhead = none, minlen = %.1f];", 
          paste(x, collapse = " -> "), width)
}

# Make text alignment
#' @keywords internal
mk_text_align <- function(text, just){
  # If empty
  if(is_empty(text))
    return(NA)
  
  jst <- ifelse(just == "center", "", 
                ifelse(just == "left", "\\l", "\r"))
  
  if(just %in% c("left", "right")){
    text <- unlist(strsplit(text, "\n"))
    text <- ifelse(just == "left",
                   paste(text, collapse = "\\l"),
                   paste(text, collapse = "\r"))
  }
  
  sprintf("[label = \"%s%s\"]", text, jst)
}

# Make invisible nodes for split nodes
#' @keywords internal
mk_invs_split <- function(from_node, 
                          to_node,
                          consort_plot, 
                          nodes_layout, 
                          node_infor){
  
  # Get middle element
  mid_pos <- function(vec) ceiling(length(vec)/2)

  one2more <- function(node1, node2, width){
    # This is one to more split
    #       Node1
    #         |
    #   +-----+------+
    #   |     |      |
    # Node2  Node2  Node2

    # Determine which one should be in the middle
    if((length(node2) %% 2) == 0) {
      invs_nd <- paste0("P", seq_len(length(node2)+1) + get_invs())
      inv2nd <- sprintf("%s -> %s;", invs_nd[-mid_pos(invs_nd)], node2)
    } else {
      invs_nd <- paste0("P", seq_len(length(node2)) + get_invs())
      inv2nd <- sprintf("%s -> %s;", invs_nd, node2)
    }
    nd2inv <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd[mid_pos(invs_nd)])
    
    nd2inv <- c(nd2inv, inv2nd, mk_invs_connect(invs_nd, width))
    
    set_invs(get_invs() + length(invs_nd))
    
    return(list(connect = nd2inv, invs = invs_nd))
  }

  more2one <- function(node1, node2, width){
    # This is more to one
    # Node1  Node1  Node1
    #   |     |      |
    #   +-----+------+
    #         |
    #       Node2
    # Determine which one should be in the middle
    if((length(node1) %% 2) == 0) {
      invs_nd <- paste0("P", seq_len(length(node1)+1) + get_invs())
      inv2nd <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd[-mid_pos(invs_nd)])
    } else {
      invs_nd <- paste0("P", seq_len(length(node1)) + get_invs())
      inv2nd <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd)
    }
    nd2inv <- sprintf("%s -> %s;", invs_nd[mid_pos(invs_nd)], node2)
    
    nd2inv <- c(nd2inv, inv2nd, mk_invs_connect(invs_nd, width))
    
    set_invs(get_invs() + length(invs_nd))
    
    return(list(connect = nd2inv, invs = invs_nd))
  }
  
  if(anyDuplicated(from_node) > 0){
    # One to more
    nds <- lapply(unique(from_node), function(x){
      nd2 <- to_node[which(from_node == x)]
      nd_width <- getwd_one2more(nd2, 
                                 consort_plot = consort_plot, 
                                 nodes_layout = nodes_layout,
                                 node_infor = node_infor)
      one2more(x, nd2, width = nd_width)
      
    })
    nd_rank <- mk_subgraph_rank(unique(to_node))
  }else{
    # More to one
    nds <- lapply(unique(to_node), function(x){
      nd1 <- from_node[which(to_node == x)]
      nd_width <- getwd_more2one(nd1, 
                                 consort_plot = consort_plot, 
                                 nodes_layout = nodes_layout,
                                 node_infor = node_infor)
      more2one(nd1, x, width = nd_width)
      
    })
    nd_rank <- mk_subgraph_rank(unique(from_node))
  }
  
  invs <- unlist(lapply(nds, "[[", "invs"), use.names = FALSE)
  connect <- unlist(lapply(nds, "[[", "connect"), use.names = FALSE)
  
  if(length(nds) > 1){
    invs_samelev <- sapply(1:(length(nds) - 1), function(i){
      prev_invs <- nds[[i]]$invs
      next_invs <- nds[[i+1]]$invs
      sprintf("%s -> %s [style=invis];", 
              prev_invs[length(prev_invs)], 
              next_invs[1])
    })
    connect <- c(connect, invs_samelev)
  }

  sm_rnk <- c(mk_subgraph_rank(invs), nd_rank)
  
  
  return(list("invs" = invs, 
              "connect" = connect,
              "rank" = sm_rnk))
  }


# Make invisible side nodes
#' @keywords internal
mk_invs_side <- function(node1, node2, node3){  
  # For side nodes, nodes is the side node
  # This is for:
  #    Node
  #     |->ND
  #    Node
  invs_nd <- paste0("P", get_invs()+1)
  nd2inv <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd)
  inv2nd <- sprintf("%s -> %s;", invs_nd, c(node2, node3))
  nd2inv <- c(nd2inv, inv2nd)
  sm_rnk <- mk_subgraph_rank(c(invs_nd, node2))
  
  set_invs(get_invs() + length(invs_nd))
  
  return(list("invs" = invs_nd, 
              "connect" = nd2inv,
              "rank" = sm_rnk))
}


# Get the maximum width of each vertical nodes
#' @keywords internal
getwd_one2more <- function(to_node, consort_plot, nodes_layout, node_infor){
  
  node_len <- sapply(nodes_layout, length)
  node_idx <- sapply(nodes_layout, function(x){
    any(to_node %in% x)
  })
  
  # Select the layout after current node
  layout_sub <- nodes_layout[which(node_idx):length(nodes_layout)]
  node_len <- node_len[which(node_idx):length(nodes_layout)]
  
  denom <- 2
  
  if(length(node_len) == 1){
    layout_width <- sapply(layout_sub, function(x){
      node_infor$width[which(node_infor$node %in% x)]
    })
    r_width <- as.numeric(layout_width)
    
    return(sum(r_width/denom))
  }
  
  if(all(diff(node_len) == 0)){
    # Split only once, no combine nor another split
    layout_mat <- do.call(rbind, layout_sub)
    if(nrow(layout_mat) == 1){
      layout_mat <- matrix(layout_mat, nrow = 1)
    }
    
  }else if(any(diff(node_len) < 0)){
    # Split one, then combine
    # Keep until the next combine
    n_len <- node_len >= length(to_node)
    
    layout_mat <- layout_sub[1:(min(which(n_len != n_len[1]))- 1)]
    layout_mat <- do.call(rbind, layout_mat)
    if(nrow(layout_mat) == 1){
      layout_mat <- matrix(layout_mat, nrow = 1)
    }
    
  }else{
    # all(diff(layout_len) >= 0)
    # Split and split
    layout_mat <- get_layout_mat(nd = unique(to_node), 
                                 consort_plot = consort_plot,
                                 nodes_layout = layout_sub)
    
    layout_mat <- do.call(rbind, layout_mat)

    denom <- 2.8
  }
  
  idx_mat <-  layout_mat[1,] %in% unique(to_node)
  layout_mat <- layout_mat[,idx_mat]

  layout_type <- apply(layout_mat, 2, function(x) 
    sapply(consort_plot[x], "[[", "node_type")
  )
  if(nrow(layout_type) == 1){
    layout_type <- matrix(layout_type, nrow = 1)
  }
  
  layout_type <- apply(layout_type, 1, unique)
  layout_type <- layout_type == "sidebox"
  
  # Get widths of each node
  layout_width <- apply(layout_mat, 2, function(x){
    node_infor$width[which(node_infor$node %in% x)]
  })
  if(nrow(layout_width) == 1){
    layout_width <- matrix(layout_width, nrow = 1)
  }
  
  # Don't count last column side box
  idx_o <- rowSums(layout_width[,-ncol(layout_width), drop = FALSE])
  
  layout_width[,ncol(layout_width)] <- ifelse(
    idx_o == 0, 
    ifelse(layout_type, 0, layout_width[,ncol(layout_width)]),
    2
  )
  
  r_width <- apply(layout_width, 2, max)
  sum(as.numeric(r_width)/denom)

}

#' @keywords internal
getwd_more2one <- function(from_node, consort_plot, nodes_layout, node_infor){
  
  node_len <- sapply(nodes_layout, length)
  node_idx <- sapply(nodes_layout, function(x){
    any(from_node %in% x)
  })
  
  layout_sub <- nodes_layout[1:which(node_idx)]
  node_len <- node_len[1:which(node_idx)] == length(from_node)
  layout_mat <- do.call(rbind, layout_sub[node_len])
  
  layout_type <- apply(layout_mat, 2, function(x) 
    sapply(consort_plot[x], "[[", "node_type")
  )
  if(nrow(layout_mat) == 1){
    layout_type <- matrix(layout_type, nrow = 1)
  }
  
  layout_type <- apply(layout_type, 1, unique)
  layout_type <- layout_type == "sidebox"
  
  # Get widths of each node
  layout_width <- apply(layout_mat, 2, function(x){
    node_infor$width[which(node_infor$node %in% x)]
  })
  if(nrow(layout_mat) == 1){
    layout_width <- matrix(layout_width, nrow = 1)
  }
  
  # Don't count last column side box
  idx_o <- rowSums(layout_width[,-ncol(layout_width), drop = FALSE])
  
  layout_width[,ncol(layout_width)] <- ifelse(
    idx_o == 0, 
    ifelse(layout_type, 0, layout_width[,ncol(layout_width)]),
    2
  )
  
  r_width <- apply(layout_width, 2, max)
  sum(as.numeric(r_width)/2)
}

# Extract layout matrix of the remaining nodes
#' @keywords internal
get_layout_mat <- function(nd, consort_plot, nodes_layout){
  idx <- sapply(nodes_layout, function(x){
    any(nd %in% x)
  })
  sub_layout <- nodes_layout[which(idx):length(nodes_layout)]
  for(i in length(sub_layout):1){
    if(i == length(sub_layout))
      next
    if(length(sub_layout[[i]]) == length(sub_layout[[i + 1]])){
      next
    }else{
      sub_layout[[i]] <- sapply(sub_layout[[i+1]], function(nd){
        consort_plot[[nd]]$prev_node
      }, USE.NAMES = FALSE)
    }
  }
  return(sub_layout)
}


# get_nodewidth <- function(nodes_layout, consort_plot){
#   nd_gp <- gp_consecutive(sapply(nodes_layout, length))
#   
#   nds_width <- lapply(nodes_layout, function(x){
#     sapply(x, function(nd){
#       floor(convertWidth(stringWidth(consort_plot[[nd]]$text),
#                          unitTo = "picas",
#                          valueOnly = TRUE))
#     })
#   })
#   
#   out <- nodes_layout
#   for(i in unique(nd_gp)){
#     idx_layout <- which(nd_gp %in% i)
#     sub_layout <- nds_width[idx_layout]
#     
#     for(j in 1:unique(sapply(sub_layout, length))){
#       sub_layout <- lapply(sub_layout, function(x) {
#         x[j] <- max(unlist(lapply(sub_layout, `[[`, j)))
#         x
#       })
#     }
#     out[idx_layout] <- sub_layout
#   }
#   return(out)
# }



