
# Make subgraph same rank
#' @keywords internal
mk_subgraph_rank <- function(x){
  sprintf("subgraph {
  rank = same; rankdir = LR; %s;
  }", paste(x, collapse = "; "))
}

# Make invisible connections
#' @keywords internal
mk_invs_connect <- function(x){
  sprintf("%s [arrowhead = none];", paste(x, collapse = " -> "))
}

# Make text alignment
#' @keywords internal
mk_text_align <- function(text, just, group = NULL){
  # If empty
  # if(is_empty(text))
  #   return("")
  
  jst <- ifelse(just == "center", "", 
                ifelse(just == "left", "\\l", "\r"))
  
  if(just %in% c("left", "right")){
    text <- unlist(strsplit(text, "\n"))
    text <- ifelse(just == "left",
                   paste(text, collapse = "\\l"),
                   paste(text, collapse = "\r"))
  }
  
  if(is.null(group))
    r <- sprintf("[label = \"%s%s\"]", text, jst)
  else
    r <- sprintf("[label = \"%s%s\" group=%s]", text, jst, group)

  if(is_empty(text)){
    r <- gsub('.{1}$', ' shape=none height=0 width=0]', r)
  }
  return(r)
}

# Make invisible nodes for split nodes
#' @keywords internal
mk_invs_split <- function(from_node, 
                          to_node,
                          consort_plot,
                          current_group,
                          next_group){
  
  # Get middle element
  mid_pos <- function(vec) ceiling(length(vec)/2)

  one2more <- function(node1, node2, gp1, gp2){
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
      # Insert the previous one in the middile
      gp2 <- append(gp2, gp1, after = length(gp2)/2)
    } else {
      invs_nd <- paste0("P", seq_len(length(node2)) + get_invs())
      inv2nd <- sprintf("%s -> %s;", invs_nd, node2)
    }
    nd2inv <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd[mid_pos(invs_nd)])
    
    nd2inv <- c(nd2inv, inv2nd, mk_invs_connect(invs_nd))
    
    set_invs(get_invs() + length(invs_nd))

    invs_nd_gp <- sprintf("%s [group=%s]", invs_nd, gp2)
    
    return(list(connect = nd2inv, invs = invs_nd, invs_gp = invs_nd_gp))
  }

  more2one <- function(node1, node2, gp1, gp2){
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
      # Insert the previous one in the middile
      gp1 <- append(gp1, gp2, after = length(gp1)/2)
    } else {
      invs_nd <- paste0("P", seq_len(length(node1)) + get_invs())
      inv2nd <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd)
    }
    nd2inv <- sprintf("%s -> %s;", invs_nd[mid_pos(invs_nd)], node2)
    
    nd2inv <- c(nd2inv, inv2nd, mk_invs_connect(invs_nd))
    
    set_invs(get_invs() + length(invs_nd))

    invs_nd_gp <- sprintf("%s [group=%s]", invs_nd, gp1)
    
    return(list(connect = nd2inv, invs = invs_nd, invs_gp = invs_nd_gp))
  }
  
  if(anyDuplicated(from_node) > 0){
    # One to more
    nds <- lapply(unique(from_node), function(x){
      nd2 <- to_node[which(from_node == x)]
      gp1 <- current_group[names(current_group) %in% x]
      gp2 <- next_group[names(next_group) %in% nd2]
      one2more(x, nd2, gp1, gp2)
    })
    nd_rank <- mk_subgraph_rank(unique(to_node))
  }else{
    # More to one
    nds <- lapply(unique(to_node), function(x){
      nd1 <- from_node[which(to_node == x)]
      gp1 <- current_group[names(current_group) %in% nd1]
      gp2 <- next_group[names(next_group) %in% x]
      more2one(nd1, x, gp1, gp2)
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
  
  invs_gp <- unlist(lapply(nds, "[[", "invs_gp"), use.names = FALSE)
  
  return(list("invs" = invs_gp, 
              "connect" = connect,
              "rank" = sm_rnk))
  }


# Make invisible side nodes
#' @keywords internal
mk_invs_side <- function(node1, node2, node3, group = NULL){  
  # For side nodes, nodes is the side node
  # This is for:
  #    Node
  #     |->ND
  #    Node

  invs_nd <- sprintf("P%i", get_invs()+1)

  nd2inv <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd)
  inv2nd <- sprintf("%s -> %s;", invs_nd, c(node2, node3))
  nd2inv <- c(nd2inv, inv2nd)
  sm_rnk <- mk_subgraph_rank(c(invs_nd, node2))
  
  set_invs(get_invs() + length(invs_nd))

  if(!is.null(group))
    invs_nd <- sprintf("%s [group=%s]", invs_nd, group) 
  
  return(list("invs" = invs_nd, 
              "connect" = nd2inv,
              "rank" = sm_rnk))
}



