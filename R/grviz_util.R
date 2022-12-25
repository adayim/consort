
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
  sprintf("%s [arrowhead = none, minlen = 10];", 
          paste(x, collapse = " -> "))
}

# Make text alignment
#' @keywords internal
mk_text_align <- function(text, just){
  jst <- ifelse(just == "center", "", 
                ifelse(just == "left", "\\l", "\r"))
  
  if(just %in% c("left", "right")){
    text <- unlist(strsplit(text, "\n"))
    text <- ifelse(just == "left",
                   paste(text, collapse = "\\l"),
                   paste(text, collapse = "\r"))
  }
  
  sprintf("[label = '%s%s']", text, jst)
}

# Make invisible nodes
#' @keywords internal
mk_invs_node <- function(node1, node2, node3 = NULL){
  
  # Get middle element
  mid_pos <- function(vec) ceiling(length(vec)/2)
  
  if(is.null(node3)){
    # For split or combining node
    if(length(node1) == 1 & length(node2) > 1){
      if((length(node2) %% 2) == 0) {
        invs_nd <- paste0("P", seq_len(length(node2)+1) + get_invs())
      } else {
        invs_nd <- paste0("P", seq_len(length(node2)) + get_invs())
      }
      nd2inv <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd[mid_pos(invs_nd)])
      inv2nd <- sprintf("%s -> %s [arrowhead = none];", invs_nd[-mid_pos(invs_nd)], node2)
      nd2inv <- c(nd2inv, inv2nd, mk_invs_connect(invs_nd))
      nd_rank <- mk_subgraph_rank(node2)
    }else{
      if((length(node1) %% 2) == 0) {
        invs_nd <- paste0("P", seq_len(length(node1)+1) + get_invs())
      } else {
        invs_nd <- paste0("P", seq_len(length(node1)) + get_invs())
      }
      nd2inv <- sprintf("%s -> %s;", invs_nd[mid_pos(invs_nd)], node2)
      inv2nd <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd[-mid_pos(invs_nd)])
      nd2inv <- c(nd2inv, inv2nd, mk_invs_connect(invs_nd))
      nd_rank <- mk_subgraph_rank(node1)
    }
    sm_rnk <- c(mk_subgraph_rank(invs_nd), nd_rank)
  }else{
    # For side nodes, nodes is the side node
    invs_nd <- paste0("P", get_invs()+1)
    nd2inv <- sprintf("%s -> %s [arrowhead = none];", node1, invs_nd)
    inv2nd <- sprintf("%s -> %s;", invs_nd, c(node2, node3))
    nd2inv <- c(nd2inv, inv2nd)
    sm_rnk <- mk_subgraph_rank(c(invs_nd, node2))
  }
  
  set_invs(get_invs() + length(invs_nd))
  
  return(list("invs" = invs_nd, 
              "connect" = nd2inv,
              "rank" = sm_rnk))
}

