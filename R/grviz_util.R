
# Convert R gpar to Graphviz node attributes
#' @keywords internal
gpar_to_grviz_attrs <- function(txt_gp, box_gp, box_fn = NULL) {
  attrs <- character()
  style_parts <- character()

  # Rounded corners for roundrectGrob
  if (!is.null(box_fn) && identical(box_fn, grid::roundrectGrob)) {
    style_parts <- c(style_parts, "rounded")
  }

  # Box fill color
  if (!is.null(box_gp$fill) && nzchar(box_gp$fill)) {
    attrs <- c(attrs, sprintf('fillcolor="%s"', box_gp$fill))
    style_parts <- c(style_parts, "filled")
  }

  # Box border color
  if (!is.null(box_gp$col)) {
    attrs <- c(attrs, sprintf('color="%s"', box_gp$col))
  }

  # Box border width
  if (!is.null(box_gp$lwd)) {
    attrs <- c(attrs, sprintf('penwidth=%s', box_gp$lwd))
  }

  # Line type
  if (!is.null(box_gp$lty)) {
    lty_val <- box_gp$lty
    lty_map <- c("2" = "dashed", "3" = "dotted", "4" = "dotdash",
                 "5" = "longdash", "6" = "twodash",
                 "dashed" = "dashed", "dotted" = "dotted",
                 "dotdash" = "dotdash", "longdash" = "longdash",
                 "twodash" = "twodash")
    lty_str <- as.character(lty_val)
    if (lty_str %in% names(lty_map)) {
      style_parts <- c(style_parts, lty_map[lty_str])
    }
  }

  # Style attribute
  if (length(style_parts) > 0) {
    attrs <- c(attrs, sprintf('style="%s"', paste(style_parts, collapse = ",")))
  }

  # Text color
  if (!is.null(txt_gp$col)) {
    attrs <- c(attrs, sprintf('fontcolor="%s"', txt_gp$col))
  }

  # Font size
  if (!is.null(txt_gp$fontsize)) {
    attrs <- c(attrs, sprintf('fontsize=%s', txt_gp$fontsize))
  } else if (!is.null(txt_gp$cex) && txt_gp$cex != 1) {
    attrs <- c(attrs, sprintf('fontsize=%s', round(14 * txt_gp$cex, 1)))
  }

  # Font family
  if (!is.null(txt_gp$fontfamily) && nzchar(txt_gp$fontfamily)) {
    attrs <- c(attrs, sprintf('fontname="%s"', txt_gp$fontfamily))
  }

  if (length(attrs) == 0) return(NULL)
  paste(attrs, collapse = " ")
}

# Convert arrow settings to Graphviz edge attributes
#' @keywords internal
arrow_to_grviz_attrs <- function(arrow_gp, arrow_type, arrow_length) {
  attrs <- character()

  # Edge color
  if (!is.null(arrow_gp$col)) {
    attrs <- c(attrs, sprintf('color="%s"', arrow_gp$col))
  }

  # Edge line width
  if (!is.null(arrow_gp$lwd)) {
    attrs <- c(attrs, sprintf('penwidth=%s', arrow_gp$lwd))
  }

  # Edge line type — always emit style to reset from label edges' style=invis
  edge_style <- ""
  if (!is.null(arrow_gp$lty)) {
    lty_map <- c("2" = "dashed", "3" = "dotted", "4" = "dotdash",
                 "5" = "longdash", "6" = "twodash",
                 "dashed" = "dashed", "dotted" = "dotted",
                 "dotdash" = "dotdash", "longdash" = "longdash",
                 "twodash" = "twodash")
    lty_str <- as.character(arrow_gp$lty)
    if (lty_str %in% names(lty_map)) {
      edge_style <- lty_map[lty_str]
    }
  }
  attrs <- c(attrs, sprintf('style="%s"', edge_style))

  # Arrowhead type: R "closed" -> Graphviz "normal", R "open" -> "vee"
  if (!is.null(arrow_type)) {
    ah <- if (arrow_type == "open") "vee" else "normal"
    attrs <- c(attrs, sprintf('arrowhead="%s"', ah))
  }

  # Arrow size: scale relative to default 0.1 inches
  if (!is.null(arrow_length) && is.numeric(arrow_length)) {
    attrs <- c(attrs, sprintf('arrowsize=%.2f', arrow_length / 0.1))
  }

  if (length(attrs) == 0) return("")
  paste(attrs, collapse = " ")
}

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
mk_text_align <- function(text, just, group = NULL, grviz_style = NULL){

  if (has_markup(text)) {
    # Graphviz HTML-like label: <html> instead of "plain"
    html <- markup_to_html(text)

    if (just == "left") {
      html <- gsub("<br/>", '<br align="left"/>', html, fixed = TRUE)
      html <- paste0(html, '<br align="left"/>')
    } else if (just == "right") {
      html <- gsub("<br/>", '<br align="right"/>', html, fixed = TRUE)
      html <- paste0(html, '<br align="right"/>')
    }

    attr_parts <- sprintf("label = <%s>", html)
  } else {
    # Original plain-text label
    jst <- ifelse(just == "center", "",
                  ifelse(just == "left", "\\l", "\r"))

    if(just %in% c("left", "right")){
      text <- unlist(strsplit(text, "\n"))
      text <- ifelse(just == "left",
                     paste(text, collapse = "\\l"),
                     paste(text, collapse = "\r"))
    }

    attr_parts <- sprintf('label = "%s%s"', text, jst)
  }

  # Build attributes
  if(!is.null(group)) attr_parts <- paste(attr_parts, sprintf("group=%s", group))
  if(!is.null(grviz_style)) attr_parts <- paste(attr_parts, grviz_style)

  r <- sprintf("[%s]", attr_parts)

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



