# Calculate coordinates
#' @keywords internal
#' @importFrom stats setNames
calc_coords <- function(consort_plot){
  
  # Get the maximum of height and width of each node
  nodes_layout <- attr(consort_plot, "nodes.list")
  
  # Node type of each
  nd_type <- sapply(nodes_layout, function(x) 
    unique(sapply(consort_plot[x], "[[", "node_type"))
  )
  
  nodes_hw <- lapply(nodes_layout, function(x){
    hw <- lapply(consort_plot[x], "[[", "box_hw")
    height <- max(sapply(hw, "[[", "height"))
    width <- max(sapply(hw, "[[", "width"))*length(x)
    c(height = height, width = width)
  })
  nd_height <- sapply(nodes_hw, "[[", 1)
  
  # More space for split
  nd_len <- sapply(nodes_layout, length)
  nd_len_ld <- nd_len - c(nd_len[-1], NA)
  nd_len_ld <- which(!nd_len_ld %in% c(0, NA))
  nd_height[nd_len_ld] <- 1.1*nd_height[nd_len_ld]
  
  nd_width <- sapply(nodes_hw, "[[", 2)
  
  # Calculate Y
  nd_y <- lapply(seq_along(nodes_layout), function(i){
    nd <- sapply(consort_plot[nodes_layout[[i]]], function(x)x$box_hw$height)
    if(i == 1){
      nd/2
    }else{
      sum(nd_height[1:c(i - 1)]) + nd/2
    }
  })
  
  # Calculate X
  nd_x <- vector("list", length = length(nodes_layout))
  
  if(any(nd_type == "splitbox"))
    bf_split <- 1:c(which(nd_type == "splitbox") - 1)
  else
    bf_split <- seq_along(nd_type)
  
  # For side box indenting
  vt_node <- max(nd_width) * 0.02
  
  # Middle of the plot
  mid_coord_x <- max(nd_width)/2
  
  # X coordinates for nodes before split
  for(i in bf_split){
    nd_name <- nodes_layout[[i]]
    
    if(nd_type[i] == "vertbox"){
      max_wd <- sapply(consort_plot[nd_name], function(nd)nd$box_hw$width)
      mid_x <- calc_coords_x(max_wd, mid_coord_x)

      if(all(nd_len == 1))
        mid_x <- mid_coord_x*0.8

      nd_x[[i]] <- setNames(mid_x, nd_name)
    }
    
    if(nd_type[i] == "sidebox"){
      sides <- sapply(consort_plot[nd_name], "[[", "side")
      half_width <- sapply(consort_plot[nd_name], function(x){
        prev_nd <- consort_plot[[x$prev_node]]
        c(x$box_hw$half_width, prev_nd$box_hw$half_width)
      })
      
      pos_x <- ifelse(sides == "right",
                      nd_x[[i-1]] + half_width[1] + vt_node,
                      nd_x[[i-1]] - half_width[1] - vt_node)
      
      nd_x[[i]] <- setNames(pos_x, nd_name)
    }
    
  }
  
  # Nodes after split
  if(any(nd_type == "splitbox")){
    
    af_split <- which(nd_type == "splitbox"):length(nd_type)
    
    # Width without side box
    sd_box <- nd_type[af_split] == "sidebox"
    sp_nd <- nodes_layout[af_split][!sd_box]
    
    # Width of maximum vertical node for x
    sp_wd_nd <- sapply(sp_nd, function(x){
      sapply(consort_plot[x], function(j)get_coords(j$box)$width)
    })
    sp_wd_nd <- apply(sp_wd_nd, 1, max)/2
    
    n_gp <- length(sp_wd_nd)
    # if (n_gp == 2) {
    #   sp_x <- c(mid_coord_x - sp_wd_nd[1], mid_coord_x + sp_wd_nd[1])
    # } else {
    #   sp_x <- mid_coord_x / n_gp
    #   sp_x <- c(sp_x, rep(2 * sp_x, times = n_gp - 1))
    #   sp_x <- cumsum(sp_x)
    # }
    
    vn_max <- sapply(nodes_layout[af_split][!sd_box], function(x){
      max(sapply(consort_plot[x], function(j)get_coords(j$box)$width))
    })
    vn_max <- max(vn_max)
    
    if((n_gp %% 2) == 0) {
      num <- 0.5 + 0:(n_gp/2 - 1)
      num <- c(-num, num)
      sp_x <- mid_coord_x + vn_max*num
    } else {
      num <- 1:((n_gp - 1)/2)
      sp_x <- c(mid_coord_x - rev(num)*vn_max, 
                mid_coord_x,
                mid_coord_x + num*vn_max)
    }
    
    if(any(sd_box)){
      
      sn_max <- sapply(nodes_layout[af_split][sd_box], function(x){
        max(sapply(consort_plot[x], function(j)get_coords(j$box)$width))
      })
      sn_max <- max(sn_max)
      
      sp_side <- sapply(unlist(nodes_layout[af_split][sd_box]), function(x){
        consort_plot[[x]]$side
      })
      if((n_gp %% 2) == 0) {
        num_j <- num
      } else {
        num_j <- c(-rev(num), 0, num)
      }
      
      for(j in seq_along(sp_side)){
        if(sp_side[j] == "right")
          sp_x[j] <- mid_coord_x + num_j[j]*sn_max*1.2
      }
    }
    
    for(i in af_split){
      
      nd_name <- nodes_layout[[i]]
      
      if(nd_type[i] %in% c("vertbox", "splitbox")){
        nd_x[[i]] <- setNames(sp_x, nd_name)
      }
      
      if(nd_type[i] == "sidebox"){
        pos_x <- vector("numeric", length(nd_name))
        for(j in seq_along(nd_name)){
          sides <- sapply(consort_plot[nd_name[j]], "[[", "side")
          half_width <- sapply(consort_plot[nd_name[j]], function(x){
            prev_nd <- consort_plot[[x$prev_node]]
            c(x$box_hw$half_width, prev_nd$box_hw$half_width)
          })
          
          pos_x[j] <- ifelse(sides == "right",
                             sp_x[j] + half_width[1] + vt_node,
                             sp_x[j] - half_width[1] - vt_node)
        }
        nd_x[[i]] <- setNames(pos_x, nd_name)
      }
      
    }
    
  }
  
  y <- (sum(nd_height) - unlist(nd_y))/sum(nd_height)
  x <- unlist(nd_x)/c(max(nd_width) + 10)
  
  return(list(x = x, y = y, nodes_hw = nodes_hw))
}

# Calculate coordinates x for horizontal nodes
#' @keywords internal
#'
calc_coords_x <- function(plt_width, mid_pos){
  max_wd <- max(plt_width)
  n_gp <- length(plt_width)
  if((n_gp %% 2) == 0) {
    num <- 0.7 + 0:(n_gp/2 - 1)
    num <- c(-num, num)
    coord_x <- mid_pos + max_wd*num
  } else {
    if(n_gp == 1){
      coord_x <- mid_pos
    }else{
      num <- 1:((n_gp - 1)/2)
      coord_x <- c(mid_pos - rev(num)*max_wd, 
                   mid_pos,
                   mid_pos + num*max_wd)
    }
  }
  return(coord_x)
}

# Calculate coordinates
#' @keywords internal
#'
calc_coords_label <- function(label_plot, nodes_hw){
  
  # Get the maximum of height and width of each node
  nodes_hw <- do.call(rbind, nodes_hw)
  
  lab_wd <- sapply(label_plot, function(x){
    c(w = x$box_hw$width, h = x$box_hw$height)
  })
  
  lab_pos <- sapply(label_plot, function(x){
    x$prev_node
  })
  
  lab_y <- vector("numeric", length(lab_pos))
  for(i in seq_along(lab_pos)){
    if(lab_pos[i] == 1)
      lab_y[i] <- lab_wd["h",1]/2
    else
      lab_y[i] <- sum(nodes_hw[1:(lab_pos[i]-1),"height"]) + lab_wd["h",i]/2
  }

  lab_y <- (sum(nodes_hw[,"height"]) - lab_y)/sum(nodes_hw[,"height"])
  names(lab_y) <- colnames(lab_wd)
  lab_x <- (lab_wd["w",]/2)/(max(nodes_hw[,"width"]) + 10)
  names(lab_x) <- colnames(lab_wd)

  return(list(lab_nd_width = c(max(lab_wd["w",]),
                               (max(nodes_hw[,"width"]) + 10)),
              x = lab_x, # Put inside
              y = lab_y))
  
}

