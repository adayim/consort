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

  # Calculate Y
  # pad_u <- convertUnit(unit(1, "char"), "mm", valueOnly = TRUE)
  pad_u <- 3
  
  nd_y <- vector("list", length = length(nodes_layout))
  for(i in seq_along(nodes_layout)){
    nd <- sapply(consort_plot[nodes_layout[[i]]], function(x)
      get_coords(x$box)$height
    )
    if(i == 1){
      nd_y[[i]] <- nd/2
      prev_bt <- max(nd)
    }else{
      
      if(length(nd_y[[i]]) != length(nd_y[[i-1]]))
        add_padd <- 2*pad_u
      else
        add_padd <- pad_u
      
      nd_y[[i]] <- prev_bt + add_padd + nd/2
      prev_bt <- prev_bt + add_padd + max(nd)
    }
    names(nd_y[[i]]) <- names(nd)
  }
  
  # Calculate X
  nd_x <- vector("list", length = length(nodes_layout))
  idx <- sapply(nodes_layout, length)
  nd_gp <- gp_consecutive(idx)
  
  # Nodes width
  nd_wd <- lapply(nodes_layout, function(nd){
    sapply(consort_plot[nd], function(x){
      get_coords(x$box)$width
    })
  })
  
  # Nodes side
  nd_sides <- lapply(nodes_layout, function(nd){
    unlist(sapply(consort_plot[nd], function(nd)nd$side))
  })
  
  for(i in unique(nd_gp)){
    idx_layout <- which(nd_gp %in% i)
    sub_layout <- nodes_layout[idx_layout]
    
    sub_len <- unique(sapply(sub_layout, length))
    
    sb_wd <- do.call(rbind, nd_wd[idx_layout])
    nd_tp <- nd_type[idx_layout]
    nd_sd <- nd_sides[idx_layout][nd_tp %in% "sidebox"]
    nd_sd <- do.call(rbind, nd_sd)

    if(sub_len == 1){
      for(j in idx_layout){
        if(nd_type[j] != "sidebox"){
          nd_x[[j]] <- 0
        }else{
          nd_x[[j]] <- ifelse(nd_sides[[j]] == "right", 
                              nd_wd[[j]]/2 + pad_u,
                              -nd_wd[[j]]/2 - pad_u)
        }
        names(nd_x[[j]]) <- nodes_layout[[j]]
      }
    }else{
      
      if(any(nd_tp %in% "sidebox")){
        pos_tmp <- apply(sb_wd[!nd_tp %in% "sidebox",], 2, max)
        pos_x <- vector("numeric", length = sub_len)
        
        # Calculate x for splits
        for(j in 1:sub_len){
          if(j == 1){
            if(any("left" %in% nd_sd[,1])){
              
              # Width of the left
              lt_max <- sb_wd[nd_tp %in% "sidebox", 1][nd_sd[,1] %in% "left"]
              
              if(max(lt_max) > pos_tmp[1]/2)
                pos_x[1] <- max(lt_max) 
              else
                pos_x[1] <- pos_tmp[1]/2
            }else{
              pos_x[1] <- pos_tmp[1]/2
            }
            pos_x[1] <- pos_x[1] + pad_u
          }else{
            if(any("right" %in% nd_sd[,j-1])){
              rt_max <- sb_wd[nd_tp %in% "sidebox", 1][nd_sd[,j-1] %in% "right"]
              
              if(max(rt_max) > pos_tmp[j-1]/2){
                rt_max <- max(rt_max)
              }else{
                rt_max <- pos_tmp[j-1]/2
              }
              
            }else{
              rt_max <- pos_tmp[j-1]/2
            }
            
            if(any("left" %in% nd_sd[,j])){
              lt_max <- sb_wd[nd_tp %in% "sidebox", 1][nd_sd[,j] %in% "left"]
              if(max(lt_max) > pos_tmp[j]/2){
                lt_max <- max(lt_max)
              }else{
                lt_max <- pos_tmp[j]/2
              }
            }else{
              lt_max <- pos_tmp[j]/2
            }
            
            pos_x[j] <- pos_x[j-1] + lt_max + rt_max + 2*pad_u
            
          }
        }

        pos_x <- pos_x - mean(pos_x)
        
        for(j in idx_layout){
          if(nd_type[j] != "sidebox"){
            nd_x[[j]] <- pos_x
          }else{
            sd_tmp <- nd_sides[[j]]
            for(k in 1:sub_len){
              nd_x[[j]][k] <- ifelse(sd_tmp[k] == "right", 
                                     pos_x[k] + nd_wd[[j]][k]/2 + pad_u,
                                     pos_x[k] - nd_wd[[j]][k]/2 - pad_u)
            }
          }
          
          names(nd_x[[j]]) <- nodes_layout[[j]]
        }
        
      }else{
        pos_tmp <- apply(sb_wd, 2, max)
        pos_x <- pos_tmp/2 + c(0, cumsum(pos_tmp[-length(pos_tmp)] + 4*pad_u))
        # Make sure center is 0
        pos_x <- pos_x - mean(pos_x)
        for(j in idx_layout){
          nd_x[[j]] <- pos_x
          names(nd_x[[j]]) <- nodes_layout[[j]]
        }
      }
    }
  }
  
  # Adjust coordinates
  nd_minmax <- lapply(seq_along(nodes_layout), function(x){
    nd_len <- length(nodes_layout[[x]])
    if(nd_len != 1){
      tmp_wd <- do.call(rbind, nd_wd[x])
      tmp_x <- do.call(rbind, nd_x[x])
      max_x <- max(tmp_x[,nd_len] + tmp_wd[,nd_len]/2)
      min_x <- min(tmp_x[, 1] - tmp_wd[,1]/2)
      return(c(minx = min_x, maxx = max_x))
    }else{
      return(c(minx = nd_x[[x]] - nd_wd[[x]]/2, maxx = nd_x[[x]] + nd_wd[[x]]/2))
    }
  })
  nd_minmax <- Filter(Negate(is.null), nd_minmax)
  min_val <- min(do.call(rbind, nd_minmax)[,1])
  max_val <- max(do.call(rbind, nd_minmax)[,2])
  for(i in seq_along(nodes_layout)){
    nd_x[[i]] <- nd_x[[i]] - min_val
  }
  max_width <- max_val - min_val
  
  
  return(list(x = unlist(nd_x), 
              y = unlist(nd_y),
              nodes_hw = nd_wd, 
              nd_x = nd_x, 
              nd_y = nd_y,
              max_width = max_width,
              max_height = prev_bt))
}

# Calculate coordinates
#' @keywords internal
#'
calc_coords_label <- function(label_plot, node_y, max_h){

  lab_wd <- sapply(label_plot, function(x){
    c(w = get_coords(x$box)$width, h = get_coords(x$box)$height)
  })
  
  lab_pos <- sapply(label_plot, function(x){
    x$prev_node
  })
  
  lab_y <- node_y[lab_pos]
  lab_y <- sapply(lab_y, mean)
  lab_y <- (max_h - lab_y)/max_h
  names(lab_y) <- colnames(lab_wd)

  lab_x <- (lab_wd["w",]/2)
  names(lab_x) <- colnames(lab_wd)

  return(list(width = max(lab_wd["w",]),
              x = lab_x, # Put inside
              y = lab_y))
  
}

# Create groups if consecutive 
#' @keywords internal
#'
#'
gp_consecutive <- function(x){
  int <- 1
  gp <- vector("character", length = length(x))
  gp[1] <- letters[int]
  for(i in 2:length(x)){
    if(x[i] != x[i-1])
      int <- int + 1
    gp[i] <- letters[int]
  }
  return(gp)
}

