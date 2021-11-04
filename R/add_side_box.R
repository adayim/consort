#' Add a side node
#'
#' Add an exclusion node on the right side. If the length of text label is two, then
#' the first one will be aligned on the left and the second on the right. Otherwise,
#' all the side nodes will be aligned on the right.
#'
#' @param prev_box Previous node object, the created new node will be aligned
#' at the right bottom of the `prev_box`.
#' @param side Position of the side box, `left` or `right` side of the terminal box.
#' Will be aligned on the left and right side if only two groups, right otherwise. 
#' @inheritParams add_box
#'
#' @seealso \code{\link{add_box}},\code{\link{add_split}}
#' 
#' @return A \code{consort.list} or \code{consort} object.
#'
#' @export
#'
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
#' 
#' g <- add_split(g, txt = c("Arm A (n=100)", "Arm B (n=100"))
#' g <- add_side_box(g, 
#'                   txt = c("Excluded (n=15):\n
#'                   \u2022 MRI not collected (n=3)\n
#'                   \u2022 Tissues not collected (n=4)\n
#'                    \u2022 Other (n=8)", 
#'                    "Excluded (n=15):\n
#'                    \u2022 MRI not collected (n=3)\n
#'                    \u2022 Tissues not collected (n=4)"))
#' 
#' g <- add_box(g, txt = c("Final analysis (n=100)", "Final analysis (n=100"))
#' g <- add_label_box(g, txt = c("1" = "Screening", "3" = "Randomized", "4" = "Final analysis"))
#' 

add_side_box <- function(prev_box,
                         txt,
                         side = NULL,
                         dist = 0.02,
                         text_width = NULL){
  
  # Wrap text
  if(!is.null(text_width)){
    txt <- sapply(txt, function(tx){
      text_wrap(unlist(tx), width = text_width)
    })
  }
  
  if(!inherits(prev_box, c("gList", "consort")))
    stop("prev_box must be consort object")
  
  bx_lst <- Filter(is.textbox, prev_box)
  
  if(grepl("sidebox", bx_lst[[length(bx_lst)]]$name))
    stop("The last box added is a side box, can not add side box after a sidebox!")
  
  sp_layout <- attr(prev_box, "split_layout")
  
  if(is.null(sp_layout) & length(txt) > 1)
    stop("Text with length of 1 supplied for splitted diagram.")
  
  if(length(txt) > 1 | !is.null(sp_layout)){
    if(!is.null(sp_layout) & length(txt) != ncol(sp_layout))
      stop("The txt length must be same as splitted node number.")
  }
  
  if(!is.null(side) & length(side) != length(txt))
    stop("The length of side must have the same length with txt.")
  
  # One box on left, the other is right if only two groups given,
  # all will be on right side if not.
  if(is.null(side)){
    if(length(txt) == 2)
      side <- c("left", "right")
    else
      side <- rep("right", length(txt))
  }
  
  if(length(txt) > 1){

    blnk_txt <- sapply(txt, function(x){
      is.null(x) | x == "" | is.na(x)
    })

    # If all the text are blank
    if(all(blnk_txt))
      return(prev_box)
    
    # Get the length of grob before editing
    len_grobs <- length(prev_box)
    
    grb_lst <- lapply(seq_along(txt), function(i){
      get_prev_grobs(prev_box, col = i)
    })
    
    # If allocation split
    out_box <- vector("list", length = length(txt))
    
    # If more than one groups is given
    for(i in seq_along(txt)){
      
      # Any missing or blank.
      if(i %in% which(blnk_txt))
        next
      
      out_box[[i]] <- .add_side(prev_vert = grb_lst[[i]]$vert_grob,
                                txt = txt[i],
                                dist = dist,
                                side = side[i])
    }
    
    out_box <- align_hori(out_box) # Horizontal align
    
    # Connect
    for(i in seq_along(txt)){
      
      # Any missing or blank.
      if(i %in% which(blnk_txt))
        next
      
      connect_pos <- switch(side[i],
                            "right" = "bl",
                            "left"  = "br")
      
      connect <- connect_box(grb_lst[[i]]$vert_grob,
                             out_box[[i]],
                             connect = connect_pos,
                             type = "p")
      
      prev_box <- gList(prev_box, out_box[[i]], connect)
      
    }
    
    # Skip counting the connection grob
    spl <- rep(NA, length(txt))
    spl[!blnk_txt] <- len_grobs + seq(length.out = length(txt[!blnk_txt]), by = 2)
    
    split_layout <- matrix(spl, ncol = length(txt))
    row.names(split_layout) <- "sidebox"
    
    class(prev_box) <- union("consort", class(prev_box))
    
    structure(prev_box, 
              split_layout = rbind(sp_layout, split_layout))
      
    }else{
      prev_grob <- get_prev_grobs(prev_box)$vert_grob
      
      out_box <- .add_side(prev_grob, txt = txt, dist = dist, side = side)
        
      connect <- connect_box(prev_grob, out_box, connect = "bl", type = "p")

      prev_box <- gList(prev_box, out_box, connect)
      
      class(prev_box) <- union("consort", class(prev_box))
      
      structure(prev_box, 
                split_layout = NULL)
    
  }
}


#' Create box grob at the right/left bottom of the previous node
#'
#' @param side Position of the side box.
#' @inheritParams add_box
#' @keywords internal

.add_side <- function(prev_vert, 
                      txt, 
                      dist = 0.02, 
                      side = c("right", "left")){
  
  side <- match.arg(side)
  
  if(!is.unit(dist))
    dist <- unit(dist, "npc")
  
  # In case the text is a list
  txt <- unlist(txt)
  
  pre_cords <- get_coords(prev_vert)
  
  box <- textbox(txt, 
                 just = "left",
                 box_fn = rectGrob,
                 name = "sidebox")
  
  y_cords <- pre_cords$bottom - get_coords(box)$half_height - dist
  
  if(side == "right")
    x <- pre_cords$x + get_coords(box)$half_width + unit(6, "mm")
  else
    x <- pre_cords$x - get_coords(box)$half_width - unit(6, "mm")
  
  move_box(box, x = x, y = y_cords)
  
}
