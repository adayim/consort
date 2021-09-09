

#' Self generating consort diagram
#' 
#' Create CONSORT diagram from a participant disposition data.
#'
#' @param data Data set with disposition information for each participants.
#' @param orders A named vector or a list, names as the variable in the dataset
#' and values as labels in the box. The order of the diagram will be based on this.
#' @param side_box Variable vector, appeared as side box in the diagram. The next
#'  box will be the subset of the missing values of these variables.
#' @param allocation Name of the grouping/treatment variable (optional), the
#'  diagram will split into branches on this variables forward.
#' @param labels Named vector, names is the location of the vertical node
#' excluding the side box. The position location should plus 1 after the allocation
#'  variables if the allocation is defined.
#' @param coords The horizontal coordinates of the boxes, see details.
#' @param dist Optional, distance between boxes. Default is 0.02.
#' @param cex Multiplier applied to font size, Default is 0.8
#'
#' @details
#' The calculation of numbers is as in an analogous to Kirchhoff's Laws of
#' electricity. The numbers in terminal nodes must sum to those in the ancestor
#'  nodes. All the drop outs will be populated as a side box. Which was different
#'   from the official CONSORT diagram template, which has dropout inside a
#'   vertical node.
#'   
#' @return A \code{consort.plot} object.
#'
#' @export
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}},\code{\link{build_consort}}
#'
#' @examples
#' ## Prepare test data
#' set.seed(1001)
#' N <- 300
#'
#' trialno <- sample(c(1000:2000), N)
#' exc1 <- rep(NA, N)
#' exc1[sample(1:N, 15)] <- sample(c("Sample not collected", "MRI not collected", "Other"),
#'                                15, 
#'                                replace = TRUE, prob = c(0.4, 0.4, 0.2))
#'
#' induc <- rep(NA, N)
#' induc[is.na(exc1)] <- trialno[is.na(exc1)]
#'
#' exc2 <- rep(NA, N)
#' exc2[sample(1:N, 20)] <- sample(c("Sample not collected", "Dead",
#'                                   "Other"), 20, replace = TRUE, 
#'                                   prob = c(0.4, 0.4, 0.2))
#' exc2[is.na(induc)] <- NA
#'
#' exc <- ifelse(is.na(exc2), exc1, exc2)
#'
#' arm <- rep(NA, N)
#' arm[is.na(exc)] <- sample(c("Conc", "Seq"), sum(is.na(exc)), replace = TRUE)
#' arm3 <- sample(c("Trt A", "Trt B", "Trt C"), N, replace = TRUE)
#' arm3[is.na(arm)] <- NA
#'
#' fow1 <- rep(NA, N)
#' fow1[!is.na(arm)] <- sample(c("Withdraw", "Discontinued", "Death", "Other", NA),
#'                             sum(!is.na(arm)), replace = TRUE,
#'                             prob = c(0.05, 0.05, 0.05, 0.05, 0.8))
#' fow2 <- rep(NA, N)
#' fow2[!is.na(arm) & is.na(fow1)] <- sample(c("Protocol deviation", "Outcome missing", NA),
#'                                           sum(!is.na(arm) & is.na(fow1)), replace = TRUE,
#'                                           prob = c(0.05, 0.05, 0.9))
#'
#'
#' df <- data.frame(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2)
#' rm(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2, N)
#'
#' ## Single arm
#' out <- consort_plot(data = df,
#' order = c(trialno = "Population",
#'           exc1    = "Excluded",
#'           arm     = "Allocated",
#'           fow1    = "Lost of Follow-up",
#'           trialno = "Finished Followup",
#'           fow2    = "Not evaluable for the final analysis",
#'           trialno = "Final Analysis"),
#' side_box = c("exc1", "fow1", "fow2"),
#' cex = 0.9)
#'
#' ## Two arms
#' out <- consort_plot(data = df,
#'              order = c(trialno = "Population",
#'                           exc    = "Excluded",
#'                           arm     = "Randomized patient",
#'                           fow1    = "Lost of Follow-up",
#'                           trialno = "Finished Followup",
#'                           fow2    = "Not evaluable",
#'                           trialno = "Final Analysis"),
#'              side_box = c("exc", "fow1", "fow2"),
#'              allocation = "arm",
#'              labels = c("1" = "Screening", "2" = "Randomization",
#'                         "5" = "Final"))
#' ## Three arms
#' consort_plot(data = df,
#'              order = c(trialno = "Population",
#'                           exc    = "Excluded",
#'                           arm3     = "Randomized patient",
#'                           fow1    = "Lost of Follow-up",
#'                           trialno = "Finished Followup",
#'                           fow2    = "Not evaluable",
#'                           trialno = "Final Analysis"),
#'              side_box = c("exc", "fow1", "fow2"),
#'              allocation = "arm3",
#'              labels = c("1" = "Screening", "2" = "Randomization",
#'                         "5" = "Final"))
#'
#' ## Multiple phase
#' consort_plot(data = df,
#'              order = list(trialno = "Population",
#'                           exc1    = "Excluded",
#'                           induc   = "Induction",
#'                           exc2    = "Excluded",
#'                           arm3     = "Randomized patient",
#'                           fow1    = "Lost of Follow-up",
#'                           trialno = "Finished Followup",
#'                           fow2    = "Not evaluable",
#'                           trialno = "Final Analysis"),
#'              side_box = c("exc1", "exc2", "fow1", "fow2"),
#'              allocation = "arm3",
#'              labels = c("1" = "Screening", "2" = "Month 4",
#'                         "3" = "Randomization", "5" = "Month 24",
#'                         "6" = "End of study"),
#'              dist = 0.02,
#'              cex = 0.7)
#'
#' 
#'
#' @import grid
#' @importFrom stats na.omit
consort_plot <- function(data,
                         orders,
                         side_box,
                         allocation = NULL,
                         labels = NULL,
                         coords = NULL,
                         dist = 0.02,
                         cex = 0.8){

  options(boxGrobTxt = grid::gpar(color = "black", cex = cex),
          boxGrob  = grid::gpar(color = "black", cex = cex),
          connectGrobArrow = grid::arrow(length = unit(0.1, "inches"),
                                   type = "closed"))

  if(is.list(orders))
    orders <- unlist(orders)

  # If all defined variables included in the orders
  if(!all(c(side_box, allocation) %in% names(orders))){
    not_in <- which(!c(side_box, allocation) %in% names(orders))
    not_in <- c(side_box, allocation)[not_in]
    stop("Variable ", paste(not_in, collapse = ", "), " not included in the `orders`")
  }

  # If all the orders variables included in the dataset
  if(!all(names(orders) %in% names(data))){
    not_in <- which(!names(orders) %in% names(data))
    not_in <- names(orders)[not_in]
    stop("Variable ", paste(not_in, collapse = ", "), " can not be found in the data")
  }

  if(!is.null(allocation) & length(allocation) > 1)
    stop("Only one treatment allocation supported")
  if(!is.null(allocation)){
    if(length(unique(stats::na.omit(data[[allocation]]))) < 2){
      warning("Single values in the allocation, will be ignored")
      allocation <- NULL
    }else{
      pos_arm <- which(allocation == names(orders))
      orders <- c(orders[c(1:pos_arm)], "split_data_variable" = "Group",
                  orders[(pos_arm + 1):length(orders)])

      data$split_data_variable <- data[[allocation]]
    }
  }

  gp_list <- vector(mode = "list", length = length(orders))

  for(indx in seq_along(orders)){
    i <- names(orders)[indx]

    if(indx == 1){
      txt <- paste0(orders[indx], " (n=", sum(!is.na(data[[i]])), ")")
      gp_list[[indx]] <- add_box(txt = txt, dist = dist)
      data <- data[!is.na(data[[i]]), ]
    }else{
      if(is.data.frame(data)){
        val <- data[[i]]
      }else{
        val <- sapply(data, function(x)x[[i]], simplify = FALSE)
      }
        
      if(i %in% side_box){
        txt <- box_text(x = val, label = orders[indx], sum_only = FALSE)
        gp_list[[indx]] <- add_side_box(gp_list[[indx-1]], txt = txt, dist = dist)
        data <- sub_data(data, i)

      }else if(i == "split_data_variable"){
        tab <- table(data[[i]])
        txt <- paste0(names(tab), " (n=", tab, ")")
        gp_list[[indx]] <- add_split(gp_list[[indx-1]],
                                     txt = txt, 
                                     dist = dist,
                                     coords = coords)
        data <- data[!is.na(data[[i]]), ]
        data <- split(data, as.factor(data[[i]]))

      }else{
        txt <- box_text(x = val, label = orders[indx], sum_only = TRUE)
        gp_list[[indx]] <- add_box(gp_list[[indx-1]], txt = txt, dist = dist)

      }
    }
  }

  if(!is.null(labels)){
    if(any(is.na(as.numeric(names(labels)))))
      stop("Labels must be a named vector with names as the position of
           the node excluding side node.")

    # Get the corresponding position in the orders
    side_pos <- which(names(orders) %in% side_box)
    lab_pos <- base::setdiff(seq_along(orders), side_pos)
    lab_pos <- lab_pos[as.numeric(names(labels))]
    lb_list <- vector(mode = "list", length = length(lab_pos))
    # labels <- labels[!labels %in% c(NA, "")]

    for(indx in seq_along(lab_pos)){
      i <- lab_pos[indx]
      lb_list[[indx]] <- add_label_box(gp_list[[i]],
                                       txt = labels[indx])
    }

  }else{
    lb_list <- NULL
  }

  # Remove the blank box
  gp_list <- gp_list[lengths(gp_list) != 0]

  gl <- build_consort(consort_list = gp_list, label_list   = lb_list)
  return(gl)
}



#' Add nodes
#'
#' Create/add vertically aligned labeled nodes or side nodes.
#' 
#'
#' @param prev_box Previous node object, the created new node will be vertically
#' aligned with this node. Left this as `NULL` if this is the first node. The first
#' node will be aligned in the top center.
#' @param txt Text in the node. If the `prev_box` is a horizontally aligned multiple
#' nodes, a vector of with the same length must be provided.
#' @param dist Distance between previous node, including the distance between the
#' side node.
#' 
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}}
#' @return A \code{consort.list} or \code{consort} object.
#' 
#' @export
#'
#' @importFrom Gmisc connectGrob coords moveBox
#'
#' @examples
#' txt1 <- "Population (n=300)"
#' txt1_side <- "Excluded (n=15): \n
#'               \u2022 MRI not collected (n=3)\n
#'               \u2022 Tissues not collected (n=4)\n
#'               \u2022 Other (n=8)"
#' 
#' node1 <- add_box(txt = txt1)
#' 
#' node3 <- add_side_box(node1, txt = txt1_side)    
#' 
#' node4 <- add_box(node3, txt = "Randomized (n=200)")
#' 
#' node1_sp <- add_split(node4, txt = c("Arm A (n=100)", "Arm B (n=100"))
#' side1_sp <- add_side_box(node1_sp, 
#'                          txt = c("Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)\n
#'                          \u2022 Other (n=8)", 
#'                          "Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)"))
#' 
#' node2_sp <- add_box(side1_sp, 
#'                     txt = c("Final analysis (n=100)",
#'                              "Final analysis (n=100"))
#' 
#' node1
#' node3
#' node4
#' node1_sp
#' side1_sp
#' node2_sp
#'
add_box <- function(prev_box = NULL, txt, just = "center", dist = 0.02){

  if(!is.null(prev_box)){

    if(!inherits(prev_box, c("consort.list", "consort")))
      stop("ref_box must be consort.list or consort object")

    if(inherits(prev_box, "consort.list") & length(txt) != length(prev_box))
      stop("The previous node must be a split box if multiple txt defined")
      
    # No allocation split
    if(length(txt) == 1){
      out_box <- .add_box(prev_box, txt = txt, just = just, dist = dist)
      class(out_box) <- union("consort", class(out_box))
    
    # If allocation split
    }else{
     
      out_box <- lapply(seq_along(txt), function(i).add_box(prev_box[[i]],
                                                            txt = txt[i],
                                                            just = just,
                                                            dist = dist))
      out_box <- align_hori(out_box) # Horizontal align
      # Re-connect
      for(i in seq_along(txt)){
        if(attr(prev_box[[i]], "type") == "side_box"){
          vert_box <- attr(prev_box[[i]], "prev_box")
        }else{
          vert_box <- prev_box[[i]]
        }
          
        connect <- Gmisc::connectGrob(vert_box, out_box[[i]], type = "vert")
        attr(out_box[[i]], "connect") <- connect
        attr(out_box[[i]], "prev_box") <- prev_box[[i]]
        
      }

      class(out_box)<- union("consort.list", class(out_box))

    }

    return(out_box)

  }else{
    out_box <- boxGrob(txt, x = 0.5, y = 0.9, box_fn = rectGrob)
    class(out_box) <- union("consort", class(out_box))
    structure(out_box,
              connect =  NULL,
              type = "box")
  }

}


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
#' node1 <- add_box(txt = txt1)
#' 
#' node3 <- add_side_box(node1, txt = txt1_side)    
#' 
#' node4 <- add_box(node3, txt = "Randomized (n=200)")
#' 
#' node1_sp <- add_split(node4, txt = c("Arm A (n=100)", "Arm B (n=100"))
#' side1_sp <- add_side_box(node1_sp, 
#'                          txt = c("Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)\n
#'                          \u2022 Other (n=8)", 
#'                          "Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)"))
#' 
#' node2_sp <- add_box(side1_sp, 
#'                     txt = c("Final analysis (n=100)",
#'                              "Final analysis (n=100")) 
#' node1
#' node3
#' node4
#' node1_sp
#' side1_sp
#' node2_sp
#' 

add_side_box <- function(prev_box, txt, side = NULL, dist = 0.02){

  if(!inherits(prev_box, c("consort.list", "consort")))
    stop("ref_box must be consort.list or consort object")

  if(inherits(prev_box, "consort.list") & length(txt) != length(prev_box))
    stop("The previous node must be a split box if multiple txt defined")
  
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

    # If more than one groups is given
    out_box <- lapply(seq_along(txt), function(i).add_side(prev_box[[i]],
                                                           txt = txt[i],
                                                           dist = dist,
                                                           side = side[i]))
    out_box <- align_hori(out_box) # Horizontal align

    # Re-connect
    for(i in seq_along(txt)){
      if(length(out_box[[i]]) == 0){
        connect <- NULL
      }else{
        connect <- Gmisc::connectGrob(prev_box[[i]], out_box[[i]], type = "L")
      }
      
      attr(out_box[[i]], "connect") <- connect
      attr(out_box[[i]], "prev_box") <- prev_box[[i]]
    }

    class(out_box) <- union("consort.list", class(out_box))

  }else{
    out_box <- .add_side(prev_box, txt = txt, dist = dist, side = side)
    class(out_box) <- union("consort", class(out_box))

  }

  return(out_box)
}


#' Add a splitting box
#'
#' This function will create a horizontally aligned nodes. The horizontal coordinate
#' will be automatically calculated if the coordinates not provided.
#'
#' @param prev_box Previous node that the newly created split box will be aligned.
#' @param txt A vector of text labels for each nodes.
#' @param coords The horizontal coordinates of the boxes, see details.
#' @inheritParams add_box
#' 
#'
#' @details
#' The `coords` will be used to set the horizontal coordinates of the nodes. The
#' `coords` should be within 0 and 1 to avoid the nodes is aligned outside of the
#'  final figure. If the `coords` is `NULL`, not given. The function will calculate
#'  the `coords`. If the the length of the `txt` is two, then a coordinates of
#'  0.35 and 0.65 will be used. Once the split box is added, all the following nodes
#'  will be split accordingly.
#'
#' @seealso \code{\link{add_box}}, \code{\link{add_side_box}}
#' 
#' @return A \code{consort.list} object.
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
#' node1 <- add_box(txt = txt1)
#' 
#' node3 <- add_side_box(node1, txt = txt1_side)    
#' 
#' node4 <- add_box(node3, txt = "Randomized (n=200)")
#' 
#' node1_sp <- add_split(node4, txt = c("Arm A (n=100)", "Arm B (n=100"))
#' side1_sp <- add_side_box(node1_sp, 
#'                          txt = c("Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)\n
#'                          \u2022 Other (n=8)", 
#'                          "Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)"))
#' 
#' node2_sp <- add_box(side1_sp, 
#'                     txt = c("Final analysis (n=100)",
#'                              "Final analysis (n=100"))#' 
#' node1
#' node3
#' node4
#' node1_sp
#' side1_sp
#' node2_sp
#' 

#'
add_split <- function(prev_box, txt, coords = NULL, dist = 0.02){

  if(inherits(prev_box, "consort.list"))
    stop("Nested splits are not supported in the current version")

  if(inherits(prev_box, "consort.list") & length(prev_box) != 1)
    stop("The package does not support multiple split.")

  if(length(txt) == 1)
    stop("The length of txt should be larger than 1, please use add_box instead.")

  # Define coordinates for the splits
  if(is.null(coords)){
    n_gp <- length(txt)
    if(n_gp == 2){
      x_coords <- c(0.35, 0.65)
    }else{
      x_coords <- 0.5/n_gp
      x_coords <- c(x_coords, rep(2*x_coords, times = n_gp - 1))
      x_coords <- cumsum(x_coords)
    }
  }else{
    if(length(coords) != length(txt))
      stop("The length of the coords should match the length of txt.")

    if(any(coords > 1 | coords < 0))
      warning("The coordinates are not within 0 and 1 and might be cropped in the final output.")

    x_coords <- coords
  }

  .add_split <- function(prev_box, txt, x, dist){
    pre_cords <- Gmisc::coords(prev_box)

    out_box <- boxGrob(txt, x = x, box_fn = rectGrob)
    y_cords <- pre_cords$bottom - Gmisc::coords(out_box)$half_height - unit(dist+0.02, "npc")
    out_box <- Gmisc::moveBox(out_box, y = y_cords)
    connect <- Gmisc::connectGrob(prev_box, out_box, type = "N")

    class(out_box) <- union("consort", class(out_box))

    structure(out_box,
              connect =  connect,
              type = "box")
  }

  out_box <- lapply(seq_along(txt), function(i).add_split(prev_box,
                                                          txt[i],
                                                          x_coords[i],
                                                          dist))
  class(out_box) <- union("consort.list", class(out_box))
  return(out_box)
}

#' Add a vertically aligned label nodes on the left side.
#'
#' In a consort diagram, this can be used to indicate different stage.
#'
#' @param ref_box Reference node to which the label will be horizontally aligned.
#' @param txt Text in the node.
#'
#' @export
#' 
#' @return A \code{consort} object.
#'
#' @importFrom Gmisc boxGrob
#'
#' @examples
#' txt1 <- "Population (n=300)"
#' txt1_side <- "Excluded (n=15): \n
#'               \u2022 MRI not collected (n=3)\n
#'               \u2022 Tissues not collected (n=4)\n
#'               \u2022 Other (n=8)"
#' 
#' node1 <- add_box(txt = txt1)
#' 
#' node3 <- add_side_box(node1, txt = txt1_side)    
#' 
#' node4 <- add_box(node3, txt = "Randomized (n=200)")
#' 
#' node1_sp <- add_split(node4, txt = c("Arm A (n=100)", "Arm B (n=100"))
#' side1_sp <- add_side_box(node1_sp, 
#'                          txt = c("Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)\n
#'                          \u2022 Other (n=8)", 
#'                          "Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)"))
#' 
#' node2_sp <- add_box(side1_sp, 
#'                     txt = c("Final analysis (n=100)",
#'                              "Final analysis (n=100"))#' 
#' lab1 <- add_label_box(node1, txt = "Screening")
#' lab2 <- add_label_box(node4, txt = "Randomized")
#' lab3 <- add_label_box(node2_sp, txt = "Final analysis")
#' build_consort(list(node1, node3, node4, node1_sp, side1_sp, node2_sp),
#'               list(lab1, lab2, lab3))
#'               


add_label_box <- function(ref_box,
                          txt){

  if(length(txt) > 1)
    stop("txt must of length one!")
  if(!inherits(ref_box, c("consort.list", "consort")))
    stop("ref_box must be consort.list or consort object")

  if(inherits(ref_box, "consort.list"))
    ref_box <- ref_box[[1]]

  ref_pos <- Gmisc::coords(ref_box)

  out_box <- boxGrob(label = txt,
                     txt_gp = gpar(col = "#4F81BD", cex = getOption("boxGrobTxt",
                                                                    default = gpar(cex = 1))$cex,
                                   fontface = "bold"),
                     box_gp = gpar(fill = "#A9C7FD"),
                     box_fn = roundrectGrob)

  # Align with the reference box
  box_pos <- Gmisc::coords(out_box)
  out_box <- Gmisc::moveBox(out_box,
                            x = unit(0.2, "npc") + box_pos$half_width,
                            y = ref_pos$top - box_pos$half_height)

  class(out_box) <- union("consort", class(out_box))

  return(out_box)
}


#' Build consort diagram
#'
#' @param consort_list A list of nodes.
#' @param label_list A list of label nodes.
#'
#' @export
#' 
#' @return A \code{consort.plot} object.
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}}
#'
#' @examples
#' txt1 <- "Population (n=300)"
#' txt1_side <- "Excluded (n=15): \n
#'               \u2022 MRI not collected (n=3)\n
#'               \u2022 Tissues not collected (n=4)\n
#'               \u2022 Other (n=8)"
#' 
#' node1 <- add_box(txt = txt1)
#' 
#' node3 <- add_side_box(node1, txt = txt1_side)    
#' 
#' node4 <- add_box(node3, txt = "Randomized (n=200)")
#' 
#' node1_sp <- add_split(node4, txt = c("Arm A (n=100)", "Arm B (n=100"))
#' side1_sp <- add_side_box(node1_sp, 
#'                          txt = c("Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)\n
#'                          \u2022 Other (n=8)", 
#'                          "Excluded (n=15):\n
#'                          \u2022 MRI not collected (n=3)\n
#'                          \u2022 Tissues not collected (n=4)"))
#' 
#' node2_sp <- add_box(side1_sp, 
#'                     txt = c("Final analysis (n=100)",
#'                              "Final analysis (n=100"))
#'                              
#' lab1 <- add_label_box(node1, txt = "Screening")
#' lab2 <- add_label_box(node4, txt = "Randomized")
#' lab3 <- add_label_box(node2_sp, txt = "Final analysis")
#' build_consort(list(node1, node3, node4, node1_sp, side1_sp, node2_sp),
#'               list(lab1, lab2, lab3))
#'
#'
build_consort <- function(consort_list, label_list = NULL){

  stopifnot(is.list(consort_list))

  # Remove any blank box
  consort_list <- consort_list[lengths(consort_list) != 0]

  gl <- list(consort_list = consort_list,
             label_list   = label_list)
  class(gl) <- union("consort.plot", class(gl))

  return(gl)

}


# Subset missing data for the next step
#' @keywords internal
sub_data <- function(data, var){
  if(!is.data.frame(data))
    sapply(data, function(x)x[is.na(x[[var]]), ],
           simplify = FALSE)
  else
    data[is.na(data[[var]]), ]
}

#' Generate label and bullet points
#' 
#' This function use the data to generate label and bullet points for the box.
#'
#' @param x A list or a vector to be used.
#' @param label A character string as a label at the beginning of the text label. 
#' @param sum_only If shows total only (default). If the value is `FALSE`, the
#' values will be tabulated with bullet points.
#'
#' @return A character string of vector.
#' @export
#'
#' @examples
#' val <- data.frame(am = factor(ifelse(mtcars$am == 1, "Automatic", "Manual")),
#'                  car = row.names(mtcars))
#'  
#'  box_text(val$car, label = "Cars in the data")
#'  box_text(val$car, label = "Cars in the data", sum_only = FALSE)
#'  box_text(split(val$car, val$am), label = "Cars in the data")
#'  box_text(split(val$car, val$am), label = "Cars in the data", sum_only = FALSE)
#' 
#' 
box_text <- function(x, label, sum_only = TRUE){
  if(is.list(x)){
    sapply(x, function(val){
      box_label(x = val, label = label, sum_only = sum_only)
    }, simplify = TRUE)
  }else{
    box_label(x = x, label = label, sum_only = sum_only)      
  }
}

# Calculate the numbers in the box use the data provided.
#' @keywords internal
box_label <- function(x, label, sum_only = TRUE){
  
  # Blank as NA
  if(is.character(x))
    x[x == ""] <- NA
  
  # Return NULL if no values and it is not for sum
  if(sum(!is.na(x)) == 0 & !sum_only)
    return(NULL)
  
  tp <- paste0(label, " (n=", sum(!is.na(x)), ")")
  if(!sum_only){
    txt_sub <- paste0("\u2022 ", names(table(x)), " (n=", table(x), ")")
    tp <- paste0(tp, ":\n", paste(txt_sub, collapse = "\n"))
  }
  
  return(tp)
  
}


# Align grobs horizontally
#' @keywords internal
align_hori <- function(boxlist) {

  # Find the lowest box, and set as reference
  y_val <- sapply(boxlist, function(x){
    if(length(x) == 0){
      return(NA)
    }else{
      convertUnit(Gmisc::coords(x)$y, unitTo = "npc", valueOnly = TRUE)
    }
  })

  y_val <- which.min(y_val)
  y_oth <- base::setdiff(seq_along(boxlist), y_val)

  ref_positions <- Gmisc::coords(boxlist[[y_val]])

  # Align other boxes
  boxlist[y_oth] <- lapply(boxlist[y_oth],
                           FUN = function(box, ref_pos) {
                             if(length(box) == 0){
                               return(structure(list(),
                                       type =  attr(box, "type")))
                             }else{
                               box_pos <- Gmisc::coords(box)
                               new_y <- ref_pos$top - box_pos$half_height
                               out_box <- Gmisc::moveBox(box, y = new_y)

                               class(out_box) <- union("consort", class(out_box))

                               structure(out_box,
                                         type =  attr(box, "type"))
                             }

                           },
                           ref_pos = ref_positions)

  return(boxlist)

}



#' Create node vertically align with the previous one
#'
#' @inheritParams add_box
#' @keywords internal
.add_box <- function(prev_box, txt, just = "center", dist = 0.02){

  # Incase the txt is a list
  txt <- unlist(txt)

  # If the current box is blank
  if(length(prev_box) == 0)
    prev_box <- attr(prev_box, "prev_box")

  # If previous box is not a side box
  if(attr(prev_box, "type") == "side_box"){
    vert_box <- attr(prev_box, "prev_box")
  }else{
    vert_box <- prev_box
    dist <- 4*dist # Add more distance
  }
    

  pre_cords <- Gmisc::coords(prev_box)

  box <- Gmisc::boxGrob(txt, just = just, box_fn = rectGrob)
  y_cords <- pre_cords$bottom - Gmisc::coords(box)$half_height - unit(dist, "npc")
  x <- Gmisc::coords(vert_box)$x

  out_box <- Gmisc::moveBox(box, x = x, y = y_cords)
  connect <- Gmisc::connectGrob(vert_box, out_box, type = "vert")

  class(out_box) <- union("consort", class(out_box))

  structure(out_box,
            connect =  connect,
            prev_box = prev_box,
            type = "box")
}

#' Create box grob at the right/left bottom of the previous node
#'
#' @param side Position of the side box.
#' @inheritParams add_box
#' @keywords internal

.add_side <- function(prev_box, txt, dist = 0.02, side = c("right", "left")){

  side <- match.arg(side)

  # Incase the txt is a list
  txt <- unlist(txt)

  if(is.null(txt))
    return(structure(list(),
              connect  =  NULL,
              prev_box =  prev_box,
              type     = "side_box"))

  pre_cords <- Gmisc::coords(prev_box)

  # Define the name of the side box
  bx_name <- switch(side,
                    "right" = "right_side_box",
                    "left"  = "left_side_box")

  box <- Gmisc::boxGrob(txt, just = "left", name = bx_name, box_fn = rectGrob)
  y_cords <- pre_cords$bottom - Gmisc::coords(box)$half_height - unit(dist, "npc")

  if(side == "right")
    x <- pre_cords$x + Gmisc::coords(box)$half_width + unit(6, "mm")
  else
    x <- pre_cords$x - Gmisc::coords(box)$half_width - unit(6, "mm")

  out_box <- Gmisc::moveBox(box, x = x, y = y_cords)

  connect <- Gmisc::connectGrob(prev_box, out_box, type = "L")

  class(out_box) <- union("consort", class(out_box))

  structure(out_box,
            connect  =  connect,
            prev_box =  prev_box,
            type     = "side_box")
}
