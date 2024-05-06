

#' Self generating consort diagram
#'
#' Create CONSORT diagram from a participant disposition data.
#'
#' @param data Data set with disposition information for each participants.
#' @param orders A named vector or a list, names as the variable in the dataset
#' and values as labels in the box. The order of the diagram will be based on this.
#' A list can be used to report multiple variable in a single node, the first 
#' variable in a list element will be used to report the total and the exact items
#' will be summarised for the remaining variable. This is limitted to non-side box.
#' @param side_box Variable vector, appeared as side box in the diagram. The next
#'  box will be the subset of the missing values of these variables.
#' @param allocation Name of the grouping/treatment variable (optional), the
#'  diagram will split into branches on this variables forward. For a factorial 
#' design, with two splits for example, a character vector with a maximum of 
#' length two can be provided. The extra box will be skipped if the values
#'  in the \code{orders} blank.
#' @param labels Named vector, names is the location of the terminal node. The
#' position location should plus 1 after the allocation variables if the allocation
#' is defined.
#' @param kickoff_sidebox remove (default) the side box observations from the 
#' following counting.
#' @param cex Multiplier applied to font size, Default is 0.8
#' @param text_width a positive integer giving the target column for wrapping
#' lines in the output. String will not be wrapped if not defined (default).
#' The \code{\link[stringi]{stri_wrap}} function will be used if \code{stringi}
#' package installed, otherwise \code{\link[base]{strwrap}} will be used.
#'
#' @details
#' The calculation of numbers is as in an analogous to Kirchhoff's Laws of
#' electricity. The numbers in terminal nodes must sum to those in the ancestor
#'  nodes. All the drop outs will be populated as a side box. Which was different
#'   from the official CONSORT diagram template, which has dropout inside a
#'   vertical node.
#'
#' @return A \code{consort} object.
#'
#' @export
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}} \code{\link{textbox}}
#' @example inst/examples/consort-plot-example.R
#' @import grid
#' @importFrom stats na.omit
#'
consort_plot <- function(data,
                         orders,
                         side_box,
                         allocation = NULL,
                         labels = NULL,
                         kickoff_sidebox = TRUE,
                         cex = 0.8,
                         text_width = NULL) {
  options(txt_gp = gpar(cex = cex))
  on.exit(options(txt_gp = gpar()))
  
  data <- as.data.frame(data)

  if(!is.list(orders)){
    lst <- vector("list", length = length(orders))
    for(i in seq_along(orders)){
      lst[[i]] <- orders[i]
    }
    orders <- lst
  }

  # Number of variables in each orders.
  ord_len <- sapply(orders, length)

  # Make sure all the side box and allocation is single variable
  side_alloc <- sapply(orders, function(x){
    any(names(x) %in% side_box)
  })
  
  if(any(ord_len != 1 & side_alloc))
    stop("The sidebox must be single variable.")
  
  if(names(orders[[1]]) %in% allocation)
    stop("The first variable can not be a allocation variable.")

  # If all defined variables included in the orders
  if (!all(c(side_box, allocation) %in% names(unlist(orders)))) {
    not_in <- which(!c(side_box, allocation) %in% names(unlist(orders)))
    not_in <- c(side_box, allocation)[not_in]
    stop("Variable ", paste(not_in, collapse = ", "), " not included in the `orders`")
  }

  # If all the orders variables included in the dataset
  if (!all(names(unlist(orders)) %in% names(data))) {
    not_in <- which(!names(unlist(orders)) %in% names(data))
    not_in <- names(unlist(orders))[not_in]
    stop("Variable ", paste(not_in, collapse = ", "), " can not be found in the data")
  }
  
  if(!is.null(allocation)){
    if(length(allocation) > 2)
      stop("A maximum of two treatment allocation is supported.")
    
    for(i in allocation){
      if(!is.factor(data[[i]]))
        data[[i]] <- as.factor(data[[i]])
    }
    
  }
  
  split_ctn <- 0

  for (indx in seq_along(orders)) {
    
    i <- orders[[indx]]
    nd <- setNames(names(i), i)
    
    text_just <- ifelse(length(nd) > 1, "left", "center")

    if (indx == 1) {
      txt <- sapply(seq_along(nd), function(x){
        gen_text(data[[nd[x]]], label = names(nd[x]))
      })
      gp_list <- add_box(txt = txt, 
                         text_width = text_width)
      # data <- data[!is.na(data[[i]]), ]
    } else {
      
      val <- get_val(data, nd)

      if (any(nd %in% side_box)) {
        txt <- gen_text(x = unlst(val), 
                        label = names(nd),
                        bullet = TRUE)

        gp_list <- add_side_box(gp_list,
          txt = txt,
          text_width = text_width
        )

        if(kickoff_sidebox)
          data <- subset_missing(data, nd)
        
      } else if (any(nd %in% allocation)) {
        if(!nd[1] %in% allocation)
          stop("The first element must be in the allocation.")
        
        data <- subset_nonmissing(data, nd[1])
        
        split_ctn <- split_ctn + 1
        
        if(length(nd) > 1 & trimws(names(nd[1])) != "")
          stop("No allocation label is allowed for multiple variable split.")

        if(!trimws(names(nd[1])) == ""){
          txt1 <- gen_text(val[,1], label = names(nd[1]))
          gp_list <- add_box(gp_list,
                             txt = txt1,
                             text_width = text_width
          )
        }
        
        if(split_ctn == 2){
          txt2 <- lapply(val, function(x){
            make_text(value = x, varialbe = nd)
          })
        }else{
          txt2 <- make_text(value = val, varialbe = nd)
          # txt2 <- gen_text(val)
        }
        
        gp_list <- add_split(gp_list,
          txt = txt2,
          just = text_just,
          text_width = text_width
        )
        
        data <- split_data(data, nd[1])
        
      } else {
        # txt <- gen_text(x = val, label = orders[indx], bullet = FALSE)
        if(split_ctn == 2){
          txt <- lapply(val, function(x){
            r <- lapply(x, function(y){
              make_text(value = y, varialbe = nd, 
                        label = TRUE, split = FALSE)
            })
            unlist(r)
          })
          txt <- unlist(txt)
        }else if(split_ctn == 1){
          txt <- lapply(val, function(x){
            make_text(value = x, varialbe = nd, 
                      label = TRUE, split = FALSE)
          })
          txt <- unlist(txt)
        }else{
          txt <- make_text(value = val, varialbe = nd, 
                           label = TRUE, split = FALSE)
        }

        gp_list <- add_box(gp_list,
          txt = txt,
          just = text_just,
          text_width = text_width
        )
      }
    }
  }

  if (!is.null(labels)) {
    if (any(is.na(as.numeric(names(labels))))) {
      stop("Labels must be a named vector with names as the position of
           the node excluding side node.")
    }

    gp_list <- add_label_box(gp_list, txt = labels, only_terminal = TRUE)
  }

  return(gp_list)
}

# Make text for multiple variable
#' @keywords internal
make_text <- function(value, varialbe, label = FALSE, split = TRUE){
  
  if(!label | trimws(names(varialbe[1])) == ""){
    r1 <- gen_text(value[,1])
  }else{
    r1 <- gen_text(value[,1], label = names(varialbe[1]))
  }
  
  if(length(varialbe) > 1){
    r2 <- sapply(2:length(varialbe), function(j){
      y <- varialbe[j]
      if(split)
        gen_text(split(value[,y], value[,1]), label = names(y), bullet = TRUE)
      else
        gen_text(value[,y], label = names(y), bullet = TRUE)
    })
    paste(r1, r2, sep = "\n")
  }else{
    return(r1)
  }
}

# Flatten the list
#' @keywords internal
unlst <- function(lst){
  if (!any((inds <- sapply(lst, is.list)))) return(lst)
  c(lst[!inds], unlst(unlist(lst[inds], recursive = FALSE)))
}


# Split data for the next step
#' @keywords internal
split_data <- function(data, variable){
  if(!is.data.frame(data)){
    sapply(data, function(x){
      split(x, as.factor(x[[variable]]))
    }, simplify = FALSE)
  }else{
    split(data, as.factor(data[[variable]]))
  }
}


# Subset missing data for the next step
#' @keywords internal
subset_missing <- function(data, variable) {
  if (!is.data.frame(data)) {
    sapply(data, function(x){
      if(!is.data.frame(x))
        sapply(x, function(y) y[is.na(y[[variable]]), ],
               simplify = FALSE
        )
      else
        x[is.na(x[[variable]]), ]
    },simplify = FALSE)
  } else {
    data[is.na(data[[variable]]), ]
  }
}

# Subset non-missing data for the next step
#' @keywords internal
subset_nonmissing <- function(data, variable) {
  if (!is.data.frame(data)) {
    sapply(data, function(x){
      if(!is.data.frame(x))
        sapply(x, function(y) y[!is.na(y[[variable]]), ],
               simplify = FALSE
        )
      else
        x[!is.na(x[[variable]]), ]
    },simplify = FALSE)
  } else {
    data[!is.na(data[[variable]]), ]
  }
}

# Extract variable values
# Extract values of a variables form a `data.frame`, `list` or a nested `list`.
#' @keywords internal
get_val <- function(dat, variables){
  if (is.data.frame(dat)) {
    val <- dat[, variables, drop = FALSE]
  } else {
    val <- sapply(dat, function(x){
      if(is.data.frame(x))
        x[, variables, drop = FALSE]
      else{
        sapply(x, function(y) y[, variables, drop = FALSE], simplify = FALSE)
      }
    }, simplify = FALSE)
    
    # if(any(sapply(val, is.list)))
    #   val <- unlist(val, recursive = FALSE)
  }
  
  return(val)
}


