

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
#'  diagram will split into branches on this variables forward. For a factorial 
#' design, with two splits for example, a character vector with a maximum of 
#' length two can be provided. The extra box will be skipped if the values
#'  in the \code{orders} blank.
#' @param labels Named vector, names is the location of the terminal node. The
#' position location should plus 1 after the allocation variables if the allocation
#' is defined.
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
                         cex = 0.8,
                         text_width = NULL) {
  options(txt_gp = gpar(cex = cex))
  on.exit(options(txt_gp = gpar()))

  if (is.list(orders)) {
    orders <- unlist(orders)
  }
  
  if(names(orders[1]) %in% allocation)
    stop("The first variable can not be a allocation variable.")

  # If all defined variables included in the orders
  if (!all(c(side_box, allocation) %in% names(orders))) {
    not_in <- which(!c(side_box, allocation) %in% names(orders))
    not_in <- c(side_box, allocation)[not_in]
    stop("Variable ", paste(not_in, collapse = ", "), " not included in the `orders`")
  }

  # If all the orders variables included in the dataset
  if (!all(names(orders) %in% names(data))) {
    not_in <- which(!names(orders) %in% names(data))
    not_in <- names(orders)[not_in]
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

  for (indx in seq_along(orders)) {
    i <- names(orders)[indx]

    if (indx == 1) {
      gp_list <- add_box(txt = gen_text(data[[i]], label = orders[indx]), 
                         text_width = text_width)
      data <- data[!is.na(data[[i]]), ]
    } else {
      val <- get_val(data, i)

      if (i %in% side_box) {
        txt <- gen_text(x = val, label = orders[indx], bullet = TRUE)

        gp_list <- add_side_box(gp_list,
          txt = txt,
          text_width = text_width
        )

        data <- subset_missing(data, i)
        
      } else if (i %in% allocation) {
        
        if(!trimws(orders[indx]) == ""){
          txt1 <- gen_text(val, label = orders[names(orders) %in% i])
          gp_list <- add_box(gp_list,
                             txt = txt1,
                             text_width = text_width
          )
        }
        
        if(is.list(val)){
          txt2 <- lapply(val, gen_text)
        }else{
          txt2 <- gen_text(val)
        }
        
        gp_list <- add_split(gp_list,
          txt = txt2,
          text_width = text_width
        )
        
        data <- subset_nonmissing(data, i)

        data <- split_data(data, i)
        
      } else {
        txt <- gen_text(x = val, label = orders[indx], bullet = FALSE)

        gp_list <- add_box(gp_list,
          txt = txt,
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
get_val <- function(dat, variable){
  if (is.data.frame(dat)) {
    val <- dat[[variable]]
  } else {
    val <- sapply(dat, function(x){
      if(is.data.frame(x))
        x[[variable]]
      else{
        sapply(x, function(y) y[[variable]], simplify = FALSE)
      }
    }, simplify = FALSE)
    
    if(any(sapply(val, is.list)))
      val <- unlist(val, recursive = FALSE)
  }
  
  return(val)
}


