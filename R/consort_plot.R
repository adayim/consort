

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

  if (!is.null(allocation) & length(allocation) > 1) {
    stop("Only one treatment allocation supported")
  }
  if (!is.null(allocation)) {
    if (length(unique(stats::na.omit(data[[allocation]]))) < 2) {
      warning("Single values in the allocation, will be ignored")
      allocation <- NULL
    } else {
      pos_arm <- which(allocation == names(orders))

      if(pos_arm == length(orders)){
        orders <- c(orders[c(1:pos_arm)])
      }else {
        orders <- c(orders[c(1:pos_arm)],
                    "split_data_variable" = "Group",
                    orders[(pos_arm + 1):length(orders)])
      }
      

      data$split_data_variable <- data[[allocation]]
    }
  }

  # gp_list <- vector(mode = "list", length = length(orders))

  for (indx in seq_along(orders)) {
    i <- names(orders)[indx]

    if (indx == 1) {
      txt <- paste0(orders[indx], " (n=", pret_num(sum(!is.na(data[[i]]))), ")")
      gp_list <- add_box(txt = txt, text_width = text_width)
      data <- data[!is.na(data[[i]]), ]
    } else {
      if (is.data.frame(data)) {
        val <- data[[i]]
      } else {
        val <- sapply(data, function(x) x[[i]], simplify = FALSE)
      }

      if (i %in% side_box) {
        txt <- gen_text(x = val, label = orders[indx], bullet = TRUE)

        gp_list <- add_side_box(gp_list,
          txt = txt,
          text_width = text_width
        )

        data <- sub_data(data, i)
      } else if (i == "split_data_variable") {
        txt <- gen_text(data[[i]])
        gp_list <- add_split(gp_list,
          txt = txt,
          text_width = text_width
        )

        data <- data[!is.na(data[[i]]), ]
        data <- split(data, as.factor(data[[i]]))
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



# Subset missing data for the next step
#' @keywords internal
sub_data <- function(data, var) {
  if (!is.data.frame(data)) {
    sapply(data, function(x) x[is.na(x[[var]]), ],
      simplify = FALSE
    )
  } else {
    data[is.na(data[[var]]), ]
  }
}
