

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
#' @param coords The horizontal coordinates of the boxes, see \link{add_split}.
#' @param dist Optional, distance between boxes. Default is 0.02.
#' @param cex Multiplier applied to font size, Default is 0.8
#' @param text_width a positive integer giving the target column for wrapping 
#' lines in the output. String will not be wrapped if not defined (default).
#' The \code{\link[stringi]{stri_wrap}} function will be used if \code{stringi}
#' package installed, otherwise \code{\link[base]{strwrap}} will be used.
#' @param widths A numeric vector of length 2 specifying relative percentage 
#' of the label and diagram in the final gprah.
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
#' \code{\link{add_side_box}}
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
#' @import grid
#' @importFrom stats na.omit
#' 
consort_plot <- function(data,
                         orders,
                         side_box,
                         allocation = NULL,
                         labels = NULL,
                         coords = NULL,
                         dist = 0.02,
                         cex = 0.8,
                         text_width = NULL,
                         widths = c(0.1, 0.9)){
	
  options(txt_gp = gpar(cex = cex))
  on.exit(options(txt_gp = gpar()))

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

  # gp_list <- vector(mode = "list", length = length(orders))

  for(indx in seq_along(orders)){
    i <- names(orders)[indx]

    if(indx == 1){
      txt <- paste0(orders[indx], " (n=", sum(!is.na(data[[i]])), ")")
      gp_list <- add_box(txt = txt, dist = dist, text_width = text_width)
      data <- data[!is.na(data[[i]]), ]
    }else{
      if(is.data.frame(data)){
        val <- data[[i]]
      }else{
        val <- sapply(data, function(x)x[[i]], simplify = FALSE)
      }
        
      if(i %in% side_box){
        
        txt <- box_text(x = val, label = orders[indx], bullet = TRUE)
        
        gp_list <- add_side_box(gp_list,
                                txt = txt,
                                dist = dist,
                                text_width = text_width)
        
        data <- sub_data(data, i)

      }else if(i == "split_data_variable"){
        txt <- box_text(data[[i]])
        gp_list <- add_split(gp_list,
                             txt = txt, 
                             dist = dist,
                             coords = coords,
                             text_width = text_width)
        
        data <- data[!is.na(data[[i]]), ]
        data <- split(data, as.factor(data[[i]]))

      }else{
        txt <- box_text(x = val, label = orders[indx], bullet = FALSE)
        
        gp_list <- add_box(gp_list, 
                           txt = txt,
                           dist = dist,
                           text_width = text_width)

      }
    }
  }

  if(!is.null(labels)){
    if(any(is.na(as.numeric(names(labels)))))
      stop("Labels must be a named vector with names as the position of
           the node excluding side node.")

    gp_list <- add_label_box(gp_list, txt = labels, only_terminal = TRUE, widths = widths)
 
  }

  class(gp_list) <- union("consort", class(gp_list))
  
  return(gp_list)
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




