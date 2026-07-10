
#' Generate label and bullet points
#'
#' This function use the data to generate label and bullet points for the box.
#'
#' @param x A list or a vector to be used. \code{x} can be atomic vector, a 
#' \code{data.frame} or a \code{list}. A \code{data.frame} is particular useful
#'  if the there's a nested reason or a \code{list} split nested reasons by group.
#' The nested reasons only support two columns and the \code{bullet} will be ignored.
#' @param label A character string as a label at the beginning of the text label.
#' The count for each categories will be returned if no label is provided.
#' @param bullet If shows bullet points. If the value is \code{TRUE}, the bullet points
#' will be tabulated, default is \code{FALSE}.
#' @param bullet_char A single character used as the bullet symbol. Defaults to
#'   \code{consort_opt("bullet")}. Can be any Unicode
#'   character such as \code{"\u2013"} (en-dash), \code{"\u25CB"} (circle),
#'   \code{"\u25A0"} (square), \code{"-"}, etc.
#' @param drop_levels If \code{TRUE} (default), unused factor levels are
#'   dropped before tabulation, so categories with zero counts are omitted.
#'   Set to \code{FALSE} to report zero-count factor levels as \code{(n=0)},
#'   e.g. to display an exclusion criterion that was applied but excluded
#'   nobody. Only applies when \code{x} (or the reason column) is a factor.
#'
#' @return A character string of vector.
#' @export
#'
#' @rdname gen_text
#'
#' @examples
#' val <- data.frame(
#'   am = factor(ifelse(mtcars$am == 1, "Automatic", "Manual"), ordered = TRUE),
#'   vs = factor(ifelse(mtcars$vs == 1, "Straight", "V-shaped"), ordered = TRUE),
#'   car = row.names(mtcars)
#' )
#'
#' gen_text(val$car, label = "Cars in the data")
#' gen_text(val$car, label = "Cars in the data", bullet = FALSE)
#' gen_text(split(val$car, val$am), label = "Cars in the data")
#' gen_text(split(val$car, val$am), label = "Cars in the data", bullet = FALSE)
#' gen_text(split(val[,c("vs", "car")], val$am), label = "Cars in the data", bullet = FALSE)
#' gen_text(val[,c("vs", "car")], label = "Cars in the data", bullet = FALSE)
#'
#' # Use a custom bullet character
#' gen_text(val$car, label = "Cars in the data", bullet = TRUE, bullet_char = "-")
#'
#' # Or set globally via set_consort_defaults
#' set_consort_defaults(bullet = "\u25CB")
#' gen_text(val$car, label = "Cars in the data", bullet = TRUE)
#'
#' # Report zero-count factor levels with (n=0)
#' reason <- factor(c("Ineligible", NA, "Declined", NA),
#'                  levels = c("Ineligible", "Declined", "Other"))
#' gen_text(reason, label = "Excluded", bullet = TRUE)
#' gen_text(reason, label = "Excluded", bullet = TRUE, drop_levels = FALSE)
gen_text <- function(x, label = NULL, bullet = FALSE,
                     bullet_char = consort_opt("bullet"),
                     drop_levels = TRUE) {

  if(!is.null(label) & length(label)>1)
    stop("label must be of length 1")

  if (!is.logical(drop_levels) || length(drop_levels) != 1 || is.na(drop_levels))
    stop("`drop_levels` must be a single TRUE/FALSE value.")

  if(is.data.frame(x) && ncol(x) == 1)
    x <- unlist(x)

  if (is.list(x)) {
    if(is.data.frame(x)){
      box_data.frame(x, label = label, bullet_char = bullet_char,
                     drop_levels = drop_levels)
    }else {
      # Calculate numers in each split
      sp_num <- sapply(x, function(i){
        if(is.data.frame(i))
          nrow(i)
        else
          length(i)
      })
      
      if(!is.null(label))
        label <- rep(label, length(x))
      
      sapply(seq_along(x), function(indx) {
        val <- x[[indx]]
        # If the list contains a data.frame
        if(is.data.frame(val)){
          r <- box_data.frame(val, bullet_char = bullet_char,
                              drop_levels = drop_levels)
          if(!is.null(label)){
            lab_lst <- sprintf("%s (n=%s)", 
                               label[indx], 
                               pret_num(sum(!is.na(val[[1]]))))
            r <- paste(lab_lst, r, sep = "\n")
          }
          
        }else{
          r <- box_label(val, label = label[indx], bullet = bullet,
                         bullet_char = bullet_char,
                         drop_levels = drop_levels)
          r <- paste(r, collapse = "\n")
        }
        
        return(r)
        
      }, simplify = TRUE)
    }
    
  } else {
    box_label(x = x, label = label, bullet = bullet, bullet_char = bullet_char,
              drop_levels = drop_levels)
  }
}

# Calculate the numbers in the box use the data provided.
#' @keywords internal
box_data.frame <- function(x, label = NULL,
                           bullet_char = consort_opt("bullet"),
                           drop_levels = TRUE){
  if(ncol(x) != 2)
    stop("only two columns are supported")

  if(!is.null(label))
    label <- sprintf("%s (n=%s)", label, pret_num(sum(!is.na(x[[1]]))))

  if (!drop_levels && is.factor(x[[1]])) {
    groups <- levels(x[[1]])
  } else {
    groups <- na.omit(unique(x[[1]]))
  }

  r <- sapply(groups, function(i){
    val <- x[[2]][x[[1]] %in% i]
    # Zero-count group kept by `drop_levels = FALSE`: report the count only
    if (!drop_levels && sum(!is.na(val)) == 0)
      return(sprintf("%s (n=0)", i))
    box_label(val, label = i, bullet = TRUE,
              bullet_char = bullet_char, drop_levels = drop_levels)
  })
  
  r <- paste(r, collapse = "\n")
  
  if(!is.null(label))
    r <- paste(label, r, sep = "\n")
  
  return(r)
}


# Calculate the numbers in the box
#' @keywords internal
box_label <- function(x, label, bullet = TRUE,
                      bullet_char = consort_opt("bullet"),
                      drop_levels = TRUE) {

  # Blank as NA
  if (is.character(x)) {
    x[x == ""] <- NA
  }

  # Return blank if no values and with bullet, unless zero-count factor
  # levels should be reported
  if (sum(!is.na(x)) == 0 & bullet) {
    if (drop_levels || !is.factor(x) || nlevels(x) == 0)
      return("")
  }

  # Tabulate, keeping zero-count factor levels if requested
  tab_fn <- function(x) {
    if (is.factor(x) && drop_levels)
      table(droplevels(x))
    else
      table(x)
  }

  if (is.null(label)) {
    tab <- tab_fn(x)

    tp <- paste0(names(tab), " (n=", pret_num(tab), ")")
    if(!bullet){
      return(tp)
    }else{
      return(paste0(bullet_char, " ", paste(tp, collapse = paste0("\n", bullet_char, " "))))
    }

  }

  tp <- paste0(label, " (n=", pret_num(sum(!is.na(x))), ")")

  if (bullet) {
    tab <- tab_fn(x)
    txt_sub <- paste0(bullet_char, " ", names(tab), " (n=", pret_num(tab), ")")
    tp <- paste0(tp, "\n", paste(txt_sub, collapse = "\n"))
  }

  return(tp)
}


# Format numbers
#' @keywords internal
pret_num <- function(x){
  prettyNum(x, big.mark = ",", preserve.width = "none", scientific = FALSE)
}