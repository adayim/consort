
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
gen_text <- function(x, label = NULL, bullet = FALSE,
                     bullet_char = consort_opt("bullet")) {
  
  if(!is.null(label) & length(label)>1)
    stop("label must be of length 1")
  
  if(is.data.frame(x) && ncol(x) == 1)
    x <- unlist(x)
  
  if (is.list(x)) {
    if(is.data.frame(x)){
      box_data.frame(x, label = label, bullet_char = bullet_char)
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
          r <- box_data.frame(val, bullet_char = bullet_char)
          if(!is.null(label)){
            lab_lst <- sprintf("%s (n=%s)", 
                               label[indx], 
                               pret_num(sum(!is.na(val[[1]]))))
            r <- paste(lab_lst, r, sep = "\n")
          }
          
        }else{
          r <- box_label(val, label = label[indx], bullet = bullet,
                         bullet_char = bullet_char)
          r <- paste(r, collapse = "\n")
        }
        
        return(r)
        
      }, simplify = TRUE)
    }
    
  } else {
    box_label(x = x, label = label, bullet = bullet, bullet_char = bullet_char)
  }
}

# Calculate the numbers in the box use the data provided.
#' @keywords internal
box_data.frame <- function(x, label = NULL,
                           bullet_char = consort_opt("bullet")){
  if(ncol(x) != 2)
    stop("only two columns are supported")
  
  if(!is.null(label))
    label <- sprintf("%s (n=%s)", label, pret_num(sum(!is.na(x[[1]]))))
  
  r <- sapply(na.omit(unique(x[[1]])), function(i){
    box_label(x[[2]][x[[1]] == i], label = i, bullet = TRUE,
              bullet_char = bullet_char)
  })
  
  r <- paste(r, collapse = "\n")
  
  if(!is.null(label))
    r <- paste(label, r, sep = "\n")
  
  return(r)
}


# Calculate the numbers in the box
#' @keywords internal
box_label <- function(x, label, bullet = TRUE,
                      bullet_char = consort_opt("bullet")) {
  
  # Blank as NA
  if (is.character(x)) {
    x[x == ""] <- NA
  }
  
  # Return blank if no values and with bullet
  if (sum(!is.na(x)) == 0 & bullet) {
    return("")
  }
  
  if (is.null(label)) {
    if(is.factor(x))
      tab <- table(droplevels(x))
    else
      tab <- table(x)
    
    tp <- paste0(names(tab), " (n=", pret_num(tab), ")")
    if(!bullet){
      return(tp)
    }else{
      return(paste0(bullet_char, " ", paste(tp, collapse = paste0("\n", bullet_char, " "))))
    }
    
  }
  
  tp <- paste0(label, " (n=", pret_num(sum(!is.na(x))), ")")
  
  if (bullet) {
    if(is.factor(x))
      tab <- table(droplevels(x))
    else
      tab <- table(x)
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