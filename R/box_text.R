
#' Generate label and bullet points
#' 
#' This function use the data to generate label and bullet points for the box.
#'
#' @param x A list or a vector to be used.
#' @param label A character string as a label at the beginning of the text label. 
#' The count for each categories will be returned if no label is provided.
#' @param bullet If shows bullet points. If the value is `TRUE`, the bullet points
#' will be tabulated, default is `FALSE`.
#'
#' @return A character string of vector.
#' @export
#' 
#' @rdname gen_text
#'
#' @examples
#' val <- data.frame(am = factor(ifelse(mtcars$am == 1, "Automatic", "Manual")),
#'                  car = row.names(mtcars))
#'  
#'  gen_text(val$car, label = "Cars in the data")
#'  gen_text(val$car, label = "Cars in the data", bullet = FALSE)
#'  gen_text(split(val$car, val$am), label = "Cars in the data")
#'  gen_text(split(val$car, val$am), label = "Cars in the data", bullet = FALSE)
#' 
#' 
gen_text <- function(x, label = NULL, bullet = FALSE){
  if(is.list(x)){
    sapply(x, function(val){
      box_label(x = val, label = label, bullet = bullet)
    }, simplify = TRUE)
  }else{
    box_label(x = x, label = label, bullet = bullet)      
  }
}

#' @export
#' @rdname gen_text
box_text <- gen_text


# Calculate the numbers in the box use the data provided.
#' @keywords internal
box_label <- function(x, label, bullet = TRUE){
  
  # Blank as NA
  if(is.character(x))
    x[x == ""] <- NA
  
  # Return NULL if no values and with bullet
  if(sum(!is.na(x)) == 0 & bullet)
    return(NULL)
  
  if(is.null(label))
    return(paste0(names(table(x)), " (n=", table(x), ")"))
  
  tp <- paste0(label, " (n=", sum(!is.na(x)), ")")
  
  if(bullet){
    txt_sub <- paste0("\u2022 ", names(table(x)), " (n=", table(x), ")")
    tp <- paste0(tp, ":\n", paste(txt_sub, collapse = "\n"))
  }
  
  return(tp)
  
}

