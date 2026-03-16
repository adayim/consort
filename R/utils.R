#' Add methods to print function
#'
#' Method for plot objects and display the output in on a grid device.
#'
#' @param x A \code{consort} object.
#' @param grViz If use \link[DiagrammeR]{grViz} to print the plot.
#' Default is \code{FALSE} to use \link[grid]{grid.draw}
#' @param diagram_width Width of the diagram viewport. Can be a number between
#' 0 and 1 (proportion of device width) or a \code{\link[grid]{unit}} object
#' (e.g., \code{unit(15, "cm")}). Default is \code{NULL} (uses 0.98 of device
#' width). Only used when \code{grViz = FALSE}.
#' @param diagram_height Height of the diagram viewport. Can be a number between
#' 0 and 1 (proportion of device height) or a \code{\link[grid]{unit}} object
#' (e.g., \code{unit(20, "cm")}). Default is \code{NULL} (uses 0.98 of device
#' height). Only used when \code{grViz = FALSE}.
#' @param ... Not used.
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}}, \link[grid]{grid.draw}
#'
#' @return None.
#'
#' @rdname plot.consort
#' @export
plot.consort <- function(x, grViz = FALSE, diagram_width = NULL,
                         diagram_height = NULL, ...) {
  if(!grViz){
    r <- build_grid(x, diagram_width = diagram_width,
                    diagram_height = diagram_height)
    grid.newpage()
    grid.draw(r)
  }else{
    if (requireNamespace("DiagrammeR", quietly = TRUE)) {
      grviz_txt <- build_grviz(x)
      DiagrammeR::grViz(grviz_txt)
    } else {
      stop("package `DiagrammeR` is needed to draw grViz plot.")
    }
  }
}

#' @rdname plot.consort
#' @export
print.consort <- plot.consort

# Check if empty
#' @keywords internal
#'
is_empty <- function(x){
  x <- gsub("[[:space:]]", "", x)
  is.null(x) | x == "" | is.na(x)
}

# Wrap text
#' @keywords internal
#'
text_wrap <- function(txt, width = 0.9 * getOption("width")) {
  if (length(txt) > 1) {
    stop("Vector does not supported!")
  }

  # Split text by line break
  s_txt <- unlist(strsplit(txt, split = "\n"))
  if (requireNamespace("stringi", quietly = TRUE)) {
    s_txt <- stringi::stri_wrap(s_txt, width, 0)
  } else {
    s_txt <- strwrap(s_txt, width)
  }
  paste(s_txt, collapse = "\n")
}

#' @keywords internal
init_auto_index <- function() {
  index <- 0
  function() {
    index <<- index + 1
    index
  }
}

auto_index <- init_auto_index()
