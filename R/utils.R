#' Add methods to print function
#'
#' Method for plot objects and display the output in on a grid device.
#'
#' @param x A \code{consort} object.
#' @param grViz If use \link[DiagrammeR]{grViz} to print the plot. 
#' Default is \code{FALSE} to use \link[grid]{grid.draw}
#' @param ... Not used.
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}}, \link[grid]{grid.draw}
#'
#' @return None.
#'
#' @rdname plot.consort
#' @export
plot.consort <- function(x, grViz = FALSE, ...) {
  if(!grViz){
    r <- build_grid(x)
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
