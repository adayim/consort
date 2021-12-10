#' Add methods to print function
#'
#' Method for plot objects and display the output in on a grid device.
#'
#' @param x A \code{consort} object.
#' @param ... Not used.
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}},
#' \code{\link{add_side_box}}, \link[grid]{grid.draw}
#'
#' @return None.
#'
#' @rdname plot.consort
#' @export
plot.consort <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x, ...)
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

# Get previous two grobs
#' @keywords internal
#'
get_prev_grobs <- function(x, col = NULL) {
  nd_list <- which(sapply(x, is.textbox))

  if (is.null(col)) {
    mx <- max(nd_list)
    if (grepl("sidebox", x[[mx]]$name)) {
      side_grob <- x[[mx]]
      vert_grob <- x[[nd_list[length(nd_list) - 1]]] # The previous one is a terminal node
    } else {
      side_grob <- NULL
      vert_grob <- x[[mx]]
    }
  } else {
    sp_layout <- attr(x, "split_layout")
    sp_layout <- sp_layout[, col]
    last_pos <- sp_layout[length(sp_layout)]

    if (length(sp_layout) == 1 | grepl("vertbox|splitbox", names(last_pos))) {
      side_grob <- NULL
      vert_grob <- x[[last_pos]]
    } else if ((is.na(last_pos) & grepl("sidebox", names(last_pos)))) {
      side_grob <- NULL
      vert_grob <- x[[sp_layout[length(sp_layout) - 1]]] # The previous one is a terminal node
    } else {
      side_grob <- x[[last_pos]]
      vert_grob <- x[[sp_layout[length(sp_layout) - 1]]] # The previous one is a terminal node
    }
  }

  return(list(vert_grob = vert_grob, side_grob = side_grob))
}

#' @keywords internal
is.textbox <- function(x) {
  inherits(x, "textbox")
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
