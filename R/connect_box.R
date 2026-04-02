
#' Connect grob box with arrow.
#'
#' This function is used to create an arrow line to connect two boxes. User should
#' provide the starting and ending side of the arrow.
#'
#' @param start Starting point of the arrow.
#' @param end Ending point of the arrow.
#' @param connect The connection of the box. It should be the combination of the
#' position. The \code{t} refers to "top", \code{l} for "left", \code{b} for
#' "bottom" and \code{r} for "right". The first letter is the starting point of
#' the start box, the second is the ending point of the end box. For example, if
#' one wants to connect the left side of the start box with right side of the
#' left side of the end box, the value should be \code{"lr"}. All the connection
#' will be started in the middle point.
#' @param type Should be one the \code{"s"} (strait line), or \code{"p"} (polyline).
#' @param name A character identifier of the line grob, passed to \code{\link[grid]{linesGrob}}.
#'
#' @return A lines grob with arrow.
#' @export
#' @rdname connect_box
#'
#' @examples
#' fg1 <- textbox(text = "This is a test")
#' fg2 <- textbox(text = "This is an other test", 0.7, 0.2)
#' grid::grid.draw(fg1)
#' grid::grid.draw(fg2)
#' connect_box(fg1, fg2, connect = "bl", type = "p")
connect_box <- function(start, end,
                        connect,
                        type = c("s", "p"),
                        name = NULL) {
  # if (!all(inherits(start, "textbox"), inherits(end, "textbox"))) {
  #   stop("Start and end must be a textbox object.")
  # }

  type <- match.arg(type)

  if (nchar(connect) != 2) {
    stop("Connect must be of two characters")
  }

  start_s <- substr(connect, 1, 1)
  end_s <- substr(connect, 2, 2)

  if (!grepl(start_s, "trbl", fixed = TRUE) | !grepl(end_s, "trbl", fixed = TRUE)) {
    stop("Connect must be a combination of \'t', \'r', \'b', \'l'.")
  }

  start_coords <- get_coords(start)
  end_coords <- get_coords(end)

  # X and Y coordinates for starting box
  x_s <- switch(start_s,
    "t" = start_coords$x,
    "r" = start_coords$right,
    "b" = start_coords$x,
    "l" = start_coords$left
  )

  y_s <- switch(start_s,
    "t" = start_coords$top,
    "r" = start_coords$y,
    "b" = start_coords$bottom,
    "l" = start_coords$y
  )

  # X and Y coordinates for ending box
  x_e <- switch(end_s,
    "t" = end_coords$x,
    "r" = end_coords$right,
    "b" = end_coords$x,
    "l" = end_coords$left
  )

  y_e <- switch(end_s,
    "t" = end_coords$top,
    "r" = end_coords$y,
    "b" = end_coords$bottom,
    "l" = end_coords$y
  )

  get_dist <- function(a, b) {
    res <- a - b
    # Consider the arrow head
    if (as.numeric(convertUnit(res, "mm", valueOnly = TRUE)) < 0) {
      0.5 * (res + unit(0.1, "inches"))
    } else {
      0.5 * (res - unit(0.1, "inches"))
    }
  }

  if (type == "s") {
    line_coords <- list(
      x = unit.c(x_s, x_e),
      y = unit.c(y_s, y_e)
    )
  } else {
    if (start_s %in% c("t", "b")) {
      if (end_s %in% c("t", "b")) {
        y_dist <- get_dist(y_s, y_e)
        x_mid <- unit.c(x_s, x_e)
        y_mid <- unit.c(y_s - y_dist, y_s - y_dist)
      } else {
        x_mid <- x_s
        y_mid <- y_e
      }
    }

    if (start_s %in% c("r", "l")) {
      if (end_s %in% c("r", "l")) {
        x_dist <- get_dist(x_s, x_e)
        x_mid <- unit.c(x_s - x_dist, x_s - x_dist)
        y_mid <- unit.c(y_s, y_e)
      } else {
        x_mid <- x_e
        y_mid <- y_s
      }
    }

    line_coords <- list(
      x = unit.c(x_s, x_mid, x_e),
      y = unit.c(y_s, y_mid, y_e)
    )
  }

  linesGrob(
    x = line_coords$x,
    y = line_coords$y,
    gp = gpar(fill = "black"),
    arrow = arrow(
      length = unit(0.1, "inches"),
      ends = "last", type = "closed"
    ),
    name = name
  )
}
