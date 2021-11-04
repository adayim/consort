

#' Add a splitting box
#'
#' This function will create a horizontally aligned nodes. The horizontal coordinate
#' will be automatically calculated if the coordinates not provided.
#'
#' @param prev_box Previous node that the newly created split box will be aligned.
#' @param txt A vector of text labels for each nodes.
#' @param coords The horizontal coordinates of the boxes, see details.
#' @inheritParams add_box
#'
#'
#' @details
#' The `coords` will be used to set the horizontal coordinates of the nodes. The
#' `coords` should be within 0 and 1 to avoid the nodes is aligned outside of the
#'  final figure. If the `coords` is `NULL`, not given. The function will calculate
#'  the `coords`. If the the length of the `txt` is two, then a coordinates of
#'  0.35 and 0.65 will be used. Once the split box is added, all the following nodes
#'  will be split accordingly.
#'
#' @seealso \code{\link{add_box}}, \code{\link{add_side_box}}
#'
#' @return A \code{consort.list} object.
#'
#' @export
#'
#' @examples
#' txt1 <- "Population (n=300)"
#' txt1_side <- "Excluded (n=15): \n
#'               \u2022 MRI not collected (n=3)\n
#'               \u2022 Tissues not collected (n=4)\n
#'               \u2022 Other (n=8)"
#'
#' g <- add_box(txt = txt1)
#'
#' g <- add_side_box(g, txt = txt1_side)
#'
#' g <- add_box(g, txt = "Randomized (n=200)")
#'
#' g <- add_split(g, txt = c("Arm A (n=100)", "Arm B (n=100"))
#' g <- add_side_box(g,
#'   txt = c(
#'     "Excluded (n=15):\n
#'                   \u2022 MRI not collected (n=3)\n
#'                   \u2022 Tissues not collected (n=4)\n
#'                    \u2022 Other (n=8)",
#'     "Excluded (n=15):\n
#'                    \u2022 MRI not collected (n=3)\n
#'                    \u2022 Tissues not collected (n=4)"
#'   )
#' )
#'
#' g <- add_box(g, txt = c("Final analysis (n=100)", "Final analysis (n=100"))
#' g <- add_label_box(g, txt = c("1" = "Screening", "3" = "Randomized", "4" = "Final analysis"))
add_split <- function(prev_box,
                      txt,
                      coords = NULL,
                      dist = 0.02,
                      text_width = NULL,
                      ...) {

  # Wrap text
  if (!is.null(text_width)) {
    txt <- sapply(txt, function(tx) {
      text_wrap(unlist(tx), width = text_width)
    })
  }

  if (!is.unit(dist)) {
    dist <- unit(dist, "npc")
  }

  if (!is.null(attr(prev_box, "split_layout"))) {
    stop("Nested splits are not supported in the current version")
  }

  if (length(txt) == 1) {
    stop("The length of txt should be larger than 1, please use add_box instead.")
  }

  grb_lst <- get_prev_grobs(prev_box)
  if (!is.null(grb_lst$side_grob)) {
    stop("The last box added is not a terminal box!")
  }

  # Define coordinates for the splits
  if (is.null(coords)) {
    n_gp <- length(txt)
    if (n_gp == 2) {
      x_coords <- c(0.35, 0.65)
    } else {
      x_coords <- 0.5 / n_gp
      x_coords <- c(x_coords, rep(2 * x_coords, times = n_gp - 1))
      x_coords <- cumsum(x_coords)
    }
  } else {
    if (length(coords) != length(txt)) {
      stop("The length of the coords should match the length of txt.")
    }

    if (any(coords > 1 | coords < 0)) {
      warning("The coordinates are not within 0 and 1 and might be cropped in the final output.")
    }

    x_coords <- coords
  }

  prev_grob <- grb_lst$vert_grob
  len_grobs <- length(prev_box)

  .add_split <- function(prev_grob, txt, x, dist, ...) {
    pre_cords <- get_coords(prev_grob)

    out_grob <- textbox(txt, x = x, box_fn = rectGrob, name = "vertbox", ...)
    y_cords <- pre_cords$y - get_coords(out_grob)$half_height - 3 * dist
    move_box(out_grob, y = y_cords)
  }

  for (i in seq_len(length(txt))) {
    out_box <- .add_split(prev_grob,
      txt  = txt[i],
      x    = x_coords[i],
      dist = dist,
      ...
    )

    connect <- connect_box(prev_grob, out_box, connect = "bt", type = "p")

    prev_box <- gList(prev_box, out_box, connect)
  }

  # Skip counting the connection grob
  split_layout <- matrix(len_grobs + seq(length.out = length(txt), by = 2),
    ncol = length(txt)
  )
  row.names(split_layout) <- "splitbox"

  class(prev_box) <- union("consort", class(prev_box))

  structure(prev_box, split_layout = split_layout)
}
