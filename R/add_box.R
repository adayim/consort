#' Add nodes
#'
#' Create/add vertically aligned labeled nodes or side nodes.
#'
#'
#' @param prev_box Previous node object, the created new node will be vertically
#' aligned with this node. Left this as `NULL` if this is the first node. The first
#' node will be aligned in the top center.
#' @param txt Text in the node. If the `prev_box` is a horizontally aligned multiple
#' nodes, a vector of with the same length must be provided.
#' @param just The justification for the text: left, center or right.
#' @param dist Distance between previous node, including the distance between the
#' side node.
#' @param y A number or unit object specifying y-location of the starting point of
#' the diagram, default is 0.9npc. Will be ignored if \code{prev_box} is not null.
#' @param text_width a positive integer giving the target column for wrapping
#' lines in the output. String will not be wrapped if not defined (default).
#' The \code{\link[stringi]{stri_wrap}} function will be used if \code{stringi}
#' package installed, otherwise \code{\link[base]{strwrap}} will be used.
#' @param ... Other parameters pass to \link{textbox},
#'
#' @seealso \code{\link{add_side_box}},\code{\link{add_split}}
#' @return A \code{consort} object.
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
#'                    \u2022 Tissues not collected (n=4)\n
#'                    \u2022 Other (n=8)",
#'     "Excluded (n=15):\n
#'                     \u2022 MRI not collected (n=3)\n
#'                     \u2022 Tissues not collected (n=4)"
#'   )
#' )
#'
#' g <- add_box(g,
#'   txt = c(
#'     "Final analysis (n=100)",
#'     "Final analysis (n=100"
#'   )
#' )
add_box <- function(prev_box = NULL,
                    txt,
                    just = "center",
                    dist = 0.02,
                    y = unit(0.9, "npc"),
                    text_width = NULL,
                    ...) {

  # Wrap text
  if (!is.null(text_width)) {
    txt <- sapply(txt, function(tx) {
      text_wrap(unlist(tx), width = text_width)
    })
  }


  if (!is.null(prev_box)) {
    if (!inherits(prev_box, c("consort", "gList"))) {
      stop("prev_box must be consort object")
    }

    sp_layout <- attr(prev_box, "split_layout")

    if (is.null(sp_layout) & length(txt) > 1) {
      stop("Text with length of 1 supplied for splitted diagram.")
    }

    if (length(txt) > 1 | !is.null(sp_layout)) {
      if (!is.null(sp_layout) & length(txt) != ncol(sp_layout)) {
        stop("The txt length must be same as splitted node number.")
      }
    }

    # No allocation split
    if (length(txt) == 1) {

      # Get the previous terminal node and side node
      grb_lst <- get_prev_grobs(prev_box)

      out_box <- .add_box(
        prev_vert = grb_lst$vert_grob,
        prev_side = grb_lst$side_grob,
        txt = txt,
        just = just,
        dist = dist,
        ...
      )

      connect <- connect_box(grb_lst$vert_grob, out_box, connect = "bt")

      grob_list <- gList(prev_box, out_box, connect)

      class(grob_list) <- union("consort", class(grob_list))

      structure(grob_list,
        split_layout = NULL
      )
    } else {

      # Get the length of grob before editing
      len_grobs <- length(prev_box)

      grb_lst <- lapply(seq_along(txt), function(i) {
        get_prev_grobs(prev_box, col = i)
      })

      # If allocation split
      out_box <- vector("list", length = length(txt))

      # Create terminal box
      for (i in seq_along(txt)) {
        out_box[[i]] <- .add_box(
          prev_vert = grb_lst[[i]]$vert_grob,
          prev_side = grb_lst[[i]]$side_grob,
          txt = txt[i],
          just = just,
          dist = dist,
          ...
        )
      }

      out_box <- align_hori(out_box) # Horizontal align terminal box

      # Connect grobs
      for (i in seq_along(txt)) {
        connect <- connect_box(grb_lst[[i]]$vert_grob,
          out_box[[i]],
          connect = "bt"
        )

        prev_box <- gList(prev_box, out_box[[i]], connect)
      }

      # Skip counting the connection grob
      split_layout <- matrix(len_grobs + seq(length.out = length(txt), by = 2),
        ncol = length(txt)
      )
      row.names(split_layout) <- "vertbox"

      class(prev_box) <- union("consort", class(prev_box))

      structure(prev_box,
        split_layout = rbind(sp_layout, split_layout)
      )
    }
  } else {
    out_box <- textbox(txt,
      x = 0.5, y = y,
      box_fn = rectGrob,
      name = "vertbox"
    )

    grob_list <- gList(gList(), out_box)

    class(grob_list) <- union("consort", class(grob_list))

    structure(grob_list,
      split_layout = NULL
    )
  }
}

#' Create node vertically align with the previous one
#'
#' @inheritParams add_box
#' @keywords internal
.add_box <- function(prev_vert,
                     prev_side = NULL,
                     txt,
                     just = "center",
                     dist = 0.02,
                     ...) {
  if (!is.unit(dist)) {
    dist <- unit(dist, "npc")
  }

  # If previous box is not a side box
  if (is.null(prev_side)) {
    pre_box <- prev_vert
    dist <- 2 * dist # Add more distance
  } else {
    pre_box <- prev_side
  }

  pre_cords <- get_coords(pre_box)

  box <- textbox(txt, just = just, box_fn = rectGrob, name = "vertbox", ...)

  y_cords <- pre_cords$bottom - dist - get_coords(box)$half_height
  x <- get_coords(prev_vert)$x

  move_box(box, x = x, y = y_cords)
}
