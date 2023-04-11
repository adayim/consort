#' Add a side node
#'
#' Add an exclusion node on the right side. If the length of text label is two, then
#' the first one will be aligned on the left and the second on the right. Otherwise,
#' all the side nodes will be aligned on the right.
#'
#' @param prev_box Previous node object, the created new node will be aligned
#' at the right bottom of the `prev_box`.
#' @param side Position of the side box, `left` or `right` side of the terminal box.
#' Will be aligned on the left and right side if only two groups, right otherwise.
#' This will be ignored for for `grViz` plot, see \link[DiagrammeR]{grViz}.
#' @inheritParams add_box
#'
#' @seealso \code{\link{add_box}} \code{\link{add_split}} \code{\link{textbox}} 
#' \code{\link{add_label_box}} 
#'
#' @return A \code{consort} object.
#'
#' @export
#'
#' @example inst/examples/add-box-example.R
add_side_box <- function(prev_box,
                         txt,
                         side = NULL,
                         text_width = NULL,
                         ...) {

  dots <- list(...)

  # Wrap text
  if (!is.null(text_width)) {
    txt <- sapply(txt, function(tx) {
      text_wrap(unlist(tx), width = text_width)
    })
  }

  if (!inherits(prev_box, c("consort"))) {
    stop("prev_box must be consort object")
  }

  prev_nodes <- attr(prev_box, "nodes.current")
  num_nodes <- attr(prev_box, "nodes.num")

  if (all(attr(prev_box, "nodes.type") %in% c("sidebox", "label"))) {
    stop("The last box added is a side box or label, can not add side box after a sidebox!")
  }

  if (length(prev_nodes) != length(txt)) 
    stop("The txt length must be same as previous node number.")

  if (!is.null(side) & length(side) != length(txt)) {
    stop("The length of side must have the same length with txt.")
  }

  # One box on left, the other is right if only two groups given,
  # all will be on right side if not.
  if (is.null(side)) {
    if (length(txt) == 2) {
      side <- c("left", "right")
    } else {
      side <- rep("right", length(txt))
    }
  }

  nodes <- lapply(seq_along(txt), function(i){
    box <- do.call(textbox, c(list(text = txt[i], just = "left", box_fn = rectGrob, name = "sidebox"), dots))

    # Add width to the side box, calculate horizontal width
    prev_box <- prev_box[[prev_nodes[i]]]$box_hw
    box_hw <- get_coords(box)
    # Add extra width to account for side box 
    box_hw$width <- box_hw$width + convertWidth(prev_box$half_width, "mm", valueOnly = TRUE)*1.5
    # Don't know why this is too large in the plot
    box_hw$height <- box_hw$height/2 

    list(
      text = txt[i],
      node_type = "sidebox",
      box = box,
      box_hw = box_hw,
      side = side[i],
      just = "left",
      prev_node = prev_nodes[i]
    )
  })

    names(nodes) <- paste0("node", num_nodes + seq_along(txt))

    node_list <- c(prev_box, nodes)

    class(node_list) <- union("consort", class(node_list))

    structure(node_list,
      nodes.num = length(txt) + num_nodes,
      nodes.current = prev_nodes,
      nodes.type = "sidebox",
      nodes.list = c(attr(prev_box, "nodes.list"), list(names(nodes)))
    )

}

