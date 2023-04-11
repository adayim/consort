

#' Add a splitting box
#'
#' This function will create a horizontally aligned nodes. The horizontal coordinate
#' will be automatically calculated if the coordinates not provided.
#'
#' @param prev_box Previous node that the newly created split box will be aligned.
#' @inheritParams add_box
#'
#'
#' @seealso \code{\link{add_box}}, \code{\link{add_side_box}} \code{\link{textbox}} 
#'
#' @return A \code{consort.list} object.
#'
#' @export
#'
#' @example inst/examples/add-box-example.R
add_split <- function(prev_box,
                      txt,
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

  if (any(attr(prev_box, "nodes.type") %in% "splitbox")) {
    stop("Nested splits are not supported in the current version")
  }

  if(attr(prev_box, "nodes.type") == "label")
      stop("The last box added is a label, can not add box after a label!")

  if (length(txt) == 1) {
    stop("The length of txt should be larger than 1, please use add_box instead.")
  }

  nodes <- lapply(seq_along(txt), function(i){
    box <- do.call(textbox, c(list(text = txt[i], just = "center", box_fn = rectGrob, name = "splitbox"), dots))
    list(
      text = txt[i],
      node_type = "splitbox",
      box = box,
      box_hw = get_coords(box),
      side = NULL,
      just = "center",
      prev_node = prev_nodes
    )
  })

  names(nodes) <- paste0("node", num_nodes + seq_along(txt))

  node_list <- c(prev_box, nodes)

  class(node_list) <- union("consort", class(node_list))

  structure(node_list,
    nodes.num = length(txt) + num_nodes,
    nodes.current = names(nodes),
    nodes.type = "splitbox",
    nodes.list = c(attr(prev_box, "nodes.list"), list(names(nodes)))
  )
}
