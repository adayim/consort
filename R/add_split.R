

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
                      just = c("center", "left", "right"),
                      text_width = NULL,
                      ...) {

  dots <- list(...)

  just <- match.arg(just)

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

  if (any(sapply(prev_box, "[[", "node_type") %in% "splitbox") & !is.list(txt) & length(prev_nodes) > 1) {
    stop("txt should be a list for a nested splits.")
  }

  if(is.list(txt) & length(txt) != length(prev_nodes))
    stop("The length of the text should be the same as the number of previous nodes.")

  if(attr(prev_box, "nodes.type") == "label")
      stop("The last box added is a label, can not add box after a label!")

  # if (length(txt) == 1) {
  #   stop("The length of txt should be larger than 1, please use add_box instead.")
  # }

  if(!is.list(txt)){
    nodes <- lapply(seq_along(txt), function(i){
    box <- do.call(textbox, c(list(text = txt[i], just = just, box_fn = rectGrob, name = "splitbox"), dots))
      list(
        text = txt[i],
        node_type = "splitbox",
        box = box,
        box_hw = get_coords(box),
        side = NULL,
        just = just,
        prev_node = prev_nodes
      )
    })
  }else{
    # For nested splits
    nodes <- lapply(seq_along(txt), function(i){
      lapply(txt[[i]], function(x){
        box <- do.call(textbox, c(list(text = x, just = just, 
                                       box_fn = rectGrob, name = "splitbox"), dots))
        list(
          text = x,
          node_type = "splitbox",
          box = box,
          box_hw = get_coords(box),
          side = NULL,
          just = just,
          prev_node = prev_nodes[i]
        )
      })
    })
    nodes <- unlist(nodes, recursive = FALSE)
  }

  names(nodes) <- paste0("node", num_nodes + seq_along(unlist(txt)))

  node_list <- c(prev_box, nodes)

  class(node_list) <- union("consort", class(node_list))

  structure(node_list,
    nodes.num = length(unlist(txt)) + num_nodes,
    nodes.current = names(nodes),
    nodes.type = "splitbox",
    nodes.list = c(attr(prev_box, "nodes.list"), list(names(nodes)))
  )
}
