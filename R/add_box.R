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
#' @param text_width a positive integer giving the target column for wrapping
#' lines in the output. String will not be wrapped if not defined (default).
#' The \code{\link[stringi]{stri_wrap}} function will be used if \code{stringi}
#' package installed, otherwise \code{\link[base]{strwrap}} will be used.
#' @param ... Other parameters pass to \link{textbox},
#'
#' @seealso \code{\link{add_side_box}} \code{\link{add_split}} \code{\link{textbox}} 
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
#' 
#' 
add_box <- function(prev_box = NULL,
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

  if (!is.null(prev_box)) {
    if (!inherits(prev_box, c("consort"))) {
      stop("prev_box must be consort object")
    }

    if(attr(prev_box, "nodes.type") == "label")
      stop("The last box added is a label, can not add box after a label!")

    prev_nodes <- attr(prev_box, "nodes.current")
    num_nodes <- attr(prev_box, "nodes.num")

    if (length(txt) != 1 & !length(prev_nodes) %in% c(1, length(txt))) {
      stop("Text with length of 1 or same node number as `prev_box`.")
    }

    if(length(prev_nodes) != length(txt))
      prev_nodes <- rep(prev_nodes, length(txt))

    # Create node

    nodes <- lapply(seq_along(txt), function(i){
      box <- do.call(textbox, c(list(text = txt[i], just = just, 
                                     box_fn = rectGrob, 
                                     name = "vertbox"), dots))
      if(length(txt) == 1){
        prev_nd <- prev_nodes
      }else{
        prev_nd <- prev_nodes[i]
      }
        

      list(
        text = txt[i],
        node_type = "vertbox",
        box = box,
        box_hw = get_coords(box),
        just = just,
        side = NULL,
        prev_node = prev_nd
      )
    })

    names(nodes) <- paste0("node", num_nodes + seq_along(txt))

    node_list <- c(prev_box, nodes)

    class(node_list) <- union("consort", class(node_list))

    structure(node_list,
      nodes.num = length(txt) + num_nodes,
      nodes.current = names(nodes),
      nodes.type = "vertbox",
      nodes.list = c(attr(prev_box, "nodes.list"), list(names(nodes)))
    )

  } else {

    nodes <- lapply(txt, function(x){
      box <- do.call(textbox, c(list(text = x, just = just, box_fn = rectGrob, name = "vertbox"), dots))
      list(
        text = x,
        node_type = "vertbox",
        box = box,
        box_hw = get_coords(box),
        side = NULL,
        just = just,
        inv_join = NULL,
        prev_node = NULL
      )
    })

    names(nodes) <- paste0("node", seq_along(txt))

    class(nodes) <- union("consort", class(nodes))

    structure(nodes,
      nodes.num = length(txt),
      nodes.current = names(nodes),
      nodes.type = "vertbox",
      nodes.list = list(names(nodes))
    )

  }
}

