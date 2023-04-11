#' Add a vertically aligned label nodes on the left side.
#'
#' In a consort diagram, this can be used to indicate different stage.
#'
#' @param prev_box A completed diagram created with \code{add_box}, \code{add_side_box} etc.
#' @param txt Text in the node. If a character string is provided, the label will be aligned
#'  to the last box if a character is provided. If a named vector, the labels will align to
#' corresponding row of the node. And the names is the number indicating row number of box to
#'  horizontally align with and value is the text in the box.
#' @param only_terminal If the txt is only for the terminal box, default. Otherwise, the side box will
#' also be accounted for.
#' @param just The justification for the text: center (default), left or right.
#' @param ... Other parameters pass to \link{textbox},
#'
#' @export
#' @seealso \code{\link{add_side_box}} \code{\link{add_split}} \code{\link{textbox}} 
#' \code{\link{add_box}}
#' @return A \code{consort} object.
#' 
#' @importFrom utils modifyList
#' @example inst/examples/add-box-example.R

add_label_box <- function(prev_box,
                          txt,
                          only_terminal = TRUE,
                          just = c("center", "left", "right"),
                          ...) {

  just <- match.arg(just)

  if (length(txt) > 1 & is.null(names(txt))) {
    stop("txt must be a named vector.")
  }

  if (length(txt) > 1 & is.null(names(txt)) & !any(is.na(as.numeric(names(txt))))) {
    stop("txt names must be number indicating the row position of the terminal
          node to be aligned.")
  }

  if (!inherits(prev_box, c("consort"))) {
    stop("prev_box must be consort object")
  }

  cex <- ifelse("cex" %in% names(getOption("txt_gp")), getOption("txt_gp")$cex, 1)
  # Set default values
  args_list <- list()
  # args_list$text <- txt
  args_list$txt_gp <- gpar(col = "#4F81BD", cex = cex, fontface = "bold")
  args_list$box_gp <- gpar(fill = "#A9C7FD")
  args_list$box_fn <- roundrectGrob
  args_list$name <- "label"

  args_list <- modifyList(args_list, list(...))
  
  # Node type of each
  nodes_layout <- attr(prev_box, "nodes.list")
  nd_type <- sapply(nodes_layout, function(x) 
    unique(sapply(prev_box[x], "[[", "node_type"))
  )
  
  if (only_terminal) {
    # Get the index of the vertical box
    grob_index <- which(nd_type %in% c("vertbox", "splitbox"))
  } else {
    grob_index <- seq_along(nodes_layout)
  }
  
  num_nodes <- attr(prev_box, "nodes.num")

  if (length(txt) == 1 & is.null(names(txt)))
    names(txt) <- 1

  if(length(txt) > length(nd_type))
    stop("provided text is larger than diagram node numbers.")

  nodes <- lapply(seq_along(txt), function(i){
    box <- do.call(textbox, c(list(text = txt[i]), args_list))
    list(
      text = txt[i],
      node_type = "label",
      box = box,
      box_hw = get_coords(box),
      side = NULL,
      just = just,
      gpar = args_list[c("txt_gp", "box_gp")],
      prev_node = grob_index[as.numeric(names(txt[i]))]
    )
  })
  
  names(nodes) <- paste0("label", num_nodes + seq_along(txt))
  
  node_list <- c(prev_box, nodes)
  
  class(node_list) <- union("consort", class(node_list))
  
  structure(node_list,
            nodes.num = length(txt) + num_nodes,
            nodes.current = names(nodes),
            nodes.type = "label",
            nodes.list = attr(prev_box, "nodes.list")
  )
}


