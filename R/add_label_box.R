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
#' @param widths A numeric vector of length 2 specifying relative percentage
#' of the label and diagram in the final graph.
#' @param ... Other parameters pass to \link{textbox},
#'
#' @details
#' The \code{ref_box} parameter kept for the legacy reason, and should be avoided. This is
#' to create a box to horizontally align with the \code{ref_box}.
#'
#' @export
#'
#' @return A \code{consort} object.
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
add_label_box <- function(prev_box,
                          txt,
                          widths = c(0.1, 0.9),
                          only_terminal = TRUE,
                          ...) {
  ot_input <- list(...)

  if (length(widths) != 2) {
    stop("The widths should be a length of two.")
  }

  widths <- as.numeric(widths)

  if (length(txt) > 1 & is.null(names(txt))) {
    stop("txt must be a named vector.")
  }

  if (length(txt) > 1 & is.null(names(txt)) & !any(is.na(as.numeric(names(txt))))) {
    stop("txt names must be number indicating the row position of the terminal
          node to be aligned.")
  }

  if (!inherits(prev_box, c("gList", "consort"))) {
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

  if (!is.null(names(ot_input))) {
    args_list <- c(
      args_list[!names(args_list) %in% names(ot_input)],
      ot_input
    )
  }

  if (only_terminal) {
    # Get the index of the vertical box
    grob_index <- sapply(prev_box, function(x) grepl("vertbox|splitbox", x$name) & is.textbox(x))
    grob_index <- which(grob_index)
  } else {
    grob_index <- which(sapply(prev_box, is.textbox))
  }


  # For only one label to create, legacy code.
  if (length(txt) == 1 & is.null(names(txt))) {
    args_list$text <- txt

    ref_pos <- get_coords(prev_box[[max(grob_index)]])

    grob_list <- .add_label_box(ref_pos, args_list)
  } else {
    if (!is.null(attr(prev_box, "split_layout"))) {
      lay <- attr(prev_box, "split_layout")
      if (only_terminal) {
        lay <- lay[grepl("vertbox|splitbox", row.names(lay)), ]
      }

      grob_index <- c(grob_index[grob_index < min(lay)], lay[, 1])
    }

    grob_index <- grob_index[as.numeric(names(txt))]

    for (i in seq_len(length(txt))) {
      args_list$text <- txt[i]
      ref_pos <- get_coords(prev_box[[grob_index[i]]])
      out_box <- .add_label_box(ref_pos, args_list)

      if (i == 1) {
        grob_list <- gList(gList(), out_box)
      } else {
        grob_list <- gList(grob_list, out_box)
      }
    }
  }
  
  layout <- grid.layout(1, 2, 
                        widths = unit(widths, "null"),
                        heights = unit(c(1,1), "null"))
  ful_grob <- frameGrob(layout = layout)
  
  ful_grob <- placeGrob(ful_grob, 
                        grobTree(grob_list, name = "label"),
                        row = 1, col = 1)
  ful_grob <- placeGrob(ful_grob,
                        grobTree(prev_box, name = "nodes"),
                        row = 1, col = 2)

  res <- packGrob(frameGrob(), ful_grob, dynamic = TRUE)
  
  class(res) <- union("consort", class(res))

  return(res)
}


.add_label_box <- function(ref_pos, args_list) {
  out_box <- do.call(textbox, args_list)

  # Align with the reference box
  box_pos <- get_coords(out_box)
  move_box(out_box,
    x = unit(0.5, "npc") + box_pos$half_width,
    y = ref_pos$top - box_pos$half_height
  )
}
