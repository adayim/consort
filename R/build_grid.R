#' Build consort diagram
#'
#' Build a \code{grob} consort diagram, use this if you want
#' to save plots with \code{\link[ggplot2]{ggsave}}. \code{build_grid}
#' does not support more than two nested splits for the moment, please use
#'  \code{\link{build_grviz}} or \code{plot(g, grViz = TRUE)} for
#' such diagrams instead.
#'
#' The diagram is laid out at a fixed, device-independent "natural" size,
#' returned (in inches) as the \code{size} attribute of the result, which
#' can be passed straight to \code{\link[ggplot2]{ggsave}}'s \code{width}/
#' \code{height} for an undistorted export, e.g.
#' \code{sz <- attr(build_grid(x), "size"); ggsave(..., width = sz$width, height = sz$height)}.
#' With \code{shrink_to_fit = TRUE}, the diagram is scaled down (text, boxes
#' and spacing together, never enlarged) to fit the currently open graphics
#' device; this is what \code{\link{plot.consort}} uses so an interactive
#' plot window never ends up with overlapping nodes.
#'
#' @param x A consort object.
#' @param shrink_to_fit If \code{TRUE}, shrink the whole diagram (text
#'   included) so it fits the currently open graphics device, never
#'   enlarging it beyond its natural size. Default is \code{FALSE}, which
#'   always lays out the diagram at its natural size -- the right choice
#'   when the result is passed on to \code{ggsave()} rather than drawn
#'   directly.
#'
#' @return A \code{gList} object, with a \code{size} attribute giving the
#'   diagram's natural width/height in inches.
#' @export
#'
#' @seealso \code{\link[grid]{gList}}
#' @examples
#' \dontrun{
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
#' gr <- build_grid(g)
#' sz <- attr(gr, "size")
#' # ggsave("consort_diagram.pdf", plot = gr, width = sz$width, height = sz$height)
#' }
#'
build_grid <- function(x, shrink_to_fit = FALSE) {

  if (!inherits(x, c("consort")))
    stop("x must be consort object")

  has_labels <- any(grepl("label", names(x)))

  if(has_labels){
    consort_plot <- x[grepl("node", names(x))]
    attr(consort_plot, "nodes.list") <- attr(x, "nodes.list")
    label_plot <- x[grepl("label", names(x))]
  }else{
    consort_plot <- x
  }

  # Natural-size layout pass
  nodes_coord <- calc_coords(consort_plot)
  if(has_labels){
    label_coord <- calc_coords_label(label_plot,
                                     nodes_coord$nd_y,
                                     max_h = nodes_coord$max_height)
    natural_width <- nodes_coord$max_width + label_coord$width[1]
  }else{
    natural_width <- nodes_coord$max_width
  }

  # Shrink text, boxes and spacing together (never enlarge) to fit the
  # current device. Re-measuring after rescaling keeps box and position
  # units on the same scale, so nothing overlaps or gets squashed.
  if(isTRUE(shrink_to_fit)){
    shrink <- calc_shrink_factor(natural_width, nodes_coord$max_height)

    if(shrink < 1){
      old_pad <- consort_opt("pad_u")
      old_arrow <- consort_opt("arrow_length")
      on.exit(set_consort_defaults(pad_u = old_pad, arrow_length = old_arrow), add = TRUE)
      set_consort_defaults(pad_u = old_pad * shrink, arrow_length = old_arrow * shrink)

      consort_plot <- rescale_nodes(consort_plot, shrink)
      nodes_coord <- calc_coords(consort_plot)

      if(has_labels){
        label_plot <- rescale_nodes(label_plot, shrink)
        label_coord <- calc_coords_label(label_plot,
                                         nodes_coord$nd_y,
                                         max_h = nodes_coord$max_height)
      }
    }
  }

  # Generate connection
  nodes_connect <- get_connect(consort_plot)

  # Move all nodes to the left if there are labels nodes
  # based on the width of the label nodes
  vp_height <- nodes_coord$max_height
  vp_width <- nodes_coord$max_width
  nodes_coord$y <- (vp_height - nodes_coord$y)/vp_height

  if(has_labels){
    vp_width <- sum(label_coord$width[1], vp_width)

    nodes_coord$x <- (nodes_coord$x + label_coord$width[1])/vp_width

    # Convert label x from char units to NPC so they scale with the viewport
    label_coord$x <- label_coord$x / vp_width

  }else{
    nodes_coord$x <- (nodes_coord$x)/vp_width
  }
  
  # Change nodes coordinates
  nodes <- sapply(names(consort_plot), function(i){
    r <- move_box(consort_plot[[i]]$box,
                  x = unit(nodes_coord$x[i], "npc"),
                  y = unit(nodes_coord$y[i], "npc"))
    r$name <- i
    
    # Skip empty side box
    if(is_empty(consort_plot[[i]]$text))
      return(NULL)
      
    return(r)
  }, simplify = FALSE)
  
  grobs_list <- Filter(Negate(is.null), nodes)

  # Connections
  for(i in seq_along(nodes_connect)){
    nd <- nodes_connect[[i]]

    if(is.null(nodes[[nd$node[1]]]))
      next

    for(j in 2:length(nd$node)){
      if(is.null(nodes[[nd$node[j]]])){
        nd_name <- nodes_connect[[nd$node[j]]]$node[2]
      }else{
        nd_name <- nd$node[j]
      }
      connect_gb <- connect_box(nodes[[nd_name]], nodes[[nd$node[1]]],
                                connect = nd$connect, type = "p")
      grobs_list[[length(grobs_list) + 1L]] <- connect_gb
    }
  }

  if(has_labels){

    # Align labels
    for(i in seq_along(label_plot)){
      nam <- names(label_plot)[i]
      r <- move_box(label_plot[[nam]]$box,
                    x = unit(label_coord$x[nam], "npc"),
                    y = unit(label_coord$y[nam], "npc"))
      r$name <- nam

      grobs_list[[length(grobs_list) + 1L]] <- r
    }

  }

  grobs_list <- do.call(gList, unname(grobs_list))

  # Absolute (device-independent) viewport sized to exactly match the
  # layout, so box positions (npc fractions computed above) and box sizes
  # (fixed "char" units) stay on the same scale -- the diagram is drawn at
  # its natural size (post-shrink, if any) and centered, never stretched.
  size_in <- list(
    width  = convertWidth(unit(vp_width, "char"), "in", valueOnly = TRUE),
    height = convertHeight(unit(vp_height, "char"), "in", valueOnly = TRUE)
  )

  result <- grobTree(grobs_list,
           name = "consort",
           vp = viewport(width = unit(size_in$width, "in"),
                         height = unit(size_in$height, "in")))

  attr(result, "size") <- size_in
  result

}



