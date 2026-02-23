#' Create a CONSORT diagram style
#'
#' @description
#' Define styling options for CONSORT diagrams including line properties,
#' arrow styles, box appearance, and layout spacing. This addresses the
#' customization needs from the original package's Issue #28.
#'
#' @param line_width Numeric line width (default: 1)
#' @param line_color Character line color (default: "black")
#' @param line_style Character line style: "solid", "dashed", "dotted" (default: "solid")
#' @param arrow_size Numeric arrow size multiplier (default: 1)
#' @param arrow_type Character arrow type: "closed", "open", "curved" (default: "closed")
#' @param box_fill Character box fill color (default: "white")
#' @param box_border Character box border color (default: "black")
#' @param box_padding Numeric padding inside boxes (default: 0.3)
#' @param box_radius Numeric corner radius for rounded boxes (default: 0.1)
#' @param text_size Numeric text size in points (default: 10)
#' @param text_color Character text color (default: "black")
#' @param text_family Character font family (default: "sans")
#' @param node_spacing Numeric vertical spacing between nodes (default: 0.5)
#' @param arm_spacing Numeric horizontal spacing between arms (default: 1.0)
#'
#' @return A consort_style object (list with class "consort_style")
#' @export
#'
#' @examples
#' # Default style
#' style <- consort_style()
#'
#' # Custom style
#' style <- consort_style(
#'   line_width = 2,
#'   line_color = "darkblue",
#'   arrow_size = 1.5,
#'   box_fill = "lightgray"
#' )
#'
#' # Set as global default
#' options(consort.style = consort_style(line_width = 2))
consort_style <- function(
  line_width = 1,
  line_color = "black",
  line_style = c("solid", "dashed", "dotted"),
  arrow_size = 1,
  arrow_type = c("closed", "open", "curved"),
  box_fill = "white",
  box_border = "black",
  box_padding = 0.3,
  box_radius = 0.1,
  text_size = 10,
  text_color = "black",
  text_family = "sans",
  node_spacing = 0.5,
  arm_spacing = 1.0
) {
  line_style <- match.arg(line_style)
  arrow_type <- match.arg(arrow_type)

  style <- list(
    line = list(
      width = line_width,
      color = line_color,
      style = line_style
    ),
    arrow = list(
      size = arrow_size,
      type = arrow_type
    ),
    box = list(
      fill = box_fill,
      border = box_border,
      padding = box_padding,
      radius = box_radius
    ),
    text = list(
      size = text_size,
      color = text_color,
      family = text_family
    ),
    layout = list(
      node_spacing = node_spacing,
      arm_spacing = arm_spacing
    )
  )

  class(style) <- c("consort_style", "list")
  style
}

#' Get default or user-specified style
#'
#' @param style Optional consort_style object. If NULL, uses global option or default.
#' @return A consort_style object
#' @keywords internal
get_style <- function(style = NULL) {
  if (!is.null(style)) {
    if (!inherits(style, "consort_style")) {
      stop("Style must be created with consort_style()", call. = FALSE)
    }
    return(style)
  }

  # Check global option
  global_style <- getOption("consort.style", default = NULL)
  if (!is.null(global_style)) {
    return(global_style)
  }

  # Return default
  consort_style()
}
