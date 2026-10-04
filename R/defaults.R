
consort_global <- new.env(parent = emptyenv())

# Default settings for consort diagram 
consort_defaults_settings <- list(
    arrow_gp = gpar(col = "black", lwd = 1),
    txt_gp = gpar(cex = 1, col = "black"),
    box_gp = gpar(fill = "white"),
    label_txt_gp = gpar(col = "#4F81BD", cex = 1, fontface = "bold"),
    label_box_gp = gpar(fill = "#A9C7FD"),
    arrow_length = 0.1,
    arrow_type = "closed",
    pad_u = 3,
    bullet = "\u2022",
    parse_markup = FALSE
)

consort_global$defaults <- consort_defaults_settings

# Internal accessor for a single default value
#' @keywords internal
consort_opt <- function(name) {
  consort_global$defaults[[name]]
}

#' Set consort diagram default options
#'
#' Modify the default graphical parameters and other settings for consort diagrams.
#' Any parameter set to \code{NULL} (the default) will remain unchanged.
#' \code{\link[grid]{gpar}} arguments are merged with the current values, so
#' only the properties to change need to be given. Use
#' \code{init_consort_defaults()} to restore the package defaults, and
#' \code{get_consort_defaults()} to view the current settings.
#'
#' @param arrow_gp A \code{\link[grid]{gpar}} object for the arrow line (colour,
#'   line width, line type, etc.). Package default is
#'   \code{gpar(col = "black", lwd = 1)}.
#' @param txt_gp A \code{\link[grid]{gpar}} object for the text inside boxes
#'   (font size, colour, font family, etc.). Package default is
#'   \code{gpar(cex = 1, col = "black")}.
#' @param box_gp A \code{\link[grid]{gpar}} object for the box border and fill.
#'   Package default is \code{gpar(fill = "white")}.
#' @param label_txt_gp A \code{\link[grid]{gpar}} object for the label text.
#'   Package default is
#'   \code{gpar(col = "#4F81BD", cex = 1, fontface = "bold")}.
#' @param label_box_gp A \code{\link[grid]{gpar}} object for the label box.
#'   Package default is \code{gpar(fill = "#A9C7FD")}.
#' @param arrow_length Numeric, length of the arrowhead in inches. Default is
#'   \code{0.1}.
#' @param arrow_type Character, arrow type: \code{"closed"} (default) or
#'   \code{"open"}.
#' @param pad_u Numeric, padding between nodes in character units. Default is
#'   \code{3}.
#' @param bullet Character, bullet character for side box items, e.g. an en dash
#'   or a hyphen. Default is the Unicode bullet (U+2022).
#' @param parse_markup Logical, whether to parse lightweight markup syntax
#'   (\code{**bold**}, \code{*italic*}, \code{^{superscript}},
#'   \code{_{subscript}}, \code{__underline__}) in node labels.
#'   Default is \code{FALSE}.
#'
#' @return Invisibly returns the previous defaults (a \code{consort_defaults} object).
#' @export
#' @examples
#' # Change text color and box fill
#' old <- set_consort_defaults(
#'   txt_gp = grid::gpar(col = "navy", cex = 0.9),
#'   box_gp = grid::gpar(fill = "#F0F0F0")
#' )
#'
#' # View current defaults
#' get_consort_defaults()
#'
#' # Restore previous defaults
#' set_consort_defaults(
#'   txt_gp = old$txt_gp,
#'   box_gp = old$box_gp
#' )
set_consort_defaults <- function(
    arrow_gp = NULL,
    txt_gp = NULL,
    box_gp = NULL,
    label_txt_gp = NULL,
    label_box_gp = NULL,
    arrow_length = NULL,
    arrow_type = NULL,
    pad_u = NULL,
    bullet = NULL,
    parse_markup = NULL
) {

  old <- get_consort_defaults()

  # Map parameter names to option names
  args <- list(
    arrow_gp     = arrow_gp,
    txt_gp       = txt_gp,
    box_gp       = box_gp,
    label_txt_gp = label_txt_gp,
    label_box_gp = label_box_gp,
    arrow_length = arrow_length,
    arrow_type   = arrow_type,
    pad_u        = pad_u,
    bullet       = bullet,
    parse_markup = parse_markup
  )

  # Keep only non-NULL arguments
  args <- Filter(Negate(is.null), args)

  if (length(args) == 0) return(invisible(old))

  # Validate and merge gpar arguments into existing defaults
  gpar_params <- c("arrow_gp", "txt_gp", "box_gp",
                    "label_txt_gp", "label_box_gp")
  for (nm in intersect(names(args), gpar_params)) {
    if (!inherits(args[[nm]], "gpar"))
      stop(sprintf("`%s` must be a gpar() object.", nm))
    args[[nm]] <- do.call(gpar,
                          utils::modifyList(as.list(consort_global$defaults[[nm]]),
                                            as.list(args[[nm]])))
  }

  if (!is.null(arrow_length)) {
    if (!is.numeric(arrow_length) || length(arrow_length) != 1 || is.na(arrow_length))
      stop("`arrow_length` must be a single numeric value.")
  }

  if (!is.null(arrow_type)) {
    arrow_type <- match.arg(arrow_type, c("closed", "open"))
    args$arrow_type <- arrow_type
  }

  if (!is.null(pad_u)) {
    if (!is.numeric(pad_u) || length(pad_u) != 1 || is.na(pad_u))
      stop("`pad_u` must be a single numeric value.")
  }

  if (!is.null(bullet)) {
    if (!is.character(bullet) || length(bullet) != 1)
      stop("`bullet` must be a single character string.")
  }

  if (!is.null(parse_markup)) {
    if (!is.logical(parse_markup) || length(parse_markup) != 1 || is.na(parse_markup))
      stop("`parse_markup` must be a single TRUE/FALSE value.")
  }

  # Update stored defaults
  consort_global$defaults <- utils::modifyList(consort_global$defaults, args)

  invisible(old)
}

#' Get consort diagram default options
#'
#' @return A \code{consort_defaults} object containing all current default settings.
#' @export
#' @rdname set_consort_defaults
#' @examples
#' get_consort_defaults()
get_consort_defaults <- function() {
  x <- consort_global$defaults
  class(x) <- "consort_defaults"
  x
}

#' @rdname set_consort_defaults
#' @export
init_consort_defaults <- function() {
  x <- consort_defaults_settings
  consort_global$defaults <- x
  class(x) <- "consort_defaults"
  invisible(x)
}

#' @param x A \code{consort_defaults} object.
#' @param ... Not used.
#' @rdname set_consort_defaults
#' @export
print.consort_defaults <- function(x, ...) {
  cat("Consort diagram default settings:\n\n")

  for (nm in names(x)) {
    label <- nm
    val <- x[[nm]]

    if (inherits(val, "gpar")) {
      parts <- vapply(names(val), function(p) {
        v <- val[[p]]
        if (is.numeric(v) && !is.null(names(v))) {
          # gpar stores fontface as named numeric (e.g., font = c(bold = 2))
          sprintf('%s = "%s"', p, names(v))
        } else if (is.character(v)) {
          sprintf('%s = "%s"', p, v)
        } else {
          sprintf("%s = %s", p, v)
        }
      }, character(1))
      cat(sprintf("  %-14s: gpar(%s)\n", label, paste(parts, collapse = ", ")))
    } else if (is.character(val)) {
      cat(sprintf("  %-14s: \"%s\"\n", label, val))
    } else {
      cat(sprintf("  %-14s: %s\n", label, val))
    }
  }

  invisible(x)
}



