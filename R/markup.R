# Markup parsing and conversion utilities
#
# Supported markup syntax:
#   **bold**         -> bold text
#   *italic*         -> italic text
#   ^{superscript}   -> superscript text
#   _{subscript}     -> subscript text
#   __underline__    -> underlined text

# Check if markup parsing is enabled and text contains markup patterns
#' @keywords internal
has_markup <- function(text) {
  if (!isTRUE(consort_opt("parse_markup")))
    return(FALSE)
  if (is.null(text) || !is.character(text) || length(text) != 1 ||
      is.na(text) || !nzchar(text))
    return(FALSE)
  grepl("\\*\\*.+?\\*\\*|__.+?__|\\*.+?\\*|\\^\\{.+?\\}|_\\{.+?\\}",
        text, perl = TRUE)
}

# Parse markup text into a list of segments
# Each segment is list(text = "...", style = "plain"|"bold"|"italic"|...)
#' @keywords internal
parse_markup <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(list(list(text = "", style = "plain")))
  }

  # Combined pattern — order matters: ** before *, __ before _{}
  pattern <- "\\*\\*(.+?)\\*\\*|__(.+?)__|\\*(.+?)\\*|\\^\\{(.+?)\\}|_\\{(.+?)\\}"

  matches <- gregexpr(pattern, text, perl = TRUE)[[1]]

  if (matches[1] == -1) {
    return(list(list(text = text, style = "plain")))
  }

  match_starts  <- as.integer(matches)
  match_lengths <- attr(matches, "match.length")

  segments <- list()
  pos <- 1L

  for (i in seq_along(match_starts)) {
    # Plain text before this match
    if (match_starts[i] > pos) {
      segments <- c(segments, list(list(
        text  = substr(text, pos, match_starts[i] - 1L),
        style = "plain"
      )))
    }

    matched <- substr(text, match_starts[i],
                      match_starts[i] + match_lengths[i] - 1L)

    # Determine which alternative matched
    if (grepl("^\\*\\*(.+?)\\*\\*$", matched, perl = TRUE)) {
      inner <- sub("^\\*\\*(.+?)\\*\\*$", "\\1", matched, perl = TRUE)
      segments <- c(segments, list(list(text = inner, style = "bold")))

    } else if (grepl("^__(.+?)__$", matched, perl = TRUE)) {
      inner <- sub("^__(.+?)__$", "\\1", matched, perl = TRUE)
      segments <- c(segments, list(list(text = inner, style = "underline")))

    } else if (grepl("^\\*(.+?)\\*$", matched, perl = TRUE)) {
      inner <- sub("^\\*(.+?)\\*$", "\\1", matched, perl = TRUE)
      segments <- c(segments, list(list(text = inner, style = "italic")))

    } else if (grepl("^\\^\\{(.+?)\\}$", matched, perl = TRUE)) {
      inner <- sub("^\\^\\{(.+?)\\}$", "\\1", matched, perl = TRUE)
      segments <- c(segments, list(list(text = inner, style = "superscript")))

    } else if (grepl("^_\\{(.+?)\\}$", matched, perl = TRUE)) {
      inner <- sub("^_\\{(.+?)\\}$", "\\1", matched, perl = TRUE)
      segments <- c(segments, list(list(text = inner, style = "subscript")))
    }

    pos <- match_starts[i] + match_lengths[i]
  }

  # Remaining text after last match
  if (pos <= nchar(text)) {
    segments <- c(segments, list(list(
      text  = substr(text, pos, nchar(text)),
      style = "plain"
    )))
  }

  segments
}

# Split parsed segments into lines at \n boundaries in plain segments
#' @keywords internal
split_segments_by_newline <- function(segments) {
  lines        <- list()
  current_line <- list()

  for (seg in segments) {
    if (seg$style == "plain" && grepl("\n", seg$text, fixed = TRUE)) {
      # Sentinel preserves trailing empty strings dropped by strsplit
      parts <- strsplit(paste0(seg$text, "\a"), "\n", fixed = TRUE)[[1]]
      parts[length(parts)] <- sub("\a$", "", parts[length(parts)])

      for (j in seq_along(parts)) {
        if (j > 1L) {
          # Close current line
          if (length(current_line) == 0L)
            current_line <- list(list(text = "", style = "plain"))
          lines[[length(lines) + 1L]] <- current_line
          current_line <- list()
        }
        if (nzchar(parts[j])) {
          current_line[[length(current_line) + 1L]] <-
            list(text = parts[j], style = "plain")
        }
      }
    } else {
      current_line[[length(current_line) + 1L]] <- seg
    }
  }

  # Final line
  if (length(current_line) == 0L)
    current_line <- list(list(text = "", style = "plain"))
  lines[[length(lines) + 1L]] <- current_line

  lines
}

# Build a grid gpar for a given markup style, based on a base gpar
#' @keywords internal
segment_gpar <- function(style, base_gp) {
  if (is.null(base_gp)) base_gp <- gpar()
  gp <- base_gp
  if (style == "bold") {
    gp$fontface <- "bold"
  } else if (style == "italic") {
    gp$fontface <- "italic"
  } else if (style %in% c("superscript", "subscript")) {
    cex    <- if (is.null(gp$cex)) 1 else gp$cex
    gp$cex <- cex * 0.7
  }
  gp
}

# Convert markup text to HTML for Graphviz HTML-like labels
#' @keywords internal
markup_to_html <- function(text) {
  if (is.null(text) || !nzchar(text)) return(text)

  # Escape HTML special characters first
  text <- gsub("&", "&amp;",  text, fixed = TRUE)
  text <- gsub("<", "&lt;",   text, fixed = TRUE)
  text <- gsub(">", "&gt;",   text, fixed = TRUE)

  # Replace markup with HTML tags (order: ** before *, __ before _{})
  text <- gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>",     text, perl = TRUE)
  text <- gsub("__(.+?)__",         "<u>\\1</u>",      text, perl = TRUE)
  text <- gsub("\\*(.+?)\\*",       "<i>\\1</i>",      text, perl = TRUE)
  text <- gsub("\\^\\{(.+?)\\}",    "<sup>\\1</sup>",  text, perl = TRUE)
  text <- gsub("_\\{(.+?)\\}",      "<sub>\\1</sub>",  text, perl = TRUE)

  # Duplicate the space after bold/italic/underline closing tags: one copy

  # inside the tag (measured with the styled font metrics) and the original
  # outside.  Graphviz tends to underestimate node width when mixing styled
  # and plain text; the extra space compensates for the difference.
  text <- gsub("</b> ", " </b> ", text, fixed = TRUE)
  text <- gsub("</i> ", " </i> ", text, fixed = TRUE)
  text <- gsub("</u> ", " </u> ", text, fixed = TRUE)

  # Newlines to <br/>
  text <- gsub("\n", "<br/>", text, fixed = TRUE)

  text
}
