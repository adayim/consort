#' Create a CONSORT diagram object
#'
#' @description
#' Creates the foundation for a CONSORT (Consolidated Standards of Reporting Trials)
#' diagram object that can be used to visualize participant flow in clinical trials.
#'
#' @param data Optional data frame containing trial disposition data.
#' @param metadata Optional named list of additional metadata for the diagram.
#'
#' @return A consort object (S3 class)
#' @export
#'
#' @examples
#' # Create empty consort object
#' diagram <- consort()
#'
#' # With metadata
#' diagram <- consort(metadata = list(title = "My Trial"))
consort <- function(data = NULL, metadata = list()) {
  obj <- list(
    data = data,
    nodes = list(),
    edges = list(),
    style = consort_style(),
    metadata = metadata,
    version = "2.0.0"
  )

  class(obj) <- c("consort", "list")
  obj
}

#' Check if object is a consort diagram
#'
#' @param x An object to test
#' @return Logical
#' @export
is.consort <- function(x) {
  inherits(x, "consort")
}

#' Validate a consort object
#'
#' @param x A consort object
#' @return The consort object (invisibly) or error
#' @keywords internal
validate_consort <- function(x) {
  if (!is.consort(x)) {
    stop("Object must be of class 'consort'", call. = FALSE)
  }

  # Validate structure
  required_fields <- c("data", "nodes", "edges", "style", "metadata", "version")
  missing <- setdiff(required_fields, names(x))

  if (length(missing) > 0) {
    stop(
      "Consort object is missing required fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(x)
}
