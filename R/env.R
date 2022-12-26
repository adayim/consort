# For indexing invisible node
pkgenv <- new.env(parent = emptyenv())
pkgenv$p <- 0

get_invs <- function() {
  pkgenv$p
}
set_invs <- function(value) {
  old <- pkgenv$p
  pkgenv$p <- value
  invisible(old)
}