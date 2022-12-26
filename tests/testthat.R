library(testthat)
library(consort)

test_check("consort")


# vdiffr always fails on MAC and Ubuntu, don't know why
to_grviz <- function(x) {
  path <- tempfile(fileext = ".gv")
  cat(x, file = path)
  path
}

