library(testthat)
library(consort)

test_check("consort")


to_grviz <- function(x) {
  path <- tempfile(fileext = ".gv")
  cat(x, file = path)
  path
}

