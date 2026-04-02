
test_that("set_consort_defaults merges gpar values", {

  old <- set_consort_defaults(txt_gp = gpar(lwd = 1))
  on.exit(consort_global$defaults <- old, add = TRUE)

  new_txt <- consort_opt("txt_gp")
  # lwd was added
  expect_equal(new_txt$lwd, 1)
  # existing col was preserved
  expect_equal(new_txt$col, "black")

  set_consort_defaults(arrow_gp = gpar(col = "red"))
  new_arrow <- consort_opt("arrow_gp")
  # col was updated
  expect_equal(new_arrow$col, "red")
  # existing lwd was preserved
  expect_equal(new_arrow$lwd, 1)

})
