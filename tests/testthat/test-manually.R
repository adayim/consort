# Plot checking helper
save_png <- function(p, width, height) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height, units = "in", res = 300)
  on.exit(dev.off())
  p
  path
}

test_that("Generate consort manually", {
  txt1 <- "Population (n=300)"
  txt1_side <- "Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)"

  g <- add_box(txt = txt1)

  g <- add_side_box(g, txt = txt1_side)

  g <- add_box(g, txt = "Randomized (n=200)")

  g <- add_split(g, txt = c("Arm A (n=100)", "Arm B (n=100)"))
  g <- add_side_box(g, txt = c(
    "Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)",
    ""
  ))

  # Multiple split
  expect_error(add_split(g, txt = c("Arm A (n=100)", "Arm B (n=100)")))
  # Length 1
  expect_error(add_split(g, txt = "Arm A (n=100)"))

  # Continue plotting
  g <- add_box(g, txt = c("Final analysis (n=85)", "Final analysis (n=100)"))

  g <- add_label_box(g, txt = c(
    "1" = "Screening",
    "3" = "Randomized",
    "4" = "Final analysis"
  ))

  expect_s3_class(g, "consort")

  skip_on_cran()
  skip_on_ci()

  expect_snapshot_file(save_png(g, width = 8, height = 5),
    "full_text.png",
    compare = compare_file_text
  )
})
