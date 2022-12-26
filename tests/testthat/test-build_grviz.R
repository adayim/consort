to_grviz <- function(x) {
  path <- tempfile(fileext = ".gv")
  x <- gsub("\u2022", "", x) 
  cat(x, file = path)
  path
}

save_png <- function(x, width = 800, height = 800) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  plot(x)
  path
}

test_that("Check plot creation", {
  g <- add_box(txt = c("Study 1 (n=8)", "Study 2 (n=12)"))
  g <- add_box(g, txt = "Included All (n=20)")
  g <- add_side_box(g, txt = "Excluded (n=7):\n\u2022 MRI not collected (n=3)")
  g <- add_box(g, txt = "Randomised")
  g <- add_split(g, txt = c("Arm A (n=143)", "Arm B (n=142)"))
  g <- add_box(g, txt = c("Follow-up (n=20)",
                          "Follow-up (n=7)"))
  
  g <- add_side_box(g, txt = c("Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)",
                               "Excluded (n=7):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)"))
  
  g <- add_box(g, txt = c("Final analysis (n=128)", "Final analysis (n=135)"))
  
  g <- add_label_box(g,
                     txt = c("1" = "Screening", "3" = "Randomized", "6" = "Final analysis"))

  expect_snapshot_file(save_png(g), "build-grviz.png")

  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "grviz.gv")
  
})
