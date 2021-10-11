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
  
  node1 <- add_box(txt = txt1)
  
  node3 <- add_side_box(node1, txt = txt1_side)    
  
  node4 <- add_box(node3, txt = "Randomized (n=200)")
  
  node1_sp <- add_split(node4, txt = c("Arm A (n=100)", "Arm B (n=100)"))
  side1_sp <- add_side_box(node1_sp, txt = c("Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)",
                                             ""))
  
  # Multiple split
  expect_error(add_split(node1_sp, txt = c("Arm A (n=100)", "Arm B (n=100)")))
  # Length 1
  expect_error(add_split(node3, txt = "Arm A (n=100)"))
  
  # Continue plotting
  node2_sp <- add_box(side1_sp, txt = c("Final analysis (n=85)", "Final analysis (n=100)"))
  
  lab1 <- add_label_box(node1, txt = "Screening")
  lab2 <- add_label_box(node4, txt = "Randomized")
  lab3 <- add_label_box(node2_sp, txt = "Final analysis")
  
  g <- build_consort(list(node1,
                          node3,
                          node4,
                          node1_sp,
                          side1_sp,
                          node2_sp),
                     list(lab1, lab2, lab3))
  expect_snapshot_file(save_png(g, width = 8, height = 5), "full_text.png")
})
