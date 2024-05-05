to_grviz <- function(x) {
  path <- tempfile(fileext = ".gv")
  x <- gsub("\u2022", "", x) 
  cat(x, file = path)
  path
}

save_png <- function(x, width = 9, height = 9) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height, 
      units = "in", type = "cairo-png", res = 300)
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

  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "grviz.gv")
  
  skip_if_not(tolower(.Platform$OS.type) == "windows")
  expect_snapshot_file(save_png(g), "build-grviz.png")
  
})


test_that("Missing in some nodes", {
  g <- add_box(txt = c("Study 1 (n=8)", "Study 2 (n=12)"))
  g <- add_box(g, txt = "Included All (n=20)")
  g <- add_side_box(g, txt = "Excluded (n=7):\n\u2022 MRI not collected (n=3)")
  g <- add_box(g, txt = "Randomised")
  g <- add_split(g, txt = c("Arm A (n=143)", "Arm B (n=142)"))
  g <- add_side_box(g, txt = c("", "Exclude (n=3"))
  g <- add_box(g, txt = c("", "From Arm B"))
  g <- add_box(g, txt = c("This is it", "From Arm B"))

  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "multi-miss-grviz.gv")
  
  skip_if_not(tolower(.Platform$OS.type) == "windows")
  expect_snapshot_file(save_png(g), "multi-miss-grviz.png")
  
})


test_that("End with missing", {
  g <- add_box(txt = c("Study 1 (n=8)", "Study 2 (n=12)"))
  g <- add_box(g, txt = "Included All (n=20)")
  g <- add_side_box(g, txt = "Excluded (n=7):\n\u2022 MRI not collected (n=3)")
  g <- add_box(g, txt = "Randomised")
  g <- add_split(g, txt = c("Arm A (n=143)", "Arm B (n=142)"))
  g <- add_side_box(g, txt = c("", "Exclude (n=3"))
  g <- add_box(g, txt = c("", "From Arm B"))
  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "end-miss-grviz.gv")
  
  skip_if_not(tolower(.Platform$OS.type) == "windows")
  expect_snapshot_file(save_png(g), "end-miss-grviz.png")
  
})


test_that("Split and combine", {
  g <- add_box(txt = c("Study 1 (n=8)", "Study 2 And this is long (n=12)", "Study 3 (n=12)", "Study 3 (n=12)", "Study 3 (n=12)"))
  g <- add_box(g, txt = "Included All (n=20)")
  g <- add_side_box(g, txt = "Excluded (n=7):\n\u2022 MRI not collected (n=3)")
  g <- add_box(g, txt = "Randomised")
  g <- add_split(g, txt = c("Arm A (n=143)", "Arm B (n=142)"))
  g <- add_box(g, txt = c("", "From Arm B"))
  g <- add_box(g, txt = "Combine all")
  g <- add_split(g, txt = list(c("Process 1 (n=140)", "Process 2 (n=140)", "Process 3 (n=142)")))
  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "split-comb-grviz.gv")
  
  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  # expect_snapshot_file(save_png(g), "split-comb-grviz.png")
  
})


test_that("Empty in the middle", {
  g <- add_box(
    txt = c("Cohort 1 (n=6)",
            "Cohort 2 (n=6)",
            "Cohort 3 (n=6)")
  ) |> 
    add_side_box(
      txt = c("Excluded (n=1)",
              "Excluded (n=3)",
              "")
    ) |>
    add_box(
      txt = c("Cohort 1 (n=5)",
              "Cohort 2 (n=3)",
              "")
    ) |> 
    add_box(
      txt = c("Total (n=14)")
    )  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "empty-middle-grviz.gv")
  
  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  # expect_snapshot_file(save_png(g), "split-comb-grviz.png")
  
})
