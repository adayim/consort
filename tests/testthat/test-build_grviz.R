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
  
  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
  expect_snapshot_file(save_png(g), "build-grviz.png")
  
})


test_that("New options", {
  
  set_consort_defaults(
    arrow_gp = gpar(col = "green"),
    label_txt_gp = gpar(col = "red")
  )
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
  expect_snapshot_file(to_grviz(txt), "grviz-withopts.gv")
  
  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
  expect_snapshot_file(save_png(g), "build-grviz-withopts.png")
  
})

init_consort_defaults()

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
  
  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
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
  
  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
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


spines_of <- function(g) {
  layout <- attr(g, "nodes.list")
  types <- sapply(layout, function(x) unique(sapply(g[x], "[[", "node_type")))
  unlist(assign_spines(g, layout, types))
}

test_that("spines keep chains straight and centre odd splits", {
  g <- add_box(txt = "Population")
  g <- add_split(g, txt = c("A", "B", "C"))
  g <- add_box(g, txt = c("A2", "B2", "C2"))
  sp <- spines_of(g)

  # A single child and the middle of an odd split continue the parent's line
  expect_equal(sp[["node3"]], sp[["node1"]])
  expect_equal(sp[["node5"]], sp[["node2"]])
  expect_equal(sp[["node6"]], sp[["node3"]])
  expect_equal(sp[["node7"]], sp[["node4"]])
  expect_equal(length(unique(sp[c("node2", "node3", "node4")])), 3)
})

test_that("an even split starts a new spine for every child", {
  g <- add_box(txt = "Population")
  g <- add_split(g, txt = c("A", "B"))
  sp <- spines_of(g)
  expect_equal(length(unique(sp)), 3)
})

test_that("a merge continues the middle parent's spine", {
  odd <- add_box(txt = c("S1", "S2", "S3"))
  odd <- add_box(odd, txt = "All")
  sp <- spines_of(odd)
  expect_equal(sp[["node4"]], sp[["node2"]])

  even <- add_box(txt = c("S1", "S2"))
  even <- add_box(even, txt = "All")
  sp <- spines_of(even)
  expect_false(sp[["node3"]] %in% sp[c("node1", "node2")])
})

test_that("a side box shares the spine of its anchor", {
  g <- add_box(txt = "Population")
  g <- add_side_box(g, txt = "Excluded")
  g <- add_box(g, txt = "Randomized")
  sp <- spines_of(g)
  expect_equal(sp[["node2"]], sp[["node1"]])
})

test_that("Nested splits deeper than two levels", {
  side_txt <- "Excluded (n=3):\n\u2022 Reason one\n\u2022 Reason two"
  g <- add_box(txt = "Population (n=300)")
  g <- add_split(g, txt = c("Region A", "Region B"))
  g <- add_split(g, txt = list(c("Site A1", "Site A2"),
                               c("Site B1", "Site B2", "Site B3")))
  g <- add_split(g, txt = list(c("Arm A1x", "Arm A1y"), "Arm A2x",
                               c("Arm B1x", "Arm B1y"),
                               c("Arm B2x long label", "Arm B2y"), "Arm B3x"))
  g <- add_side_box(g, txt = rep(c("Lost (n=1)", side_txt), length.out = 8))
  g <- add_box(g, txt = paste("Analysed", 1:8))

  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "nested-split-grviz.gv")
})
