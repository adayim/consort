
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

data(dispos.data)

test_that("Multiple split generate", {

  p <- consort_plot(data = dispos.data,
                    orders = c(trialno = "Population",
                               exc    = "Excluded",
                               arm     = "Randomized patient",
                               arm3     = "",
                               fow1    = "Lost of Follow-up",
                               trialno = "Finished Followup",
                               fow2    = "Not evaluable",
                               trialno = "Final Analysis"),
                    side_box = c("exc", "fow1", "fow2"),
                    allocation = c("arm", "arm3"))

  txt <- build_grviz(p)
  expect_snapshot_file(to_grviz(txt), "multiple-split.gv")

  skip_if_not(tolower(.Platform$OS.type) == "windows")
  expect_snapshot_file(save_png(p), "multiple-split.png")

})

