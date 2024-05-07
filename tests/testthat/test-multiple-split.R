
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
                               exclusion = "Excluded",
                               arm     = "Randomized patient",
                               arm3     = "",
                               subjid_notdosed = "Lost of Follow-up",
                               followup = "Followup-up",
                               lost_followup = "Lost to follow-up",
                               mitt = "Final Analysis"),
                    side_box = c("exclusion", "subjid_notdosed", "lost_followup"),
                    allocation = c("arm", "arm3"))

  txt <- build_grviz(p)
  expect_snapshot_file(to_grviz(txt), "multiple-split.gv")

  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
  expect_snapshot_file(save_png(p), "multiple-split.png")

})


test_that("Multiple split and no kickoff", {

  df <- dispos.data[!dispos.data$arm3 %in% "Trt C",]
  p <- consort_plot(data = df,
                  orders = list(c(trialno = "Population"),
                                c(exclusion = "Excluded"),
                                c(arm     = "Randomized patient"),
                                # The following two variables will be stacked together
                                c(arm3     = "", # Should not provide a value to show the actual arm
                                  subjid_notdosed="Participants not treated"),
                                # The following two variables will be stacked together
                                c(followup    = "Pariticpants planned for follow-up",
                                  lost_followup = "Reason for tot followed"),
                                c(assessed = "Assessed for final outcome"),
                                c(no_value = "Reason for not assessed"),
                                c(mitt = "Included in the mITT analysis")),
                  side_box = c("exclusion", "no_value"), 
                  allocation = c("arm", "arm3"), # Two level randomisation
                  labels = c("1" = "Screening", "2" = "Randomization",
                             "5" = "Follow-up", "7" = "Final analysis"),
                  kickoff_sidebox = FALSE,
                  cex = 0.7)

  txt <- build_grviz(p)
  expect_snapshot_file(to_grviz(txt), "multiple-split-nokick.gv")

  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
  expect_snapshot_file(save_png(p), "multiple-split-nokick.png")

})


