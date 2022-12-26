
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

test_that("Auto generate", {

  df <- readRDS('dat1.rds')
  g <- consort_plot(
    data = df,
    orders = c(
      trialno = "Population",
      exc = "Excluded",
      arm = "Allocated",
      fow1 = "Lost of Follow-up",
      trialno = "Finished Followup",
      fow2 = "Not evaluable for the final analysis",
      trialno = "Final Analysis"
    ),
    allocation = "arm",
    side_box = c("exc", "fow1", "fow2"),
    labels = c(
      "1" = "Screening", "2" = "Randomization",
      "5" = "Final"
    ),
    cex = 0.9
  )

  expect_s3_class(g, "consort")

  expect_snapshot_file(save_png(g), "autogen.png")
  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "auto-grviz.gv")


})

test_that("Allocation last node", {
  r <- readRDS('dat2.rds')
  g <- consort_plot(r,
                    orders = c(id      = 'Screened',
                               exc     = 'Excluded',
                               qual    = 'Qualified for Randomization',
                               consent = 'Consented',
                               tx      = 'Randomized'),
                    side_box   = 'exc',
                    allocation = 'tx',
                    labels=c('1'='Screening', '2'='Consent') #, '3'='Randomization')
  )
  expect_s3_class(g, "consort")

  expect_snapshot_file(save_png(g), "autogen-last.png")
  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "auto-last-grviz.gv")

  
  # No label
  g <- consort_plot(r,
                    orders = c(id      = 'Screened',
                               exc     = 'Excluded',
                               qual    = 'Qualified for Randomization',
                               consent = 'Consented',
                               tx      = 'Randomized'),
                    side_box = 'exc',
                    allocation = 'tx'
  )

  expect_snapshot_file(save_png(g), "autogen-nolab.png")
  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "auto-nolab-grviz.gv")
  

})
