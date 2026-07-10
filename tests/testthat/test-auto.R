
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
  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "auto-grviz.gv")

  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
  expect_snapshot_file(save_png(g), "autogen.png")

})

test_that("Allocation last node", {
  r <- readRDS('dat2.rds')
  r$qual[!is.na(r$exc)] <- NA
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
  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "auto-last-grviz.gv")
  
  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
  expect_snapshot_file(save_png(g), "autogen-last.png")
  
})


test_that("Allocation no label", {

  r <- readRDS('dat2.rds')
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
  
  txt <- build_grviz(g)
  expect_snapshot_file(to_grviz(txt), "auto-nolab-grviz.gv")

  # skip_if_not(tolower(.Platform$OS.type) == "windows")
  skip_on_ci()
  expect_snapshot_file(save_png(g), "autogen-nolab.png")

})

test_that("Zero-count factor levels kept with drop_levels = FALSE (issue #25)", {

  set.seed(1001)
  n <- 200
  df <- data.frame(
    id = sample(1:n, n),
    exclusion = factor(sample(c(NA, 1, 2), n, replace = TRUE,
                              prob = c(0.9, 0.05, 0.05)),
                       levels = c(1, 2, 3),
                       labels = c("Ineligible", "Declined", "Other"))
  )

  ords <- c(
    "id" = "Enrolled",
    "exclusion" = "Excluded",
    "id" = "Randomized"
  )

  # Default: zero-count level omitted (previous behaviour)
  g_def <- consort_plot(df, orders = ords, side_box = "exclusion")
  side_def <- g_def[["node2"]]$text
  expect_false(grepl("Other", side_def))

  # drop_levels = FALSE: zero-count level reported as (n=0)
  g_keep <- consort_plot(df, orders = ords, side_box = "exclusion",
                         drop_levels = FALSE)
  side_keep <- g_keep[["node2"]]$text
  expect_true(grepl("Other (n=0)", side_keep, fixed = TRUE))
  expect_true(grepl("Ineligible", side_keep))

  # A side box with no exclusions at all is drawn when levels are kept
  df_none <- df
  df_none$exclusion[] <- NA
  g_none <- consort_plot(df_none, orders = ords, side_box = "exclusion",
                         drop_levels = FALSE)
  side_none <- g_none[["node2"]]$text
  expect_true(grepl("Excluded (n=0)", side_none, fixed = TRUE))
  expect_true(grepl("Other (n=0)", side_none, fixed = TRUE))

  # ... and stays hidden by default
  g_none_def <- consort_plot(df_none, orders = ords, side_box = "exclusion")
  expect_true(is_empty(g_none_def[["node2"]]$text))
})
