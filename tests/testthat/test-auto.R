set.seed(1001)
N <- 300

trialno <- sample(c(1000:2000), N)
exc <- rep(NA, N)
exc[sample(1:N, 15)] <- sample(c(
  "Sample not collected", "MRI not collected",
  "Other"
), 15, replace = T, prob = c(0.4, 0.4, 0.2))
arm <- rep(NA, N)
arm[is.na(exc)] <- sample(c("Conc", "Seq"), sum(is.na(exc)), replace = T)

fow1 <- rep(NA, N)
fow1[!is.na(arm)] <- sample(c("Withdraw", "Discontinued", "Death", "Other", NA),
                            sum(!is.na(arm)),
                            replace = T,
                            prob = c(0.05, 0.05, 0.05, 0.05, 0.8)
)
fow2 <- rep(NA, N)
fow2[!is.na(arm) & is.na(fow1)] <- sample(c("Protocol deviation", "Outcome missing", NA),
                                          sum(!is.na(arm) & is.na(fow1)),
                                          replace = T,
                                          prob = c(0.05, 0.05, 0.9)
)

df <- data.frame(trialno, exc, arm, fow1, fow2)

tmp_dir <- tempdir()

test_that("Auto generate", {
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

  vdiffr::expect_doppelganger("Auto generate", g)
  
  cat(build_grviz(g), file = file.path(tmp_dir, "auto-grviz.dot"))
  
  expect_true(compare_file_text(test_path("ref", "auto-grviz.dot"),
                                file.path(tmp_dir, "auto-grviz.dot")))

})

test_that("Allocation last node", {
  r <- read.csv("exmaple-data1.csv")
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
  
  vdiffr::expect_doppelganger("Allocation last node", g)
  
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
  
  vdiffr::expect_doppelganger("no label node", g)
  

})
