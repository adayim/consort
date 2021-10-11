# Plot checking helper
save_png <- function(p, width = 9, height = 5) {
  path <- tempfile(fileext = ".png")
  ggplot2::ggsave(filename = path, p, width = width, height = height)
  path
}

set.seed(1001)
N <- 300

trialno <- sample(c(1000:2000), N)
exc <- rep(NA, N)
exc[sample(1:N, 15)] <- sample(c("Sample not collected", "MRI not collected",
                                  "Other"), 15, replace = T, prob = c(0.4, 0.4, 0.2))
arm <- rep(NA, N)
arm[is.na(exc)] <- sample(c("Conc", "Seq"), sum(is.na(exc)), replace = T)

fow1 <- rep(NA, N)
fow1[!is.na(arm)] <- sample(c("Withdraw", "Discontinued", "Death", "Other", NA),
                            sum(!is.na(arm)), replace = T, 
                            prob = c(0.05, 0.05, 0.05, 0.05, 0.8))
fow2 <- rep(NA, N)
fow2[!is.na(arm) & is.na(fow1)] <- sample(c("Protocol deviation", "Outcome missing", NA),
                                          sum(!is.na(arm) & is.na(fow1)), replace = T, 
                                          prob = c(0.05, 0.05, 0.9))

df <- data.frame(trialno, exc, arm, fow1, fow2)

test_that("Auto generate", {
  g <- consort_plot(data = df,
                    orders = c(trialno = "Population",
                               exc     = "Excluded",
                               arm     = "Allocated",
                               fow1    = "Lost of Follow-up",
                               trialno = "Finished Followup",
                               fow2    = "Not evaluable for the final analysis",
                               trialno = "Final Analysis"),
                    allocation = "arm",
                    side_box = c("exc", "fow1", "fow2"),
                    labels = c("1" = "Screening", "2" = "Randomization",
                               "5" = "Final"),
                    cex = 0.9)
  
  expect_snapshot_file(save_png(g, width = 9, height = 8), "auto_text.png")
})
