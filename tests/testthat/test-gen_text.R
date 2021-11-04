
test_that("Generate text for the consort", {
  val <- data.frame(
    am = factor(ifelse(mtcars$am == 1, "Automatic", "Manual")),
    car = row.names(mtcars)
  )
  tab1 <- table(val$car[val$am == "Manual"])
  tab2 <- table(val$am)

  tx1 <- gen_text(val$car, label = "Cars in the data")
  tx2 <- gen_text(split(val$car, val$am),
    label = "Cars in the data",
    bullet = TRUE
  )
  tx3 <- gen_text(val$am)
  tx4 <- gen_text(rep("", 4), bullet = TRUE)

  # Numbers equal
  expect_equal(
    as.numeric(gsub("[^\\d]+", "", tx1, perl = TRUE)),
    nrow(val)
  )

  expect_length(tx2, 2)
  expect_equal(
    strsplit(tx2, "\n\u2022 ")$Manual[-1],
    paste0(names(tab1), " (n=", tab1, ")")
  )

  expect_equal(tx3, paste0(names(tab2), " (n=", tab2, ")"))

  expect_equal(tx4, "")
})
