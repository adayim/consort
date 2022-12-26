
test_that("Generate text for the consort", {
  val <- data.frame(
    am = factor(ifelse(mtcars$am == 1, "Automatic", "Manual"), ordered = TRUE),
    vs = factor(ifelse(mtcars$vs == 1, "Straight", "V-shaped"), ordered = TRUE),
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
    strsplit(tx2, "\n\u2022 ")[[2]][-1],
    paste0(names(tab1), " (n=", tab1, ")")
  )

  expect_equal(tx3, paste0(names(tab2), " (n=", tab2, ")"))

  expect_equal(tx4, "")
  
  # data.frame
  tx5 <- gen_text(split(val[,c("vs", "car")], val$am),
                  label = "Cars in the data", bullet = FALSE)
  tx5_ulst <- strsplit(tx5, "\n\u2022 ")[[2]]
  tab5 <- table(val$vs, val$am)
  expect_equal(
    tx5_ulst[1],
    paste0("Cars in the data (n=", sum(tab5[,2]), ")\nStraight (n=", 
           sum(tab5["Straight", 2]), "):")
  )
  
  tx6 <- gen_text(val[,c("vs", "car")], 
                  label = "Cars in the data", bullet = FALSE)
  tab6 <- table(val$vs, val$car)
  tx6_ulst <- strsplit(tx6, "\n\u2022 ")[[1]]
  
  expect_equal(
    tx6_ulst[1],
    paste0("Cars in the data (n=", nrow(mtcars), ")\nV-shaped (n=", 
           sum(tab6["V-shaped",]), "):")
  )
  
})
