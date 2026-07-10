
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
  
  tx7 <- gen_text(split(val$car, val$am),
                  bullet = TRUE
  )
  
  expect_equal(tx7, sub(".*?\\n", "", tx2))
  
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
           sum(tab5["Straight", 2]), ")")
  )
  
  tx6 <- gen_text(val[,c("vs", "car")], 
                  label = "Cars in the data", bullet = FALSE)
  tab6 <- table(val$vs, val$car)
  tx6_ulst <- strsplit(tx6, "\n\u2022 ")[[1]]
  
  expect_equal(
    tx6_ulst[1],
    paste0("Cars in the data (n=", nrow(mtcars), ")\nV-shaped (n=",
           sum(tab6["V-shaped",]), ")")
  )


})

test_that("drop_levels keeps or drops zero-count factor levels", {
  b <- "\u2022"
  reason <- factor(c("Ineligible", NA, "Declined", NA, "Ineligible"),
                   levels = c("Ineligible", "Declined", "Other"))

  # Default: unused level is dropped (previous behaviour)
  tx_drop <- gen_text(reason, label = "Excluded", bullet = TRUE)
  expect_false(grepl("Other", tx_drop))
  expect_equal(
    tx_drop,
    sprintf("Excluded (n=3)\n%s Ineligible (n=2)\n%s Declined (n=1)", b, b)
  )

  # drop_levels = FALSE: zero-count level reported as (n=0)
  tx_keep <- gen_text(reason, label = "Excluded", bullet = TRUE,
                      drop_levels = FALSE)
  expect_equal(
    tx_keep,
    sprintf("Excluded (n=3)\n%s Ineligible (n=2)\n%s Declined (n=1)\n%s Other (n=0)",
            b, b, b)
  )

  # Without a label
  expect_equal(
    gen_text(reason, bullet = FALSE, drop_levels = FALSE),
    c("Ineligible (n=2)", "Declined (n=1)", "Other (n=0)")
  )

  # All-NA factor: hidden by default, full n=0 listing when levels kept
  none <- factor(rep(NA_character_, 4),
                 levels = c("Ineligible", "Declined"))
  expect_equal(gen_text(none, label = "Excluded", bullet = TRUE), "")
  expect_equal(
    gen_text(none, label = "Excluded", bullet = TRUE, drop_levels = FALSE),
    sprintf("Excluded (n=0)\n%s Ineligible (n=0)\n%s Declined (n=0)", b, b)
  )

  # Character input is unaffected by the flag
  chr <- c("A", NA, "B")
  expect_equal(
    gen_text(chr, label = "Excluded", bullet = TRUE),
    gen_text(chr, label = "Excluded", bullet = TRUE, drop_levels = FALSE)
  )
  expect_equal(
    gen_text(rep(NA_character_, 3), bullet = TRUE, drop_levels = FALSE),
    ""
  )

  # Nested reasons in a data.frame: zero-count group reported as count only
  df <- data.frame(
    grp = factor(c("Ineligible", "Ineligible", NA),
                 levels = c("Ineligible", "Other")),
    why = c("Age", "Consent", NA)
  )
  tx_df <- gen_text(df, label = "Excluded", drop_levels = FALSE)
  expect_true(grepl("Other (n=0)", tx_df, fixed = TRUE))
  expect_false(grepl("Other", gen_text(df, label = "Excluded")))

  # Validation
  expect_error(gen_text(chr, drop_levels = NA), "drop_levels")
})
