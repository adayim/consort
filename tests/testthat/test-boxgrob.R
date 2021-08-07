
node1 <- add_box(txt = "Screened (n=100)")

test_that("Add side box", {
  
  expect_error(add_side_box(prev_box = node1, txt = c("Exluded A", "Excluded B")))

})

node3 <- add_side_box(node1, txt = "Excluded (n=15):\n\u2022 MRI not collected (n=3)")    

test_that("Add box function", {
  
  expect_error(add_box(prev_box = node1, txt = c("Arm A (n=100)", "Arm B (n=100")))



})
