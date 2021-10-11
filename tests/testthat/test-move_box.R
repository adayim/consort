
test_that("Box width and height", {
  bx1 <- textbox(text = "This is a test")
  bx2 <- move_box(bx1, x = 0.2)
  bx2_r <- move_box(bx1, x = -0.3, pos_type = "relative")
  bx3 <- move_box(bx1, y = 0.4)
  bx3_r <- move_box(bx1, y = -0.1, pos_type = "relative")
  bx4 <- move_box(bx1, x = 0.2, y = 0.4)
  bx5 <- textbox(text = "This is a test", x = 0.2, y = 0.4)

  
  expect_equal(get_coords(bx1)$y, get_coords(bx2)$y)
  expect_equal(get_coords(bx2)$x, get_coords(bx2_r)$x)
  
  expect_equal(get_coords(bx1)$x, get_coords(bx3)$x)
  expect_equal(get_coords(bx3)$y, get_coords(bx3_r)$y)
  
  expect_equal(get_coords(bx4), get_coords(bx5))

})


