
test_that("Box width and height", {
  bx1 <- textbox(text = "This is a test")
  bx2 <- textbox(text = "This is a test", x = 0.2, y = 0.4)
  bx3 <- textbox(text = "BBB", x = 0.2, y = 0.4)
  bx4 <- textbox(text = "This is a test\n BBB")
  bx5 <- textbox(text = "Ths\nBBB")
  
  # Width
  expect_equal(get_coords(bx1)$width, get_coords(bx2)$width)
  # expect_equal(get_coords(bx3)$width, get_coords(bx5)$width)
  
  # Height
  expect_equal(get_coords(bx1)$height, get_coords(bx2)$height)
  expect_equal(get_coords(bx1)$height, get_coords(bx3)$height)
  expect_equal(get_coords(bx4)$height, get_coords(bx5)$height)

})

test_that("Box options", {
  options(txt_gp = gpar(cex = 0.5),
          box_gp = gpar(fill = "red"))
  bx1 <- textbox(text = "This is a test")
  
  # Text size
  expect_equal(getGrob(bx1, "label")$gp$cex, 0.5)
  # Box fill
  expect_equal(getGrob(bx1, "box")$gp$fill, "red")
  
  options(txt_gp = gpar(),
          box_gp = gpar())
  
})


test_that("Expect class type", {
  tx <- textGrob(label = "text")
  bx1 <- textbox(text = "This is a test")
  
  expect_error(get_coords(tx), "Object x must be box.")
  expect_s3_class(bx1, "box")
  
})

