
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
  old <- set_consort_defaults(
    txt_gp = gpar(cex = 0.5),
    box_gp = gpar(fill = "red")
  )
  on.exit(set_consort_defaults(
    txt_gp = old$txt_gp,
    box_gp = old$box_gp
  ), add = TRUE)

  bx1 <- textbox(text = "This is a test")

  # Text size
  expect_equal(bx1$txt_gp$cex, 0.5)
  # Box fill
  expect_equal(bx1$box_gp$fill, "red")
})


test_that("Measurement caching is stable", {
  bx1 <- textbox(text = "This is a test")

  # Repeated measurements return identical values (served from cache)
  first <- get_coords(bx1)
  second <- get_coords(bx1)
  expect_identical(first$width, second$width)
  expect_identical(first$height, second$height)

  # Cached measurement matches a freshly built identical textbox
  bx2 <- textbox(text = "This is a test")
  expect_equal(first$width, get_coords(bx2)$width)
  expect_equal(first$height, get_coords(bx2)$height)

  # Cache environment is populated after measuring
  expect_true(!is.null(bx1$hw_cache$hw))
})

test_that("Expect class type", {
  tx <- textGrob(label = "text")
  bx1 <- textbox(text = "This is a test")

  # expect_error(get_coords(tx), "Object x must be textbox.")
  expect_s3_class(bx1, "textbox")
})
