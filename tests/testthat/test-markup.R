
# --- Parser tests -----------------------------------------------------------

test_that("has_markup returns FALSE when parse_markup is disabled", {
  init_consort_defaults()
  expect_false(has_markup("**bold**"))
  expect_false(has_markup("*italic*"))
})

test_that("has_markup detects markup patterns when enabled", {
  old <- set_consort_defaults(parse_markup = TRUE)
  on.exit(set_consort_defaults(parse_markup = old$parse_markup), add = TRUE)

  expect_true(has_markup("**bold**"))
  expect_true(has_markup("*italic*"))
  expect_true(has_markup("^{super}"))
  expect_true(has_markup("_{sub}"))
  expect_true(has_markup("__underline__"))
  expect_true(has_markup("text **with** markup"))

  expect_false(has_markup("plain text"))
  expect_false(has_markup(""))
  expect_false(has_markup(NULL))
  expect_false(has_markup(NA_character_))
})

test_that("parse_markup handles plain text", {
  segs <- parse_markup("hello world")
  expect_length(segs, 1)
  expect_equal(segs[[1]]$text, "hello world")
  expect_equal(segs[[1]]$style, "plain")
})

test_that("parse_markup handles bold", {
  segs <- parse_markup("**bold**")
  expect_length(segs, 1)
  expect_equal(segs[[1]]$text, "bold")
  expect_equal(segs[[1]]$style, "bold")
})

test_that("parse_markup handles italic", {
  segs <- parse_markup("*italic*")
  expect_length(segs, 1)
  expect_equal(segs[[1]]$text, "italic")
  expect_equal(segs[[1]]$style, "italic")
})

test_that("parse_markup handles superscript and subscript", {
  segs <- parse_markup("x^{2}")
  expect_length(segs, 2)
  expect_equal(segs[[1]]$text, "x")
  expect_equal(segs[[2]]$text, "2")
  expect_equal(segs[[2]]$style, "superscript")

  segs2 <- parse_markup("H_{2}O")
  expect_length(segs2, 3)
  expect_equal(segs2[[2]]$text, "2")
  expect_equal(segs2[[2]]$style, "subscript")
})

test_that("parse_markup handles underline", {
  segs <- parse_markup("__underline__")
  expect_length(segs, 1)
  expect_equal(segs[[1]]$text, "underline")
  expect_equal(segs[[1]]$style, "underline")
})

test_that("parse_markup handles mixed markup", {
  segs <- parse_markup("**Enrolled** (n=300)\n*p* < 0.05^{1}")
  styles <- sapply(segs, "[[", "style")
  texts  <- sapply(segs, "[[", "text")

  expect_equal(styles, c("bold", "plain", "italic", "plain", "superscript"))
  expect_equal(texts[[1]], "Enrolled")
  expect_equal(texts[[3]], "p")
  expect_equal(texts[[5]], "1")
})

test_that("parse_markup returns plain for empty/NULL", {
  segs <- parse_markup("")
  expect_equal(segs[[1]]$text, "")
  expect_equal(segs[[1]]$style, "plain")

  segs2 <- parse_markup(NULL)
  expect_equal(segs2[[1]]$text, "")
})

# --- Newline splitting -------------------------------------------------------

test_that("split_segments_by_newline splits plain text at newlines", {
  segs <- list(list(text = "line1\nline2", style = "plain"))
  lines <- split_segments_by_newline(segs)
  expect_length(lines, 2)
  expect_equal(lines[[1]][[1]]$text, "line1")
  expect_equal(lines[[2]][[1]]$text, "line2")
})

test_that("split_segments_by_newline keeps non-plain segments intact", {
  segs <- list(
    list(text = "before\n", style = "plain"),
    list(text = "bold", style = "bold"),
    list(text = "\nafter", style = "plain")
  )
  lines <- split_segments_by_newline(segs)
  expect_length(lines, 3)
  expect_equal(lines[[2]][[1]]$text, "bold")
  expect_equal(lines[[2]][[1]]$style, "bold")
})

# --- HTML conversion ---------------------------------------------------------

test_that("markup_to_html converts tags correctly", {
  expect_equal(markup_to_html("**bold**"), "<b>bold</b>")
  expect_equal(markup_to_html("*italic*"), "<i>italic</i>")
  expect_equal(markup_to_html("^{sup}"), "<sup>sup</sup>")
  expect_equal(markup_to_html("_{sub}"), "<sub>sub</sub>")
  expect_equal(markup_to_html("__underline__"), "<u>underline</u>")
})

test_that("markup_to_html escapes HTML special characters", {
  expect_equal(markup_to_html("a < b & c"), "a &lt; b &amp; c")
  expect_equal(markup_to_html("**a < b**"), "<b>a &lt; b</b>")
})

test_that("markup_to_html converts newlines to <br/>", {
  expect_equal(markup_to_html("a\nb"), "a<br/>b")
})

test_that("markup_to_html duplicates space inside bold/italic/underline tags", {
  # Space is duplicated: one inside the tag + one outside
  expect_equal(markup_to_html("**bold** text"), "<b>bold </b> text")
  expect_equal(markup_to_html("*italic* text"), "<i>italic </i> text")
  expect_equal(markup_to_html("__underline__ text"), "<u>underline </u> text")
  # No space after tag — nothing to duplicate
  expect_equal(markup_to_html("**bold**\ntext"), "<b>bold</b><br/>text")
})

# --- segment_gpar ------------------------------------------------------------

test_that("segment_gpar sets fontface for bold/italic", {
  base <- gpar(fontsize = 12)
  expect_equal(segment_gpar("bold", base)$fontface, "bold")
  expect_equal(segment_gpar("italic", base)$fontface, "italic")
})

test_that("segment_gpar scales cex for super/subscript", {
  base <- gpar(cex = 1)
  expect_equal(segment_gpar("superscript", base)$cex, 0.7)
  expect_equal(segment_gpar("subscript", base)$cex, 0.7)
})

test_that("segment_gpar leaves plain text unchanged", {
  base <- gpar(fontsize = 12)
  gp <- segment_gpar("plain", base)
  expect_null(gp$fontface)
  expect_equal(gp$fontsize, 12)
})

# --- Grid rendering (smoke tests) -------------------------------------------

test_that("textbox with markup creates valid grob when enabled", {
  old <- set_consort_defaults(parse_markup = TRUE)
  on.exit(set_consort_defaults(parse_markup = old$parse_markup), add = TRUE)

  bx <- textbox(text = "**Enrolled** (n=300)")
  expect_s3_class(bx, "textbox")

  bx2 <- textbox(text = "H_{2}O is *water*")
  expect_s3_class(bx2, "textbox")
})

test_that("textbox with markup renders without error", {
  old <- set_consort_defaults(parse_markup = TRUE)
  on.exit(set_consort_defaults(parse_markup = old$parse_markup), add = TRUE)

  skip_on_ci()
  png(tf <- tempfile(fileext = ".png"), width = 400, height = 200)
  on.exit({ dev.off(); unlink(tf) }, add = TRUE)

  grid::grid.newpage()
  expect_no_error(grid::grid.draw(textbox(text = "**Bold** and *italic*")))
  expect_no_error(grid::grid.draw(
    textbox(text = "x^{2} + H_{2}O\n__underlined__", y = 0.3)
  ))
})

test_that("textbox ignores markup when parse_markup is FALSE", {
  init_consort_defaults()  # parse_markup = FALSE
  bx <- textbox(text = "**bold** text")
  # label is stored as-is (not parsed)
  expect_equal(bx$label, "**bold** text")
})

# --- grViz integration -------------------------------------------------------

test_that("mk_text_align uses HTML label for markup text when enabled", {
  old <- set_consort_defaults(parse_markup = TRUE)
  on.exit(set_consort_defaults(parse_markup = old$parse_markup), add = TRUE)

  result <- mk_text_align("**bold** text", just = "center")
  expect_true(grepl("label = <", result, fixed = TRUE))
  # Space duplicated: one inside + one outside the bold tag
  expect_true(grepl("<b>bold </b> ", result, fixed = TRUE))
})

test_that("mk_text_align uses plain label without markup", {
  init_consort_defaults()
  result <- mk_text_align("plain text", just = "center")
  expect_true(grepl('label = "', result, fixed = TRUE))
})

test_that("mk_text_align uses plain label when parse_markup is FALSE", {
  init_consort_defaults()  # parse_markup = FALSE
  result <- mk_text_align("**bold** text", just = "center")
  expect_true(grepl('label = "', result, fixed = TRUE))
  expect_false(grepl("<b>", result, fixed = TRUE))
})

test_that("mk_text_align HTML label handles alignment", {
  old <- set_consort_defaults(parse_markup = TRUE)
  on.exit(set_consort_defaults(parse_markup = old$parse_markup), add = TRUE)

  result_left <- mk_text_align("**bold**\nmore", just = "left")
  expect_true(grepl('align="left"', result_left, fixed = TRUE))

  result_right <- mk_text_align("**bold**\nmore", just = "right")
  expect_true(grepl('align="right"', result_right, fixed = TRUE))
})

# --- Full pipeline (build_grviz) --------------------------------------------

test_that("build_grviz handles markup in nodes when enabled", {
  old <- set_consort_defaults(parse_markup = TRUE)
  on.exit(set_consort_defaults(parse_markup = old$parse_markup), add = TRUE)

  g <- add_box(txt = "**Enrolled** (n=300)")
  g <- add_side_box(g, txt = "Excluded^{1} (n=15)")
  g <- add_box(g, txt = "*Randomised*")

  txt <- build_grviz(g)
  # Space duplicated: one inside + one outside the bold tag
  expect_true(grepl("<b>Enrolled </b>", txt, fixed = TRUE))
  expect_true(grepl("<sup>1</sup>", txt, fixed = TRUE))
  # No space after *Randomised* (end of text), so tag is unchanged
  expect_true(grepl("<i>Randomised</i>", txt, fixed = TRUE))
})

# --- set_consort_defaults validation -----------------------------------------

test_that("set_consort_defaults validates parse_markup", {
  expect_error(set_consort_defaults(parse_markup = "yes"),
               "parse_markup")
  expect_error(set_consort_defaults(parse_markup = NA),
               "parse_markup")
  expect_error(set_consort_defaults(parse_markup = c(TRUE, FALSE)),
               "parse_markup")
})

init_consort_defaults()
