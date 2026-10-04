# Structural checks on the X layout (calc_coords), independent of snapshots.

# Per-row boxes that are actually drawn: main nodes (placeholders included,
# they still occupy a slot) and side boxes with text.
layout_boxes <- function(g) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  cc <- calc_coords(g)

  lapply(seq_along(cc$nd_x), function(r) {
    nm <- names(cc$nd_x[[r]])
    drawn <- vapply(nm, function(n) {
      g[[n]]$node_type != "sidebox" || !is_empty(g[[n]]$text)
    }, logical(1))
    data.frame(node = nm, x = unname(cc$nd_x[[r]]),
               w = unname(cc$nodes_hw[[r]]), drawn = drawn,
               stringsAsFactors = FALSE)
  })
}

expect_no_overlap <- function(g) {
  sib_gap <- 2 * consort_opt("pad_u")
  for (row in layout_boxes(g)) {
    row <- row[row$drawn, ]
    if (nrow(row) < 2) next
    row <- row[order(row$x), ]
    gaps <- (row$x - row$w / 2)[-1] - (row$x + row$w / 2)[-nrow(row)]
    expect_true(all(gaps >= sib_gap - 1e-6),
                info = paste("row with", paste(row$node, collapse = ", ")))
  }
}

all_x <- function(g) {
  b <- do.call(rbind, layout_boxes(g))
  setNames(b$x, b$node)
}

# A node is centred over its first and last child; a lone child is directly
# below its parent.
expect_centred <- function(g) {
  x <- all_x(g)
  main <- names(g)[vapply(g, function(n) n$node_type != "sidebox", logical(1))]
  for (nm in main) {
    kids <- main[vapply(main, function(k) identical(g[[k]]$prev_node, nm), logical(1))]
    if (length(kids) == 0) next
    expect_equal(unname(x[nm]), mean(x[kids[c(1, length(kids))]]),
                 info = nm, tolerance = 1e-6)
  }
}

# A merge node group sits under the mean of its parents.
expect_merges_centred <- function(g) {
  x <- all_x(g)
  merged <- names(g)[vapply(g, function(n) length(n$prev_node) > 1, logical(1))]
  keys <- vapply(merged, function(n) paste(g[[n]]$prev_node, collapse = "|"), "")
  for (key in unique(keys)) {
    grp <- merged[keys == key]
    expect_equal(mean(x[grp[c(1, length(grp))]]),
                 mean(x[g[[grp[1]]]$prev_node]), tolerance = 1e-6)
  }
}

expect_sides <- function(g) {
  x <- all_x(g)
  w <- setNames(unlist(lapply(layout_boxes(g), `[[`, "w")),
                unlist(lapply(layout_boxes(g), `[[`, "node")))
  for (nm in names(g)[vapply(g, function(n) n$node_type == "sidebox", logical(1))]) {
    a <- g[[nm]]$prev_node
    if (g[[nm]]$side == "right") {
      expect_gt(x[[nm]] - w[[nm]] / 2, x[[a]])
    } else {
      expect_lt(x[[nm]] + w[[nm]] / 2, x[[a]])
    }
  }
}

check_layout <- function(g, centred = TRUE) {
  expect_no_overlap(g)
  expect_sides(g)
  expect_merges_centred(g)
  if (centred) expect_centred(g)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(build_grid(g))
}

side_txt <- "Excluded (n=3):\n• Reason one\n• Reason two"

test_that("single column with a side box", {
  g <- add_box(txt = "Population (n=300)")
  g <- add_side_box(g, txt = side_txt)
  g <- add_box(g, txt = "Randomized (n=200)")
  check_layout(g)
})

test_that("one split with left and right side boxes", {
  g <- add_box(txt = "Population (n=300)")
  g <- add_split(g, txt = c("Arm A (n=100)", "Arm B (n=100)"))
  g <- add_side_box(g, txt = c(side_txt, side_txt))
  g <- add_box(g, txt = c("Final A", "Final B"))
  check_layout(g)
})

test_that("uneven arm widths do not collide", {
  g <- add_box(txt = "Population (n=300)")
  g <- add_split(g, txt = c("A", "Arm B with a long descriptive name (n=100)", "C"))
  g <- add_side_box(g, txt = c("Withdrew (n=1)", "Out (n=2)", "Out (n=3)"))
  g <- add_box(g, txt = c("A", "Included in the final modified analysis set (n=98)", "C"))
  check_layout(g)
})

test_that("two-level split from disposition data", {
  p <- consort_plot(data = dispos.data,
                    orders = c(trialno = "Population", exclusion = "Excluded",
                               arm = "Randomized patient", arm3 = "",
                               subjid_notdosed = "Lost of Follow-up",
                               followup = "Followup-up",
                               lost_followup = "Lost to follow-up",
                               mitt = "Final Analysis"),
                    side_box = c("exclusion", "subjid_notdosed", "lost_followup"),
                    allocation = c("arm", "arm3"))
  check_layout(p)
})

nested_split <- function(levels = 3) {
  g <- add_box(txt = "Population (n=300)")
  g <- add_split(g, txt = c("Region A", "Region B"))
  g <- add_split(g, txt = list(c("Site A1", "Site A2"),
                               c("Site B1", "Site B2", "Site B3")))
  if (levels >= 3) {
    g <- add_split(g, txt = list(c("Arm A1x", "Arm A1y"), "Arm A2x",
                                 c("Arm B1x", "Arm B1y"),
                                 c("Arm B2x with a rather long label", "Arm B2y"),
                                 "Arm B3x"))
  }
  if (levels >= 4) {
    g <- add_split(g, txt = list(c("a", "b"), "c", "d", c("e", "f"), c("g", "h"),
                                 "i", c("j", "kk with long text"), "l"))
  }
  n <- length(attr(g, "nodes.list")[[length(attr(g, "nodes.list"))]])
  add_box(g, txt = paste("Analysed", seq_len(n)))
}

test_that("three-level nested split", {
  check_layout(nested_split(3))
})

test_that("four-level nested split", {
  check_layout(nested_split(4))
})

test_that("three-level nested split with side boxes at depth", {
  g <- add_box(txt = "Population (n=300)")
  g <- add_split(g, txt = c("Region A", "Region B"))
  g <- add_split(g, txt = list(c("Site A1", "Site A2"), c("Site B1", "Site B2")))
  g <- add_split(g, txt = list(c("Arm 1", "Arm 2"), c("Arm 1", "Arm 2"),
                               c("Arm 1", "Arm 2"), c("Arm 1", "Arm 2")))
  g <- add_side_box(g, txt = rep(side_txt, 8))
  g <- add_box(g, txt = rep("Analysed", 8))
  check_layout(g)
})

test_that("multiple roots merge into one node", {
  g <- add_box(txt = c("Study 1 (n=8)", "Study 2 (n=12)"))
  g <- add_box(g, txt = "Included All (n=20)")
  g <- add_side_box(g, txt = "Excluded (n=7):\n• MRI not collected (n=3)")
  g <- add_box(g, txt = "Randomised")
  g <- add_split(g, txt = c("Arm A (n=143)", "Arm B (n=142)"))
  check_layout(g)
})

test_that("empty placeholders keep their column", {
  g <- add_box(txt = c("Study 1 (n=8)", "Study 2 (n=12)"))
  g <- add_box(g, txt = "Included All (n=20)")
  g <- add_side_box(g, txt = "Excluded (n=7):\n• MRI not collected (n=3)")
  g <- add_box(g, txt = "Randomised")
  g <- add_split(g, txt = c("Arm A (n=143)", "Arm B (n=142)"))
  g <- add_side_box(g, txt = c("", "Exclude (n=3)"))
  g <- add_box(g, txt = c("", "From Arm B"))
  g <- add_box(g, txt = c("This is it", "From Arm B"))
  check_layout(g, centred = FALSE)
})

test_that("empty box in the middle of a merge", {
  g <- add_box(txt = c("Cohort 1 (n=6)", "Cohort 2 (n=6)", "Cohort 3 (n=6)"))
  g <- add_side_box(g, txt = c("Excluded (n=1)", "Excluded (n=3)", ""))
  g <- add_box(g, txt = c("Cohort 1 (n=5)", "Cohort 2 (n=3)", ""))
  g <- add_box(g, txt = "Total (n=14)")
  check_layout(g, centred = FALSE)
})

test_that("a non-nested split over parallel nodes", {
  g <- add_box(txt = c("Cohort 1", "Cohort 2"))
  g <- add_split(g, txt = c("Treatment A", "Treatment B", "Treatment C"))
  g <- add_box(g, txt = c("Final A", "Final B", "Final C"))
  check_layout(g)
})

test_that("a split after a merge and an arm that rejoins", {
  g <- add_box(txt = "Population")
  g <- add_split(g, txt = c("Arm A", "Arm B"))
  g <- add_box(g, txt = "Pooled analysis")
  g <- add_split(g, txt = c("Sub 1", "Sub 2", "Sub 3"))
  check_layout(g)
})

test_that("nested splits render", {
  skip_on_ci()
  expect_snapshot_file(save_png(nested_split(3), width = 14, height = 9),
                       "nested-split-3.png")
  expect_snapshot_file(save_png(nested_split(4), width = 16, height = 9),
                       "nested-split-4.png")
})
