# Add nodes

Create/add vertically aligned labeled nodes or side nodes.

## Usage

``` r
add_box(
  prev_box = NULL,
  txt,
  just = c("center", "left", "right"),
  text_width = NULL,
  ...
)
```

## Arguments

- prev_box:

  Previous node object, the created new node will be vertically aligned
  with this node. Left this as \`NULL\` if this is the first node. The
  first node will be aligned in the top center.

- txt:

  Text in the node. If the \`prev_box\` is a horizontally aligned
  multiple nodes, a vector of with the same length must be provided.

- just:

  The justification for the text: left, center or right.

- text_width:

  a positive integer giving the target column for wrapping lines in the
  output. String will not be wrapped if not defined (default). The
  [`stri_wrap`](https://rdrr.io/pkg/stringi/man/stri_wrap.html) function
  will be used if `stringi` package installed as it is a better options
  for non-Latin language, otherwise
  [`strwrap`](https://rdrr.io/r/base/strwrap.html) will be used.

- ...:

  Other parameters pass to
  [textbox](https://adayim.github.io/consort/reference/textbox.md),

## Value

A `consort` object.

## See also

[`add_side_box`](https://adayim.github.io/consort/reference/add_side_box.md)
[`add_split`](https://adayim.github.io/consort/reference/add_split.md)
[`textbox`](https://adayim.github.io/consort/reference/textbox.md)
[`add_label_box`](https://adayim.github.io/consort/reference/add_label_box.md)

## Examples

``` r
txt1 <- "Population (n=300)"
txt1_side <- "Excluded (n=15): \n
              \u2022 MRI not collected (n=3)\n
              \u2022 Tissues not collected (n=4)\n
              \u2022 Other (n=8)"

g <- add_box(txt = txt1)

g <- add_side_box(g, txt = txt1_side)

g <- add_box(g, txt = "Randomized (n=200)")

g <- add_split(g, txt = c("Arm A (n=100)", "Arm B (n=100"))
g <- add_side_box(g,
  txt = c(
    "Excluded (n=15):\n
                  \u2022 MRI not collected (n=3)\n
                  \u2022 Tissues not collected (n=4)\n
                   \u2022 Other (n=8)",
    "Excluded (n=15):\n
                   \u2022 MRI not collected (n=3)\n
                   \u2022 Tissues not collected (n=4)"
  )
)

g <- add_box(g, txt = c("Final analysis (n=100)", "Final analysis (n=100"))
g <- add_label_box(g, txt = c("1" = "Screening", "3" = "Randomized", "4" = "Final analysis"))
```
