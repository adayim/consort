# Add a vertically aligned label nodes on the left side.

In a consort diagram, this can be used to indicate different stage.

## Usage

``` r
add_label_box(
  prev_box,
  txt,
  only_terminal = TRUE,
  just = c("center", "left", "right"),
  box_fn = roundrectGrob,
  ...
)
```

## Arguments

- prev_box:

  A completed diagram created with `add_box`, `add_side_box` etc.

- txt:

  Text in the node. If a character string is provided, the label will be
  aligned to the last box if a character is provided. If a named vector,
  the labels will align to corresponding row of the node. And the names
  is the number indicating row number of box to horizontally align with
  and value is the text in the box.

- only_terminal:

  If the txt is only for the terminal box, default. Otherwise, the side
  box will also be accounted for.

- just:

  The justification for the text: center (default), left or right.

- box_fn:

  The function to create the box grob. Default is
  [`roundrectGrob`](https://rdrr.io/r/grid/grid.roundrect.html).

- ...:

  Other parameters pass to
  [textbox](https://adayim.github.io/consort/reference/textbox.md).

## Value

A `consort` object.

## See also

[`add_side_box`](https://adayim.github.io/consort/reference/add_side_box.md)
[`add_split`](https://adayim.github.io/consort/reference/add_split.md)
[`textbox`](https://adayim.github.io/consort/reference/textbox.md)
[`add_box`](https://adayim.github.io/consort/reference/add_box.md)

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
