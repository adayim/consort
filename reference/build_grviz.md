# Build consort diagram as Graphviz DOT code

Build the Graphviz DOT representation of a consort object, use this if
you want to render with
[`grViz`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)
(see `plot(x, grViz = TRUE)`) or export via `export_svg`. Splits can be
nested to any depth.

## Usage

``` r
build_grviz(x)
```

## Arguments

- x:

  A consort object.

## Value

A `Graphviz` DOT code string.

## See also

[`grViz`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)

## Examples

``` r
if (FALSE) { # \dontrun{
txt1 <- "Population (n=300)"
txt1_side <- "Excluded (n=15): \n
              \u2022 MRI not collected (n=3)\n
              \u2022 Tissues not collected (n=4)\n
              \u2022 Other (n=8)"

g <- add_box(txt = txt1)

g <- add_side_box(g, txt = txt1_side)

g <- add_box(g, txt = "Randomized (n=200)")
# plot(g, grViz = TRUE)
} # }
```
