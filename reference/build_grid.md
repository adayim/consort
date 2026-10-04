# Build consort diagram

Build a `grob` consort diagram, use this if you want to save plots with
`ggsave`. Splits can be nested to any depth.

## Usage

``` r
build_grid(x, shrink_to_fit = FALSE)
```

## Arguments

- x:

  A consort object.

- shrink_to_fit:

  If `TRUE`, shrink the whole diagram (text included) so it fits the
  currently open graphics device, never enlarging it beyond its natural
  size. Default is `FALSE`, which always lays out the diagram at its
  natural size – the right choice when the result is passed on to
  `ggsave()` rather than drawn directly.

## Value

A `gList` object, with a `size` attribute giving the diagram's natural
width/height in inches.

## Details

The diagram is laid out at a fixed, device-independent "natural" size,
returned (in inches) as the `size` attribute of the result, which can be
passed straight to `ggsave`'s `width`/ `height` for an undistorted
export, e.g.
`sz <- attr(build_grid(x), "size"); ggsave(..., width = sz$width, height = sz$height)`.
With `shrink_to_fit = TRUE`, the diagram is scaled down (text, boxes and
spacing together, never enlarged) to fit the currently open graphics
device; this is what
[`plot.consort`](https://adayim.github.io/consort/reference/plot.consort.md)
uses so an interactive plot window never ends up with overlapping nodes.

## See also

[`gList`](https://rdrr.io/r/grid/grid.grob.html)

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
gr <- build_grid(g)
sz <- attr(gr, "size")
# ggsave("consort_diagram.pdf", plot = gr, width = sz$width, height = sz$height)
} # }
```
