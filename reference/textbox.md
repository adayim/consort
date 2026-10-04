# Create a box with text

Create a [grob](https://rdrr.io/r/grid/grid.grob.html) with text inside.
To extract the units describing grob boundary location can be accessed
with [grobX](https://rdrr.io/r/grid/grobX.html) and
[grobY](https://rdrr.io/r/grid/grobX.html). The units describing width
and height can be accessed with
[grobWidth](https://rdrr.io/r/grid/grobWidth.html) and
[grobHeight](https://rdrr.io/r/grid/grobWidth.html).

## Usage

``` r
textbox(
  text,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  just = c("center", "left", "right"),
  txt_gp = consort_opt("txt_gp"),
  leftrotate = FALSE,
  box_fn = roundrectGrob,
  box_gp = consort_opt("box_gp"),
  name = "textbox"
)

grid.textbox(...)
```

## Arguments

- text:

  A character text to be passed to
  [textGrob](https://rdrr.io/r/grid/grid.text.html).

- x:

  A number or unit object specifying x-location.

- y:

  A number or unit object specifying y-location.

- just:

  The justification of the text, \`"left"\`, \`"right\` and
  \`"center"\`. See [textGrob](https://rdrr.io/r/grid/grid.text.html)
  for more details.

- txt_gp:

  An object of class [gpar](https://rdrr.io/r/grid/gpar.html) style to
  be applied to the text. Defaults are read from
  [`set_consort_defaults`](https://adayim.github.io/consort/reference/set_consort_defaults.md).
  For example, to set a font size for all text inside boxes, use
  `set_consort_defaults(txt_gp = gpar(cex = 0.8))`.

- leftrotate:

  If the text box will be rotated 90 degrees counter-clockwise. Default
  is `FALSE`.

- box_fn:

  Function to create box for the text. Parameters of \`x=0.5\`,
  \`y=0.5\` and \`box_gp\` will be passed to this function and return a
  `grob` object.

- box_gp:

  An object of class [gpar](https://rdrr.io/r/grid/gpar.html) style to
  be applied to the box.

- name:

  A character identifier.

- ...:

  Parameters passed to `textbox`

## Value

A text box grob. grid.textbox() returns the value invisibly.

## Examples

``` r
fg <- textbox(text = "This is a test")
grid::grid.draw(fg)
grid.textbox(text = "This is a test")
grid.textbox(text = "This is a test")
```
