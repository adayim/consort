# Move a box grob

This function can be used to move the box to a given position with
[editGrob](https://rdrr.io/r/grid/grid.edit.html) changing the `x` and
`y` value.

## Usage

``` r
move_box(obj, x = NULL, y = NULL, pos_type = c("absolute", "relative"))
```

## Arguments

- obj:

  A `box` object.

- x:

  A unit element or a number that can be converted to `npc`, see
  [unit](https://rdrr.io/r/grid/unit.html).

- y:

  A unit element or a number that can be converted to `npc`, see
  [unit](https://rdrr.io/r/grid/unit.html).

- pos_type:

  If the provided coordinates are `absolute` position the box will be
  moved to or it's a `relative` position to it's current.

## Value

A box object with updated x and y coordinates.

## Examples

``` r
fg <- textbox(text = "This is a test")
fg2 <- move_box(fg, 0.3, 0.3)
```
