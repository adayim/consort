# Get the coordinates of the textbox object

This function will get the coordinates of the textbox object.

## Usage

``` r
get_coords(x)
```

## Arguments

- x:

  A textbox object

## Value

A list of coordinates will return:

- left:

  Left (x-min) side coordinate.

- right:

  Right (x-max) side coordinate.

- bottom:

  Bottom (y-min) side coordinate.

- top:

  Top (y-max) side coordinate.

- top.mid :

  Coordinate vector of top middle, measured by grob.

- left.mid:

  Coordinate vector of left middle, measured by grob.

- bottom.mid:

  Coordinate vector of bottom middle, measured by grob.

- right.mid:

  Coordinate vector of right middle, measured by grob.

- x:

  X (center x) coordinate.

- y:

  Y (center y) coordinate.

- width:

  Width of the textbox, derived with `grobWidth`.

- height:

  Height of the textbox, derived with `grobHeight`.

- half_width:

  Half width of the box.

- half_height:

  Half height of the box.

## Examples

``` r
fg <- textbox(text = "This is a test")
get_coords(fg)
#> $left
#> [1] sum(0.5npc, -3.6640625char)
#> 
#> $right
#> [1] sum(0.5npc, 3.6640625char)
#> 
#> $bottom
#> [1] sum(0.5npc, -0.875char)
#> 
#> $top
#> [1] sum(0.5npc, 0.875char)
#> 
#> $top.mid
#> [1] 90grobx 90groby
#> 
#> $left.mid
#> [1] 180grobx 180groby
#> 
#> $bottom.mid
#> [1] 90grobx  270groby
#> 
#> $right.mid
#> [1] 0grobx 0groby
#> 
#> $x
#> [1] 0.5npc
#> 
#> $y
#> [1] 0.5npc
#> 
#> $width
#> [1] 7.328125
#> 
#> $height
#> [1] 1.75
#> 
#> $half_width
#> [1] 3.6640625char
#> 
#> $half_height
#> [1] 0.875char
#> 
```
