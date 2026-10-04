# Add methods to print function

Method for plot objects and display the output in on a grid device.

## Usage

``` r
# S3 method for class 'consort'
plot(x, grViz = FALSE, ...)

# S3 method for class 'consort'
print(x, grViz = FALSE, ...)
```

## Arguments

- x:

  A `consort` object.

- grViz:

  If use
  [grViz](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)
  to print the plot. Default is `FALSE` to use
  [grid.draw](https://rdrr.io/r/grid/grid.draw.html)

- ...:

  Not used.

## Value

None.

## See also

[`add_side_box`](https://adayim.github.io/consort/reference/add_side_box.md),[`add_split`](https://adayim.github.io/consort/reference/add_split.md),
[`add_side_box`](https://adayim.github.io/consort/reference/add_side_box.md),
[grid.draw](https://rdrr.io/r/grid/grid.draw.html)
