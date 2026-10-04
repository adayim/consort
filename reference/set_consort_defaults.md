# Set consort diagram default options

Modify the default graphical parameters and other settings for consort
diagrams. Any parameter set to `NULL` (the default) will remain
unchanged.

## Usage

``` r
set_consort_defaults(
  arrow_gp = NULL,
  txt_gp = NULL,
  box_gp = NULL,
  label_txt_gp = NULL,
  label_box_gp = NULL,
  arrow_length = NULL,
  arrow_type = NULL,
  pad_u = NULL,
  bullet = NULL,
  parse_markup = NULL
)

get_consort_defaults()

init_consort_defaults()

# S3 method for class 'consort_defaults'
print(x, ...)
```

## Arguments

- arrow_gp:

  A [`gpar`](https://rdrr.io/r/grid/gpar.html) object for the arrow
  line.

- txt_gp:

  A [`gpar`](https://rdrr.io/r/grid/gpar.html) object for the text
  inside boxes.

- box_gp:

  A [`gpar`](https://rdrr.io/r/grid/gpar.html) object for the box border
  and fill.

- label_txt_gp:

  A [`gpar`](https://rdrr.io/r/grid/gpar.html) object for the label
  text.

- label_box_gp:

  A [`gpar`](https://rdrr.io/r/grid/gpar.html) object for the label box.

- arrow_length:

  Numeric, length of the arrowhead in inches.

- arrow_type:

  Character, arrow type: `"closed"` or `"open"`.

- pad_u:

  Numeric, padding between nodes.

- bullet:

  Character, bullet character for side box items.

- parse_markup:

  Logical, whether to parse lightweight markup syntax (`**bold**`,
  `*italic*`, `^{superscript}`, `_{subscript}`, `__underline__`) in node
  labels. Default is `FALSE`.

- x:

  A `consort_defaults` object.

- ...:

  Not used.

## Value

Invisibly returns the previous defaults (a `consort_defaults` object).

A `consort_defaults` object containing all current default settings.

## Examples

``` r
# Change text color and box fill
old <- set_consort_defaults(
  txt_gp = grid::gpar(col = "navy", cex = 0.9),
  box_gp = grid::gpar(fill = "#F0F0F0")
)

# View current defaults
get_consort_defaults()
#> Consort diagram default settings:
#> 
#>   arrow_gp      : gpar(col = "blue", lwd = 2)
#>   txt_gp        : gpar(cex = 0.9, col = "navy")
#>   box_gp        : gpar(fill = "#F0F0F0")
#>   label_txt_gp  : gpar(col = "#4F81BD", cex = 1, font = "bold")
#>   label_box_gp  : gpar(fill = "#A9C7FD")
#>   arrow_length  : 0.1
#>   arrow_type    : "closed"
#>   pad_u         : 3
#>   bullet        : "○"
#>   parse_markup  : FALSE

# Restore previous defaults
set_consort_defaults(
  txt_gp = old$txt_gp,
  box_gp = old$box_gp
)
get_consort_defaults()
#> Consort diagram default settings:
#> 
#>   arrow_gp      : gpar(col = "blue", lwd = 2)
#>   txt_gp        : gpar(cex = 1, col = "black")
#>   box_gp        : gpar(fill = "white")
#>   label_txt_gp  : gpar(col = "#4F81BD", cex = 1, font = "bold")
#>   label_box_gp  : gpar(fill = "#A9C7FD")
#>   arrow_length  : 0.1
#>   arrow_type    : "closed"
#>   pad_u         : 3
#>   bullet        : "○"
#>   parse_markup  : FALSE
```
