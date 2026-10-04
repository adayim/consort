# Self generating consort diagram

Create CONSORT diagram from a participant disposition data.

## Usage

``` r
consort_plot(
  data,
  orders,
  side_box,
  allocation = NULL,
  labels = NULL,
  kickoff_sidebox = TRUE,
  cex = 0.8,
  text_width = NULL,
  drop_levels = TRUE
)
```

## Arguments

- data:

  Data set with disposition information for each participants.

- orders:

  A named vector or a list, names as the variable in the dataset and
  values as labels in the box. The order of the diagram will be based on
  this. A list can be used to report multiple variable in a single node,
  the first variable in a list element will be used to report the total
  and the exact items will be summarised for the remaining variable.
  This is limited to non-side box.

- side_box:

  Variable vector, appeared as side box in the diagram. The next box
  will be the subset of the missing values of these variables.

- allocation:

  Name of the grouping/treatment variable (optional), the diagram will
  split into branches on this variables forward. For a factorial design,
  with several splits, a character vector with one variable per split
  can be provided, the splits are nested in that order. The extra box
  will be skipped if the values in the `orders` blank.

- labels:

  Named vector, names is the location of the terminal node. The position
  location should plus 1 after the allocation variables if the
  allocation is defined.

- kickoff_sidebox:

  remove (default) the side box observations from the following
  counting.

- cex:

  Multiplier applied to font size, default is 0.8. Prefer using
  [`set_consort_defaults`](https://adayim.github.io/consort/reference/set_consort_defaults.md)`(txt_gp = gpar(cex = ...))`
  instead.

- text_width:

  a positive integer giving the target column for wrapping lines in the
  output. String will not be wrapped if not defined (default). The
  [`stri_wrap`](https://rdrr.io/pkg/stringi/man/stri_wrap.html) function
  will be used if `stringi` package installed, otherwise
  [`strwrap`](https://rdrr.io/r/base/strwrap.html) will be used.

- drop_levels:

  If `TRUE` (default), unused factor levels are dropped when tabulating
  counts, so categories with zero counts are omitted from the boxes. Set
  to `FALSE` to report zero-count factor levels as `(n=0)`, e.g. to
  display an exclusion criterion that was applied but excluded nobody.
  Only applies to factor variables, see
  [`gen_text`](https://adayim.github.io/consort/reference/gen_text.md).

## Value

A `consort` object.

## Details

The calculation of numbers is as in an analogous to Kirchhoff's Laws of
electricity. The numbers in terminal nodes must sum to those in the
ancestor nodes. All the drop outs will be populated as a side box. Which
was different from the official CONSORT diagram template, which has
dropout inside a vertical node.

## See also

[`add_side_box`](https://adayim.github.io/consort/reference/add_side_box.md),[`add_split`](https://adayim.github.io/consort/reference/add_split.md),
[`add_side_box`](https://adayim.github.io/consort/reference/add_side_box.md)
[`textbox`](https://adayim.github.io/consort/reference/textbox.md)
[`set_consort_defaults`](https://adayim.github.io/consort/reference/set_consort_defaults.md)

## Examples

``` r

## Prepare test data
data(dispos.data)

df <- dispos.data[!dispos.data$arm3 %in% "Trt C", ]
p <- consort_plot(data = df,
                  orders = list(c(trialno = "Population"),
                                c(exclusion = "Excluded"),
                                c(arm     = "Randomized patient"),
                                c(arm3     = "", 
                                  subjid_notdosed="Participants not treated"),
                                c(followup    = "Pariticpants planned for follow-up",
                                  lost_followup = "Reason for tot followed"),
                                c(assessed = "Assessed for final outcome"),
                                c(no_value = "Reason for not assessed"),
                                c(mitt = "Included in the mITT analysis")),
                  side_box = c("exclusion", "no_value"),
                  allocation = c("arm", "arm3"),
                  labels = c("1" = "Screening", "2" = "Randomization",
                             "5" = "Follow-up", "7" = "Final analysis"),
                  cex = 0.7)
```
