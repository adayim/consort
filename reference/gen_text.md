# Generate label and bullet points

This function use the data to generate label and bullet points for the
box.

## Usage

``` r
gen_text(
  x,
  label = NULL,
  bullet = FALSE,
  bullet_char = consort_opt("bullet"),
  drop_levels = TRUE
)
```

## Arguments

- x:

  A list or a vector to be used. `x` can be atomic vector, a
  `data.frame` or a `list`. A `data.frame` is particular useful if the
  there's a nested reason or a `list` split nested reasons by group. The
  nested reasons only support two columns and the `bullet` will be
  ignored.

- label:

  A character string as a label at the beginning of the text label. The
  count for each categories will be returned if no label is provided.

- bullet:

  If shows bullet points. If the value is `TRUE`, the bullet points will
  be tabulated, default is `FALSE`.

- bullet_char:

  A single character used as the bullet symbol. Defaults to
  `consort_opt("bullet")`. Can be any Unicode character such as
  `"\u2013"` (en-dash), `"\u25CB"` (circle), `"\u25A0"` (square), `"-"`,
  etc.

- drop_levels:

  If `TRUE` (default), unused factor levels are dropped before
  tabulation, so categories with zero counts are omitted. Set to `FALSE`
  to report zero-count factor levels as `(n=0)`, e.g. to display an
  exclusion criterion that was applied but excluded nobody. Only applies
  when `x` (or the reason column) is a factor.

## Value

A character string of vector.

## Examples

``` r
val <- data.frame(
  am = factor(ifelse(mtcars$am == 1, "Automatic", "Manual"), ordered = TRUE),
  vs = factor(ifelse(mtcars$vs == 1, "Straight", "V-shaped"), ordered = TRUE),
  car = row.names(mtcars)
)

gen_text(val$car, label = "Cars in the data")
#> [1] "Cars in the data (n=32)"
gen_text(val$car, label = "Cars in the data", bullet = FALSE)
#> [1] "Cars in the data (n=32)"
gen_text(split(val$car, val$am), label = "Cars in the data")
#> [1] "Cars in the data (n=13)" "Cars in the data (n=19)"
gen_text(split(val$car, val$am), label = "Cars in the data", bullet = FALSE)
#> [1] "Cars in the data (n=13)" "Cars in the data (n=19)"
gen_text(split(val[,c("vs", "car")], val$am), label = "Cars in the data", bullet = FALSE)
#> [1] "Cars in the data (n=13)\nV-shaped (n=6)\n• Ferrari Dino (n=1)\n• Ford Pantera L (n=1)\n• Maserati Bora (n=1)\n• Mazda RX4 (n=1)\n• Mazda RX4 Wag (n=1)\n• Porsche 914-2 (n=1)\nStraight (n=7)\n• Datsun 710 (n=1)\n• Fiat 128 (n=1)\n• Fiat X1-9 (n=1)\n• Honda Civic (n=1)\n• Lotus Europa (n=1)\n• Toyota Corolla (n=1)\n• Volvo 142E (n=1)"                                                                                                                                                  
#> [2] "Cars in the data (n=19)\nStraight (n=7)\n• Hornet 4 Drive (n=1)\n• Merc 230 (n=1)\n• Merc 240D (n=1)\n• Merc 280 (n=1)\n• Merc 280C (n=1)\n• Toyota Corona (n=1)\n• Valiant (n=1)\nV-shaped (n=12)\n• AMC Javelin (n=1)\n• Cadillac Fleetwood (n=1)\n• Camaro Z28 (n=1)\n• Chrysler Imperial (n=1)\n• Dodge Challenger (n=1)\n• Duster 360 (n=1)\n• Hornet Sportabout (n=1)\n• Lincoln Continental (n=1)\n• Merc 450SE (n=1)\n• Merc 450SL (n=1)\n• Merc 450SLC (n=1)\n• Pontiac Firebird (n=1)"
gen_text(val[,c("vs", "car")], label = "Cars in the data", bullet = FALSE)
#> [1] "Cars in the data (n=32)\nV-shaped (n=18)\n• AMC Javelin (n=1)\n• Cadillac Fleetwood (n=1)\n• Camaro Z28 (n=1)\n• Chrysler Imperial (n=1)\n• Dodge Challenger (n=1)\n• Duster 360 (n=1)\n• Ferrari Dino (n=1)\n• Ford Pantera L (n=1)\n• Hornet Sportabout (n=1)\n• Lincoln Continental (n=1)\n• Maserati Bora (n=1)\n• Mazda RX4 (n=1)\n• Mazda RX4 Wag (n=1)\n• Merc 450SE (n=1)\n• Merc 450SL (n=1)\n• Merc 450SLC (n=1)\n• Pontiac Firebird (n=1)\n• Porsche 914-2 (n=1)\nStraight (n=14)\n• Datsun 710 (n=1)\n• Fiat 128 (n=1)\n• Fiat X1-9 (n=1)\n• Honda Civic (n=1)\n• Hornet 4 Drive (n=1)\n• Lotus Europa (n=1)\n• Merc 230 (n=1)\n• Merc 240D (n=1)\n• Merc 280 (n=1)\n• Merc 280C (n=1)\n• Toyota Corolla (n=1)\n• Toyota Corona (n=1)\n• Valiant (n=1)\n• Volvo 142E (n=1)"

# Use a custom bullet character
gen_text(val$car, label = "Cars in the data", bullet = TRUE, bullet_char = "-")
#> [1] "Cars in the data (n=32)\n- AMC Javelin (n=1)\n- Cadillac Fleetwood (n=1)\n- Camaro Z28 (n=1)\n- Chrysler Imperial (n=1)\n- Datsun 710 (n=1)\n- Dodge Challenger (n=1)\n- Duster 360 (n=1)\n- Ferrari Dino (n=1)\n- Fiat 128 (n=1)\n- Fiat X1-9 (n=1)\n- Ford Pantera L (n=1)\n- Honda Civic (n=1)\n- Hornet 4 Drive (n=1)\n- Hornet Sportabout (n=1)\n- Lincoln Continental (n=1)\n- Lotus Europa (n=1)\n- Maserati Bora (n=1)\n- Mazda RX4 (n=1)\n- Mazda RX4 Wag (n=1)\n- Merc 230 (n=1)\n- Merc 240D (n=1)\n- Merc 280 (n=1)\n- Merc 280C (n=1)\n- Merc 450SE (n=1)\n- Merc 450SL (n=1)\n- Merc 450SLC (n=1)\n- Pontiac Firebird (n=1)\n- Porsche 914-2 (n=1)\n- Toyota Corolla (n=1)\n- Toyota Corona (n=1)\n- Valiant (n=1)\n- Volvo 142E (n=1)"

# Or set globally via set_consort_defaults
set_consort_defaults(bullet = "\u25CB")
gen_text(val$car, label = "Cars in the data", bullet = TRUE)
#> [1] "Cars in the data (n=32)\n○ AMC Javelin (n=1)\n○ Cadillac Fleetwood (n=1)\n○ Camaro Z28 (n=1)\n○ Chrysler Imperial (n=1)\n○ Datsun 710 (n=1)\n○ Dodge Challenger (n=1)\n○ Duster 360 (n=1)\n○ Ferrari Dino (n=1)\n○ Fiat 128 (n=1)\n○ Fiat X1-9 (n=1)\n○ Ford Pantera L (n=1)\n○ Honda Civic (n=1)\n○ Hornet 4 Drive (n=1)\n○ Hornet Sportabout (n=1)\n○ Lincoln Continental (n=1)\n○ Lotus Europa (n=1)\n○ Maserati Bora (n=1)\n○ Mazda RX4 (n=1)\n○ Mazda RX4 Wag (n=1)\n○ Merc 230 (n=1)\n○ Merc 240D (n=1)\n○ Merc 280 (n=1)\n○ Merc 280C (n=1)\n○ Merc 450SE (n=1)\n○ Merc 450SL (n=1)\n○ Merc 450SLC (n=1)\n○ Pontiac Firebird (n=1)\n○ Porsche 914-2 (n=1)\n○ Toyota Corolla (n=1)\n○ Toyota Corona (n=1)\n○ Valiant (n=1)\n○ Volvo 142E (n=1)"

# Report zero-count factor levels with (n=0)
reason <- factor(c("Ineligible", NA, "Declined", NA),
                 levels = c("Ineligible", "Declined", "Other"))
gen_text(reason, label = "Excluded", bullet = TRUE)
#> [1] "Excluded (n=2)\n○ Ineligible (n=1)\n○ Declined (n=1)"
gen_text(reason, label = "Excluded", bullet = TRUE, drop_levels = FALSE)
#> [1] "Excluded (n=2)\n○ Ineligible (n=1)\n○ Declined (n=1)\n○ Other (n=0)"
```
