# Changelog

## consort 1.2.4

- New `drop_levels` argument in
  [`consort_plot()`](https://adayim.github.io/consort/reference/consort_plot.md)
  and
  [`gen_text()`](https://adayim.github.io/consort/reference/gen_text.md)
  to report zero-count factor levels as `(n=0)`
  ([\#25](https://github.com/adayim/consort/issues/25)).
- Faster plotting by caching text box measurements and simplifying
  layout calculations.
- Fixed node-name confusion in
  [`build_grviz()`](https://adayim.github.io/consort/reference/build_grviz.md).
- Fixed an error with multiple variables in the first element of
  `orders`.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) now shrinks
  the diagram to fit the device instead of stretching it, fixing
  overlapping nodes.
- Fixed
  [`add_label_box()`](https://adayim.github.io/consort/reference/add_label_box.md)
  ignoring its own `box_fn` and `just` arguments.
- [`build_grid()`](https://adayim.github.io/consort/reference/build_grid.md),
  [`build_grviz()`](https://adayim.github.io/consort/reference/build_grviz.md)
  and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) now
  support any depth of nested splits.
- [`consort_plot()`](https://adayim.github.io/consort/reference/consort_plot.md)
  accepts more than two `allocation` variables.
- Straighter, better centred nodes in `grViz` plots.

## consort 1.2.3

CRAN release: 2026-04-26

- Allow user configuration of arrow graphical parameters and padding
  with
  [`set_consort_defaults()`](https://adayim.github.io/consort/reference/set_consort_defaults.md).
- Allow custom bullet characters.
- Allow simple markup for bold, italic and superscript.
- Figure styles will also be applied to grViz plot.
- Improved documentation.
- Special thanks to [@Ramsas88](https://github.com/Ramsas88)

## consort 1.2.2

CRAN release: 2024-06-04

- Use comma separators for large number.
- Support two level randomisation/stratification
- Support multiple variables in one node
- Better node alignment
- Improved documentation

## consort 1.2.1

CRAN release: 2023-09-22

- Better numeric format in `gen_text`
- Bug in connection with `build_grviz`

## consort 1.2.0

CRAN release: 2023-04-11

- Able to have multiple split with `grViz`
- Improve node width calculation to avoid overlap.
- Bug in producing nodes for blank text.
- Bug in not drawing arrow after split.
- Bug in quotation for `grViz`
- Fixed some typos

## consort 1.1.0

CRAN release: 2023-01-05

- Re-write most of the codes, there’s some changes with the parameters.
- Improved the alignment of the nodes, no need to provide a coordinates.
- Now the plots will be drawn at the final stage.
- New function `build_grviz` and `build_grid`.
- Print the diagram with Shiny and HTML.

## consort 1.0.1

CRAN release: 2021-12-20

- Fixed error in lower `grid` version.
- Removed `gtable` dependency.
- Fixed auto align with middle.

## consort 1.0.0

CRAN release: 2021-11-04

- Many updates and changes in the consort building process.
- Removed `Gmisc` dependencies and added some functions for the
  replacement.
- Added some unit tests.
- Added option for text width.
- Fixed some typos.
- Fixed alignment issues and error if side box is blank.
- `build_consort` has been deprecated.
- Box adding functions return `gList`, and the `add_label` returns
  `gtable` with labels and flowchart.
- Various updates to the box adding functions, and supports the pipeline
  operators.
- Enhanced `connect_box` function.

## consort 0.2.0

CRAN release: 2021-09-10

- No side box if no subjects excluded.
- Add text align option for terminal box.
- Allow continuous terminal box.
- Exported box label generation function.
- Added some examples.

## consort 0.1.1

CRAN release: 2021-08-10

- Initial release.
