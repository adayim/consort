# Package index

## Diagram from data

Generate a CONSORT diagram from a participant disposition data set,
counts are calculated automatically.

- [`consort_plot()`](https://adayim.github.io/consort/reference/consort_plot.md)
  : Self generating consort diagram
- [`gen_text()`](https://adayim.github.io/consort/reference/gen_text.md)
  : Generate label and bullet points
- [`dispos.data`](https://adayim.github.io/consort/reference/dispos.data.md)
  : Demo clinical trial disposition data

## Diagram step by step

Build a diagram node by node with a pipeline of `add_*()` functions.
Splits can be nested to any depth.

- [`add_box()`](https://adayim.github.io/consort/reference/add_box.md) :
  Add nodes
- [`add_split()`](https://adayim.github.io/consort/reference/add_split.md)
  : Add a splitting box
- [`add_side_box()`](https://adayim.github.io/consort/reference/add_side_box.md)
  : Add a side node
- [`add_label_box()`](https://adayim.github.io/consort/reference/add_label_box.md)
  : Add a vertically aligned label nodes on the left side.

## Plot and export

Draw a diagram with grid graphics, or as Graphviz code for HTML and
Shiny use.

- [`plot(`*`<consort>`*`)`](https://adayim.github.io/consort/reference/plot.consort.md)
  [`print(`*`<consort>`*`)`](https://adayim.github.io/consort/reference/plot.consort.md)
  : Add methods to print function
- [`build_grid()`](https://adayim.github.io/consort/reference/build_grid.md)
  : Build consort diagram
- [`build_grviz()`](https://adayim.github.io/consort/reference/build_grviz.md)
  : Build consort diagram as Graphviz DOT code

## Appearance

Package-wide defaults for text, boxes, arrows, padding and markup.

- [`set_consort_defaults()`](https://adayim.github.io/consort/reference/set_consort_defaults.md)
  [`get_consort_defaults()`](https://adayim.github.io/consort/reference/set_consort_defaults.md)
  [`init_consort_defaults()`](https://adayim.github.io/consort/reference/set_consort_defaults.md)
  [`print(`*`<consort_defaults>`*`)`](https://adayim.github.io/consort/reference/set_consort_defaults.md)
  : Set consort diagram default options

## Low-level building blocks

Create, move and connect individual boxes.

- [`textbox()`](https://adayim.github.io/consort/reference/textbox.md)
  [`grid.textbox()`](https://adayim.github.io/consort/reference/textbox.md)
  : Create a box with text
- [`move_box()`](https://adayim.github.io/consort/reference/move_box.md)
  : Move a box grob
- [`connect_box()`](https://adayim.github.io/consort/reference/connect_box.md)
  : Connect grob box with arrow.
- [`get_coords()`](https://adayim.github.io/consort/reference/get_coords.md)
  : Get the coordinates of the textbox object
