# consort 1.2.2

-   Use comma separators for large number. 
-   Support two level randomisation/stratification
-   Support multiple variables in one node
-   Better node alignment
-   Improved documentation

# consort 1.2.1

-   Better numeric format in `gen_text`
-   Bug in connection with `build_grviz`

# consort 1.2.0

-   Able to have multiple split with `grViz`
-   Improve node width calculation to avoid overlap.
-   Bug in producing nodes for blank text.
-   Bug in not drawing arrow after split.
-   Bug in quotation for `grViz`
-   Fixed some typos

# consort 1.1.0

-   Re-write most of the codes, there's some changes with the parameters.
-   Improved the alignment of the nodes, no need to provide a coordinates.
-   Now the plots will be drawn at the final stage.
-   New function `build_grviz` and `build_grid`.
-   Print the diagram with Shiny and HTML.

# consort 1.0.1

-   Fixed error in lower `grid` version.
-   Removed `gtable` dependency.
-   Fixed auto align with middle.

# consort 1.0.0

-   Many updates and changes in the consort building process.
-   Removed `Gmisc` dependencies and added some functions for the replacement.
-   Added some unit tests.
-   Added option for text width.
-   Fixed some typos.
-   Fixed alignment issues and error if side box is blank.
-   `build_consort` has been deprecated.
-   Box adding functions return `gList`, and the `add_label` returns `gtable` with labels and flowchart.
-   Various updates to the box adding functions, and supports the pipeline operators.
-   Enhanced `connect_box` function.

# consort 0.2.0

-   No side box if no subjects excluded.
-   Add text align option for terminal box.
-   Allow continuous terminal box.
-   Exported box label generation function.
-   Added some examples.

# consort 0.1.1

-   Initial release.
