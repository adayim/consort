# consort2 2.0.0 (Development)

## Major Changes

This is a complete modernization and simplification of the consort package.

### New Features

* **Simplified API**: New declarative API makes creating diagrams easier
* **Enhanced Styling**: Full control over line widths, colors, arrows, and spacing (addresses Issue #28)
* **Better Validation**: Improved error messages and data validation
* **Modern Dependencies**: Updated to use latest Graphviz rendering

### Breaking Changes

* Package renamed to consort2 to allow parallel installation
* New S3 class structure for consort objects
* Simplified function names and parameters
* Primary focus on Graphviz rendering (grid as fallback)

### Improvements

* Reduced codebase from 18+ files to 8 core files
* 40-50% reduction in code complexity
* Comprehensive test coverage (target: 90%+)
* Modern documentation with pkgdown website

### Bug Fixes

* Fixed arrow rendering issues (Issue #26)
* Improved coordinate calculations
* Better handling of missing nodes

### Migration Guide

See vignettes for migration guide from consort v1.x to consort2 v2.0

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
