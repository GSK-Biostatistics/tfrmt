# Changelog

## tfrmt 0.2.1

CRAN release: 2025-10-04

Patch release for latest {gt} release and upcoming {purrr} and {stringr}
releases: \* Ensure `rowname_col = NULL` in
[`gt()`](https://gt.rstudio.com/reference/gt.html) within
[`print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/print_to_gt.md)
if no row label exists. \* Use of `seq_along(x)` in lieu of
`1:length(x)` prior to `str_replace()` as it no longer accepts NA
patterns. \* Ensure values are character before processing via
[`map_chr()`](https://purrr.tidyverse.org/reference/map.html) as it no
longer coerces to character.

## tfrmt 0.2.0

CRAN release: 2025-09-06

### Improvements

- Working with ARDs created from the {cards} package:
  - Added
    [`shuffle_card()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/shuffle_card.md),
    which prepares a `card` ARD object for analysis.
  - Added `prep_...()` functions to reduce the manual processing
    required for a `card` ARD object once it has been shuffled.
    ([\#509](https://github.com/GSK-Biostatistics/tfrmt/issues/509),
    [@dragosmg](https://github.com/dragosmg))
    - [`prep_combine_vars()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_combine_vars.md):
      useful for combining sparsely populated columns into a single one.
    - [`prep_big_n()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_big_n.md):
      useful when the tfrmt contains a bigN structure. It recodes the
      `"n"` `stat_name` into `"bigN"` for the desired variables.
    - [`prep_label()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_label.md):
      combines `stat_label` and `variable_level` for categorical (or
      categorical-like) variables.
    - [`prep_hierarchical_fill()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_hierarchical_fill.md):
      does a pair-wise replacement of missing values (`NAs`) in one
      column based on another column. It can replace `NA` either with a
      predetermined value, with the value of the other column in the
      pair, or with a {glue} expression.
- Updated RTF footnotes so they go in the document footer by default.

### Bug fixes

- Fix issue where `*_structure` functions did not correctly parse
  strings containing quotes in some cases.
  ([\#466](https://github.com/GSK-Biostatistics/tfrmt/issues/466))
- Fix issue where page_plan doesn’t work if there is only one level in
  your paging variable.
  ([\#506](https://github.com/GSK-Biostatistics/tfrmt/issues/506))
- Fix issues where an error is thrown instead of a message if: (1)
  levels of page-specific big Ns do not align with levels of pagination
  ([\#505](https://github.com/GSK-Biostatistics/tfrmt/issues/505))
  or (2) the pagination levels are sorted non-alphabetically.
  ([\#516](https://github.com/GSK-Biostatistics/tfrmt/issues/516))
- Fix issue where the big_n_structure doesn’t work when the column name
  used for page_plan/grouping has a space in it.
  ([\#522](https://github.com/GSK-Biostatistics/tfrmt/issues/522))

## tfrmt 0.1.3

CRAN release: 2024-10-15

### Improvements

- Incorporate contents of `col_style_plan` in the creation of mock data.

### Bug fixes

- Fixed issue where JSON conversion of `frmt_when` dropped quotes from
  strings
- Avoid use of deprecated functionality in
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)

## tfrmt 0.1.2

CRAN release: 2024-07-06

### Bug fixes

- Fixed issue where table stub indentation does not transfer to all
  output types
- Fixed issue where incomplete `body_plan` may error if the grouping
  variable is a factor

## tfrmt 0.1.1

CRAN release: 2024-03-15

### Bug fixes

- Fixed issue where `frmt_combine` couldn’t process variable names
  surrounded by backticks
- Fixed issue where `row_grp_plan` post space did not respect
  `col_style_plan` widths by adding new `fill` argument to
  `element_block`. The `fill` argument controls whether post space
  values should be recycled for the cell’s data width. For example, a
  cell width of 3 will be respected by the post space with the following
  syntax: `element_block(post_space = "---", fill = FALSE)`.
- Remove unused `border` argument in `element_block`.
- Fixed bug where `row_grp_plan` splits on all grouping variables, even
  if not mentioned. Instead, the logic has been updated to split on
  those explicitly mentioned, similar to `page_plan`
- Fixed issue where padding and alignment is lost for non-HTML outputs
  via the `.unicode_ws` argument added to
  [`print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/print_to_gt.md)
  and
  [`print_mock_gt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/print_mock_gt.md).
  This defaults to `TRUE` but should be set to `FALSE` for RTF outputs
  (until {gt} bug is resolved).
- Fixed issue where `make_mock_data` could result in duplicate rows

## tfrmt 0.1.0

CRAN release: 2023-10-17

### New features:

- Improved column alignment capabilities (via `col_style_plan`).
  Alignment options now fall into two types: character (type = “char”)
  and positional (type = “pos”). Positional alignment is new and allows
  for aligning across multiple positions.
- Add `page_plan` for splitting tables across multiple pages
- Add ability to add a group/label header via the `col_plan`

### Breaking changes:

- Name of the list component inside `row_grp_plan` that stores
  `row_grp_structure`s has been changed from “struct_ls” to
  “struct_list” to be consistent with other objects. This may impact
  compatibility with JSON files created using prior versions of {tfrmt}.

### Bug fixes:

- `frmt_combine` no longer throws error if group variable is named “var”
- `row_grp_plan` with post-space no longer throws error if character
  column contains NA values

## tfrmt 0.0.3

CRAN release: 2023-06-22

- Fixed bugs with JSON read/write
- Added transformation capabilities to
  [`frmt()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/frmt.md)

## tfrmt 0.0.2

CRAN release: 2023-02-13

- Added a `NEWS.md` file to track changes to the package.
- Added functionality to read/write tfrmts to JSON files
- Updates made to work with the newest version of dplyr
