# Footnote Plan

Defining the location and content of footnotes with a series of footnote
structures. Each structure is a footnote and can be applied in multiple
locations.

## Usage

``` r
footnote_plan(
  ...,
  marks = c("numbers", "letters", "standard", "extended"),
  order = c("marks_first", "preserve_order", "marks_last")
)
```

## Arguments

- ...:

  a series of
  [`footnote_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/footnote_structure.md)
  separated by commas

- marks:

  type of marks required for footnotes, properties inherited from
  tab_footnote in 'gt'. Available options are "numbers", "letters",
  "standard" and "extended" (standard for a traditional set of 4
  symbols, extended for 6 symbols). The default option is set to
  "numbers".

- order:

  how to order footnotes in the footer. Options are "marks_first"
  (anchored notes appear first, default), "preserve_order" or
  "marks_last" (general notes appear first).

## Value

footnote plan object

## Examples

``` r
# Adds a footnote indicated by letters rather than numbers to Group 1
footnote_plan <- footnote_plan(
    footnote_structure(footnote_text = "footnote", group_val = "Group 1"),
    marks = "letters")

# Adds a footnote to the 'Placebo' column
footnote_plan <- footnote_plan(
    footnote_structure(footnote_text = "footnote", column_val = "Placebo"),
    marks = "numbers")

# Preserve order of footnotes
footnote_plan <- footnote_plan(
    footnote_structure(footnote_text = "footnote 1", group_val = "Group 1"),
    footnote_structure(footnote_text = "footnote 2", column_val = "Placebo"),
    order = "marks_first")
```
