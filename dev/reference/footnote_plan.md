# Footnote Plan

Defining the location and content of footnotes with a series of footnote
structures. Each structure is a footnote and can be applied in multiple
locations.

## Usage

``` r
footnote_plan(..., marks = c("numbers", "letters", "standard", "extended"))
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

## Value

footnote plan object

## Examples

``` r
# Adds a footnote indicated by letters rather than numbers to Group 1
footnote_plan <- footnote_plan(
    footnote_structure(footnote_text = "Source Note", group_val = "Group 1"),
    marks="letters")

# Adds a footnote to the 'Placebo' column
footnote_plan <- footnote_plan(
    footnote_structure(footnote_text = "footnote", column_val = "Placebo"),
    marks="numbers")
```
