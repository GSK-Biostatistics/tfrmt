# Footnote Structure

Footnote Structure

## Usage

``` r
footnote_structure(
  footnote_text,
  column_val = NULL,
  group_val = NULL,
  label_val = NULL
)
```

## Arguments

- footnote_text:

  string with text for footnote

- column_val:

  string or a named list of strings which represent the column to apply
  the footnote to

- group_val:

  string or a named list of strings which represent the value of group
  to apply the footnote to

- label_val:

  string which represents the value of label to apply the footnote to

## Value

footnote structure object

## Examples

``` r
# Adds a source note aka a footnote without a symbol in the table
footnote_structure <- footnote_structure(footnote_text = "Source Note")

# Adds a footnote to the 'Placebo' column
footnote_structure <- footnote_structure(footnote_text = "Text",
                               column_val = "Placebo")

# Adds a footnote to either 'Placebo' or 'Treatment groups' depending on which
# which is last to appear in the column vector
footnote_structure <- footnote_structure(footnote_text = "Text",
      column_val = list(col1 = "Placebo", col2= "Treatment groups"))

# Adds a footnote to the 'Adverse Event' label
footnote_structure <- footnote_structure("Text", label_val = "Adverse Event")
```
