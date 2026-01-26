# Column Style Structure

Column Style Structure

## Usage

``` r
col_style_structure(
  col,
  align = NULL,
  type = c("char", "pos"),
  width = NULL,
  ...
)
```

## Arguments

- col:

  Column value to align on from `column` variable. May be a quoted or
  unquoted column name, a tidyselect semantic, or a span_structure.

- align:

  Alignment to be applied to column. Defaults to `left` alignment. See
  details for acceptable values.

- type:

  Type of alignment: "char" or "pos", for character alignment (default),
  and positional alignment, respectively. Positional alignment allows
  for aligning over multiple positions in the column.

- width:

  Width to apply to the column in number of characters. Acceptable
  values include a numeric value, or a character string of a number.

- ...:

  These dots are for future extensions and must be empty

## Value

col_style_structure object

## Details

Supports alignment and width setting of data value columns (values found
in the `column` column). Row group and label columns are left-aligned by
default. Acceptable input values for `align` differ by type = "char" or
"pos":

### Character alignment (type = "char"):

- "left" for left alignment

- "right" for right alignment"

- supply a vector of character(s) to align on. If more than one
  character is provided, alignment will be based on the first occurrence
  of any of the characters. For alignment based on white space, leading
  white spaces will be ignored.

### Positional alignment (type = "pos"):

supply a vector of strings covering all formatted cell values, with
numeric values represented as x's. These values can be created manually
or obtained by utilizing the helper
[`display_val_frmts()`](https://gsk-biostatistics.github.io/tfrmt/reference/display_val_frmts.md).
Alignment positions will be represented by vertical bars. For example,
with starting values: c("12.3", "(5%)", "2.35 (10.23)") we can align all
of the first sets of decimals and parentheses by providing align =
c("xx\|.x", "\|\|(x%)", "x\|.xx \|")

## See also

[`col_style_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/col_style_plan.md)
for more information on how to combine col_style_structure()'s together
to form a plan.

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/col_style_plan.html)

## Examples

``` r
 plan <- col_style_plan(
    col_style_structure(col = "my_var",
                        align = c("xx| |(xx%)",
                                  "xx|.x |(xx.x - xx.x)"),
                        type = "pos", width = 100),
    col_style_structure(col = vars(four), align = "right", width = 200),
    col_style_structure(col = vars(two, three), align = c(".", ",", " ")),
    col_style_structure(col = c(two, three), width = 25),
    col_style_structure(col = two, width = 25),
    col_style_structure(col = span_structure(span = value, col = val2),
                        width = 25)
   )
```
