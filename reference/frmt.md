# Table Value Formatting

These functions provide an abstracted way to approach to define
formatting of table contents. By defining in this way, the formats can
be layered to be more specific and general cell styling can be done
first.

`frmt()` is the base definition of a format. This defines spacing,
rounding, and missing behaviour.

`frmt_combine()` is used when two or more rows need to be combined into
a single cell in the table. Each of the rows needs to have a defined
`frmt()` and need to share a label.

`frmt_when()` is used when a rows format behaviour is dependent on the
value itself and is written similarly to
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case_when.html).
The left hand side of the equation is a `"TRUE"`for the default case or
the right hand side of a boolean expression `">50"`.

## Usage

``` r
frmt(expression, missing = NULL, scientific = NULL, transform = NULL, ...)

frmt_combine(expression, ..., missing = NULL)

frmt_when(..., missing = NULL)
```

## Arguments

- expression:

  this is the string representing the intended format. See details:
  expression for more a detailed description.

- missing:

  when a value is missing that is intended to be formatted, what value
  to place. See details: missing for more a detailed description.

- scientific:

  a string representing the intended scientific notation to be appended
  to the expression. Ex. "e^XX" or " x10^XX".

- transform:

  this is what should happen to the value prior to formatting, It should
  be a formula or function. Ex. `~.*100`if you want to convert a percent
  from a decimal prior to rounding

- ...:

  See details: `...` for a detailed description.

## Value

frmt object

## Details

### expression

- `frmt()` All numbers are represented by "x". Any additional character
  are printed as-is. If additional X's present to the left of the
  decimal point than the value, they will be represented as spaces.

- `frmt_combine()` defines how the parameters will be combined as a
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  statement. Parameters need to be equal to the values in the param
  column and defined in the expression as `"{param1} {param2}"`.

### missing

- `frmt()` Value to enter when the value is missing. When NULL, the
  value is "".

- `frmt_combine()` defines how when all values to be combined are
  missing. When NULL the value is "".

### ...

- `frmt()` These dots are for future extensions and must be empty.

- `frmt_combine()` accepts named arguments defining the `frmt()` to be
  applied to which parameters before being combined.

- `frmt_when()`accepts a series of equations separated by commas,
  similar to
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case_when.html).
  The left hand side of the equation is a `"TRUE"`for the default case
  or the right hand side of a boolean expression `">50"`. The right hand
  side of the equation is the `frmt()` to apply when the left side
  evaluates to `TRUE`.

## See also

[`body_plan()`](https://gsk-biostatistics.github.io/tfrmt/reference/body_plan.md)
combines the frmt_structures to be applied to the table body, and
[`frmt_structure()`](https://gsk-biostatistics.github.io/tfrmt/reference/frmt_structure.md)
defines which rows the formats will be applied to.

[Link to related
article](https://gsk-biostatistics.github.io/tfrmt/articles/body_plan.html)

## Examples

``` r
frmt("XXX %")
#> < frmt | Expression: `XXX %` >

frmt("XX.XXX")
#> < frmt | Expression: `XX.XXX` >

frmt("xx.xx", scientific = "x10^xx")
#> < frmt | Expression: `xx.xx` >

frmt_combine(
 "{param1} {param2}",
 param1 = frmt("XXX %"),
 param2 = frmt("XX.XXX")
)
#> < frmt_combine | Expression: `{param1} {param2}` >

frmt_when(
  ">3" ~ frmt("(X.X%)"),
  "<=3" ~ frmt("Undetectable")
  )
#> < frmt_when |  
#>   >3 ~ < frmt | Expression: `(X.X%)` >
#>   <=3 ~ < frmt | Expression: `Undetectable` > 
#>   Missing:  
#>  >

frmt_when(
  "==100"~ frmt(""),
  "==0"~ "",
  "TRUE" ~ frmt("(XXX.X%)")
  )
#> < frmt_when |  
#>   ==100 ~ < frmt | Expression: `` >
#>   ==0 ~ 
#>   TRUE ~ < frmt | Expression: `(XXX.X%)` > 
#>   Missing:  
#>  >
```
