# Check if input is a frmt

Check if input is a frmt

Check if input is a frmt_combine

Check if input is a frmt_when

Check if input is a frmt_structure

Check if input is a row_grp_structure

## Usage

``` r
is_frmt(x)

is_frmt_combine(x)

is_frmt_when(x)

is_frmt_structure(x)

is_row_grp_structure(x)
```

## Arguments

- x:

  Object to check

## Value

'TRUE' if yes, 'FALSE' if no

## Examples

``` r
x1 <- frmt("XXX.XX")
is_frmt(x1)
#> [1] TRUE

x2 <- frmt_combine("XXX %","XX,XXX")
is_frmt_combine(x2)
#> [1] TRUE

x2 <- frmt_when(
">3" ~ frmt("(X.X%)"),
"<=3" ~ frmt("Undetectable")
)
is_frmt_when(x2)
#> [1] TRUE

x3 <- frmt_structure(
 group_val = c("group1"),
 label_val = ".default",
frmt("XXX")
)
is_frmt_structure(x3)
#> [1] TRUE

x4 <- row_grp_structure(group_val = c("A","C"), element_block(post_space = "---"))
is_row_grp_structure(x4)
#> [1] TRUE
```
