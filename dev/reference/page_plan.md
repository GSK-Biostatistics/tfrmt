# Page Plan

Defining the location and/or frequency of page splits with a series of
page_structure's and the row_every_n argument, respectively.

## Usage

``` r
page_plan(
  ...,
  note_loc = c("noprint", "preheader", "subtitle", "source_note"),
  max_rows = NULL
)
```

## Arguments

- ...:

  a series of
  [`page_structure()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/page_structure.md)
  separated by commas

- note_loc:

  Location of the note describing each table's subset value(s). Useful
  if the `page_structure` contains only ".default" values (meaning the
  table is split by every unique level of a grouping variable), and that
  variable is dropped in the col_plan. `preheader` only available for
  rtf output.

- max_rows:

  Option to set a maximum number of rows per page. Takes a numeric
  value.

## Value

page_plan object

## Examples

``` r
 # use of page_struct
 page_plan(
    page_structure(group_val = "grp1", label_val = "lbl1")
 )
#> $struct_list
#> $struct_list[[1]]
#> $group_val
#> [1] "grp1"
#> 
#> $label_val
#> [1] "lbl1"
#> 
#> attr(,"class")
#> [1] "page_structure" "structure"     
#> 
#> 
#> $note_loc
#> [1] "noprint"
#> 
#> $max_rows
#> NULL
#> 
#> attr(,"class")
#> [1] "page_plan" "plan"     

 # use of #  rows
 page_plan(
    max_rows = 5
 )
#> $struct_list
#> list()
#> 
#> $note_loc
#> [1] "noprint"
#> 
#> $max_rows
#> [1] 5
#> 
#> attr(,"class")
#> [1] "page_plan" "plan"     

```
