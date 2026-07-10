# Reset or Remove a Component from a tfrmt Object

This utility function allows users to remove a specific component (e.g.,
`body_plan`, `row_grp_plan`, `col_plan`) from a `tfrmt` object by
setting it to `NULL`.

## Usage

``` r
reset_component(tfrmt_obj, component_name)
```

## Arguments

- tfrmt_obj:

  A `tfrmt` object to be modified.

- component_name:

  A character string specifying the name of the component to
  reset/remove (e.g., "body_plan").

## Value

A modified `tfrmt` object with the specified component removed.

## Examples

``` r
if (FALSE) { # \dontrun{
  my_tfrmt <- tfrmt(column = "col1", label = "label1")
  # Remove the column component
  reset_tfrmt <- reset_component(my_tfrmt, "column")
} # }
```
