# Labs Analysis Results Data

A dataset containing the results needed for an labs results table. Using
the CDISC pilot data.

## Usage

``` r
data_labs
```

## Format

A data frame with 4,950 rows and 7 variables:

- group1:

  highest level row labels: Lab value class

- group2:

  more specific row labels: Lab parameter

- rowlbl:

  most specific row labels: Study visit

- col1:

  higher level column names (spanners)

- col2:

  lower level column names

- param:

  parameter to explain each value

- value:

  values to put in a table

- ord1:

  controls ordering

- ord2:

  more ordering controls

- ord3:

  more ordering controls
