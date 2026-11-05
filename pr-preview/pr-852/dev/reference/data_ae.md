# Adverse Events Analysis Results Data

A dataset containing the results needed for an AE table. Using the CDISC
pilot data.

## Usage

``` r
data_ae
```

## Format

A data frame with 2,794 rows and 8 variables:

- AEBODSYS:

  highest level row labels: System Organ Class

- AETERM:

  more specific row labels: Preferred Term

- col2:

  higher level column names (spanners)

- col1:

  lower level column names

- param:

  parameter to explain each value

- value:

  values to put in a table

- ord1:

  controls ordering

- ord2:

  more ordering controls
