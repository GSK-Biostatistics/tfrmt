# ARDs and expected input

### What is an ARD?

An **Analysis Results Data (ARD)** provides statistical analysis results
in a structured, machine-readable format. It provides a way to store
analysis outcomes in a consistent manner, making it easier to reuse and
integrate those results into reports, tables, and figures.

### Key Features of an ARD

- **Structured Data**: The ARD format organizes statistical results,
  ensuring they are easily interpreted and accessible for further
  analysis.

- **Reusability**: Once created, an ARD can be reused for future
  reporting needs, eliminating the need for repetitive data processing.

- **No Layout Description**: The ARD focuses on the data itself, not on
  the visual layout of tables or figures. This means you can use the
  data to generate reports in various formats.

### How to Create an ARD

ARDs can easily be created using the powerful **cards** package. For
more details, visit the [cards
documentation](https://insightsengineering.github.io/cards/latest-tag/index.html).

If you’re new to ARDs or the cards package, check out the following
[Other
Resources](https://insightsengineering.github.io/cards/latest-tag/#other-resources).

For information on the **Analysis Results Standard (ARS)**, visit this
[CDISC Webinar on
ARS](https://www.cdisc.org/events/webinar/analysis-results-standard-public-review).

## Dataset Structure and Column Requirements

We expect a long-format dataset, with one record per computed value.

Required columns are as follows:

- **\[Optional\] Grouping columns**: One or more columns containing
  grouping values.

- **Label column**: A single column containing row label values.

- **Column value columns**: One or more columns containing the values
  defining each column in your table. These are essentially additional
  grouping variables.

- **Param column**: A single column that labels distinct types of
  values.

- **Value column**: A single column containing the computed raw data
  values. Note: these must be numeric.

- **\[Optional\] Sorting columns**: One or more columns with numeric
  values to control row ordering.

For information on transforming ARD output for use in {tfrmt} see the
[ARD-first tables using
{cards}](https://gsk-biostatistics.github.io/tfrmt/articles/cards_to_tfrmt.html)
vignette.
