
# tlang

<!-- badges: start -->

[![R-CMD-check](https://github.com/GSK-Biostatistics/tlang/workflows/R-CMD-check/badge.svg)](https://github.com/GSK-Biostatistics/tlang/actions)
[![Codecov test
coverage](https://codecov.io/gh/GSK-Biostatistics/tlang/branch/main/graph/badge.svg)](https://app.codecov.io/gh/GSK-Biostatistics/tlang?branch=main)
<!-- badges: end -->

The tlang package provides a language for defining display-related
metadata, which can then be used to automate and easily update output
formats.

In clinical trials, displays are generally quite standard, but frequent,
highly specific formatting tweaks (e.g., rounding, footnotes, headers)
are very common. Prior to data analysis, study teams often generate mock
displays to represent the desired end product for sponsors to approve or
programmers to replicate. This process is typically highly manual and
separate from the programming itself. There is also a high importance
placed on verifying the accuracy of the results via a QC process such as
double programming. Finally, there is a movement toward an industry
standard data structure for Analysis Results Data “ARD”, which means
analysis results datasets will have consistent structures and column
names. Specifically, the ARD is long, with 1 record per computed value.

tlang supports a vision where:

-   Mock displays are integrated with the programming workflow
-   Results are QC’ed prior to formatting to reduce rework
-   Standard formatting styles can be applied in as little as one line
    of code
-   The ARD structure can be leveraged to accommodate a variety of
    tables

By reducing the amount of repetitive tasks, study teams can focus on the
quality and interpretation of the results themselves.

# Why tlang?

While there are many existing table-making packages in the R ecosystem,
they typically fall into one of two categories:

-   Table packages that perform analyses and format the results
-   Table packages that format and output existing data

By design, tlang is more of the latter, as it is intended to be used
after the results have been computed. What makes tlang unique, however,
is that it offers an intuitive interface for defining and layering
standard or custom formats that are often specific to clinical trials.
It also offers the novel ability to easily generate mock displays using
metadata that will be used for the actual displays. tlang is built on
top of the powerful gt package, which is intended to support a variety
of output formats in the future.

# Installation

The development version of tlang can be installed with:

``` r
devtools::install_github("GSK-Biostatistics/tlang")
```

# Input data structure

We expect an input dataset that is long, with 1 record per computed
value. Required columns include:

-   \[Optional\] 1 or more **group** columns, containing grouping values
-   A single **label** column, containing row label values
-   1 or more **column** columns, containing column values
-   A single **param** column, which provides a label for distinct types
    of values
-   A single **values** column, containing the computed, raw data values
-   \[Optional\] 1 or more **sorting_cols** columns, containing numeric
    vlaues to be used in the row ordering

# Functionality

Here is an overview of what is possible with tlang:

-   Create a “tfrmt” metadata object containing all formatting and
    labeling for the display
-   Layer tfrmt objects to combine standard formatting with
    user-specific formatting
-   Create mock displays based on existing sample data or no prior data
-   ARD-standard compliant facilitates reuse and automation

Other benefits of tlang:

-   Provides a tidyverse-friendly, pipeable interface
-   Leverages gt as output engine, which allows for further
    customizations within gt itself

# More Info

For more information about how to build your own tlang mocks/tables
(like the one below!), please explore the vignettes.

<div id="lbeixknuds" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lbeixknuds .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lbeixknuds .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lbeixknuds .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lbeixknuds .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lbeixknuds .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbeixknuds .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lbeixknuds .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lbeixknuds .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lbeixknuds .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lbeixknuds .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lbeixknuds .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lbeixknuds .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: rgba(255, 255, 255, 0);
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: rgba(255, 255, 255, 0);
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#lbeixknuds .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: rgba(255, 255, 255, 0);
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: rgba(255, 255, 255, 0);
  vertical-align: middle;
}

#lbeixknuds .gt_from_md > :first-child {
  margin-top: 0;
}

#lbeixknuds .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lbeixknuds .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lbeixknuds .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: rgba(255, 255, 255, 0);
  padding-left: 5px;
  padding-right: 5px;
}

#lbeixknuds .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: rgba(255, 255, 255, 0);
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lbeixknuds .gt_row_group_first td {
  border-top-width: 2px;
}

#lbeixknuds .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbeixknuds .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lbeixknuds .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lbeixknuds .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbeixknuds .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbeixknuds .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lbeixknuds .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lbeixknuds .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbeixknuds .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lbeixknuds .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbeixknuds .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lbeixknuds .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbeixknuds .gt_left {
  text-align: left;
}

#lbeixknuds .gt_center {
  text-align: center;
}

#lbeixknuds .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lbeixknuds .gt_font_normal {
  font-weight: normal;
}

#lbeixknuds .gt_font_bold {
  font-weight: bold;
}

#lbeixknuds .gt_font_italic {
  font-style: italic;
}

#lbeixknuds .gt_super {
  font-size: 65%;
}

#lbeixknuds .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#lbeixknuds .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#lbeixknuds .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lbeixknuds .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#lbeixknuds .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#lbeixknuds .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="2"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Placebo</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Xanomeline Low Dose</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Xanomeline High Dose</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">p-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_row_group_first"><td rowspan="11" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Age (y)</td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Mean</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">SD</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Median</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Min</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Max</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">&lt;65 yrs</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">65-80 yrs</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">&gt;80 yrs</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr class="gt_row_group_first"><td rowspan="4" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Sex</td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Male</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Female</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr class="gt_row_group_first"><td rowspan="6" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Race (Origin)</td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Caucasian</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">African Descent</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Hispanic</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Other</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr class="gt_row_group_first"><td rowspan="7" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">MMSE</td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Mean</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">SD</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Median</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Min</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Max</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr class="gt_row_group_first"><td rowspan="10" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Duration of disease </td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Mean</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">SD</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Median</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Min</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Max</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">&lt;12 months</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">&gt;=12 months</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr class="gt_row_group_first"><td rowspan="7" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Years of education</td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Mean</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">SD</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Median</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Min</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Max</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr class="gt_row_group_first"><td rowspan="7" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Baseline weight(kg)</td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Mean</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">SD</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Median</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Min</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Max</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr class="gt_row_group_first"><td rowspan="7" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Baseline height(cm)</td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Mean</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">SD</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Median</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Min</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Max</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr class="gt_row_group_first"><td rowspan="11" class="gt_row gt_right gt_stub_row_group" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Baseline BMI</td>
<td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">n</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx         </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Mean</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">SD</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.xx      </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Median</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Min</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">Max</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx.x       </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">&lt;25</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">x.xxx</td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">25-&lt;30</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">&gt;=30</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">xxx (xx.x %)</td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
    <tr><td class="gt_row gt_right gt_stub" style="text-align: left; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">               </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">            </td>
<td class="gt_row gt_left" style="white-space: pre; border-top-width: 1px; border-top-style: solid; border-top-color: transparent; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: transparent;">     </td></tr>
  </tbody>
  
  
</table>
</div>
