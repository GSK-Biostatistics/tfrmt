
# tfrmt <a href='https://gsk-biostatistics.github.io/tfrmt/'><img src="https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/man/figures/tfrmt.png" align="right" alt = "tfrmt logo" style="height:139px;"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/GSK-Biostatistics/tfrmt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GSK-Biostatistics/tfrmt/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/GSK-Biostatistics/tfrmt/branch/main/graph/badge.svg)](https://app.codecov.io/gh/GSK-Biostatistics/tfrmt?branch=main)
[![status:
experimental](https://github.com/GIScience/badges/raw/master/status/experimental.svg)](https://github.com/GIScience/badges#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tfrmt)](https://CRAN.R-project.org/package=tfrmt)
<!-- badges: end -->

The tfrmt package provides a language for defining display-related
metadata, which can then be used to automate and easily update output
formats.

In clinical trials, displays are generally quite standard, but frequent,
highly specific formatting tweaks (e.g., rounding, footnotes, headers)
are very common. Prior to data analysis, study teams often generate mock
displays to represent the desired end product for sponsors to approve or
programmers to replicate. This process is typically highly manual and
separate from the programming itself. There is also a high importance
placed on verifying the accuracy of the results via a QC (Quality
Control) process such as double programming. Finally, there is a
movement toward an industry standard data structure for Analysis Results
Data “ARD”, which means analysis results datasets will have consistent
structures and column names. Specifically, the ARD is long, with 1
record per computed value. For more information about ARDs click
[here](https://pharmasug.org/download/sde/rtp2021/PharmaSUG-NCSDE_2021-08.pdf).

tfrmt supports a vision where:

- Mock displays are integrated with the programming workflow
- Results are QC’ed prior to formatting to reduce rework
- Standard formatting styles can be applied in as little as one line of
  code
- The ARD structure can be leveraged to accommodate a variety of tables

By reducing the amount of repetitive tasks, study teams can focus on the
quality and interpretation of the results themselves.

# Why tfrmt?

While there are many existing table-making packages in the R ecosystem,
they typically fall into one of two categories:

- Table packages that perform analyses and format the results
- Table packages that format and output existing data

By design, tfrmt is more of the latter, as it is intended to be used
after the results have been computed. What makes tfrmt unique, however,
is that it offers an intuitive interface for defining and layering
standard or custom formats that are often specific to clinical trials.
It also offers the novel ability to easily generate mock displays using
metadata that will be used for the actual displays. tfrmt is built on
top of the powerful gt package, which is intended to support a variety
of output formats in the future.

# Installation

The tfrmt package can be installed from CRAN with:

``` r
install.packages("tfrmt")
```

The development version of tfrmt can be installed with:

``` r
devtools::install_github("GSK-Biostatistics/tfrmt")
```

# Input data structure

We expect an input dataset that is long, with 1 record per computed
value. Required columns include:

- \[Optional\] 1 or more **group** columns, containing grouping values
- A single **label** column, containing row label values
- 1 or more **column** columns, containing column values
- A single **param** column, which provides a label for distinct types
  of values
- A single **value** column, containing the computed, raw data values
- \[Optional\] 1 or more **sorting_cols** columns, containing numeric
  values to be used in the row ordering

# Functionality

Here is an overview of what is possible with tfrmt:

- Create a “tfrmt” metadata object containing all formatting and
  labelling for the display
- Create mock displays based on existing sample data or no prior data
- ARD-standard compliant facilitates reuse and automation

Other benefits of tfrmt:

- Provides a tidyverse-friendly, pipeable interface
- Leverages gt as output engine, which allows for further customizations
  within gt itself

# More Info

For more information about how to build your own tfrmt mocks/tables
(like the one below!), please explore the
[vignettes](https://gsk-biostatistics.github.io/tfrmt/articles/examples.html).

<figure>
<img
src="https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/main/man/figures/gt_readme.png"
alt="Example GT Demography Table" />
<figcaption aria-hidden="true">Example GT Demography Table</figcaption>
</figure>

## Other Resources

<div id="phhnjkvvwf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#phhnjkvvwf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#phhnjkvvwf thead, #phhnjkvvwf tbody, #phhnjkvvwf tfoot, #phhnjkvvwf tr, #phhnjkvvwf td, #phhnjkvvwf th {
  border-style: none;
}
&#10;#phhnjkvvwf p {
  margin: 0;
  padding: 0;
}
&#10;#phhnjkvvwf .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#phhnjkvvwf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#phhnjkvvwf .gt_title {
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
&#10;#phhnjkvvwf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#phhnjkvvwf .gt_heading {
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
&#10;#phhnjkvvwf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#phhnjkvvwf .gt_col_headings {
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
&#10;#phhnjkvvwf .gt_col_heading {
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
&#10;#phhnjkvvwf .gt_column_spanner_outer {
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
&#10;#phhnjkvvwf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#phhnjkvvwf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#phhnjkvvwf .gt_column_spanner {
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
&#10;#phhnjkvvwf .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#phhnjkvvwf .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
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
  vertical-align: middle;
  text-align: left;
}
&#10;#phhnjkvvwf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#phhnjkvvwf .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#phhnjkvvwf .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#phhnjkvvwf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#phhnjkvvwf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phhnjkvvwf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#phhnjkvvwf .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#phhnjkvvwf .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#phhnjkvvwf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phhnjkvvwf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#phhnjkvvwf .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#phhnjkvvwf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#phhnjkvvwf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phhnjkvvwf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#phhnjkvvwf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#phhnjkvvwf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#phhnjkvvwf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#phhnjkvvwf .gt_footnotes {
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
&#10;#phhnjkvvwf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phhnjkvvwf .gt_sourcenotes {
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
&#10;#phhnjkvvwf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phhnjkvvwf .gt_left {
  text-align: left;
}
&#10;#phhnjkvvwf .gt_center {
  text-align: center;
}
&#10;#phhnjkvvwf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#phhnjkvvwf .gt_font_normal {
  font-weight: normal;
}
&#10;#phhnjkvvwf .gt_font_bold {
  font-weight: bold;
}
&#10;#phhnjkvvwf .gt_font_italic {
  font-style: italic;
}
&#10;#phhnjkvvwf .gt_super {
  font-size: 65%;
}
&#10;#phhnjkvvwf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#phhnjkvvwf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#phhnjkvvwf .gt_indent_1 {
  text-indent: 5px;
}
&#10;#phhnjkvvwf .gt_indent_2 {
  text-indent: 10px;
}
&#10;#phhnjkvvwf .gt_indent_3 {
  text-indent: 15px;
}
&#10;#phhnjkvvwf .gt_indent_4 {
  text-indent: 20px;
}
&#10;#phhnjkvvwf .gt_indent_5 {
  text-indent: 25px;
}
&#10;#phhnjkvvwf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#phhnjkvvwf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  &#10;  <tbody class="gt_table_body">
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>2022 R/Pharma</span></td>
<td headers="presenters" class="gt_row gt_left">Christina Fillmore</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>Why do I spend all my life formatting tables?</span></td>
<td headers="url_slides" class="gt_row gt_center"><br /></td>
<td headers="url_video" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://www.youtube.com/watch?v=00lGhuANUJw" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M549.655 124.083c-6.281-23.65-24.787-42.276-48.284-48.597C458.781 64 288 64 288 64S117.22 64 74.629 75.486c-23.497 6.322-42.003 24.947-48.284 48.597-11.412 42.867-11.412 132.305-11.412 132.305s0 89.438 11.412 132.305c6.281 23.65 24.787 41.5 48.284 47.821C117.22 448 288 448 288 448s170.78 0 213.371-11.486c23.497-6.321 42.003-24.171 48.284-47.821 11.412-42.867 11.412-132.305 11.412-132.305s0-89.438-11.412-132.305zm-317.51 213.508V175.185l142.739 81.205-142.739 81.201z"/></svg></a></span></td></tr>
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>2023 R/Pharma</span></td>
<td headers="presenters" class="gt_row gt_left">Becca Krouse</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>Everyone’s Invited: A Case Study on Bridging the Usability Gap</span></td>
<td headers="url_slides" class="gt_row gt_center"><br /></td>
<td headers="url_video" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://www.youtube.com/watch?v=Zg1LPJSO0kQ" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M549.655 124.083c-6.281-23.65-24.787-42.276-48.284-48.597C458.781 64 288 64 288 64S117.22 64 74.629 75.486c-23.497 6.322-42.003 24.947-48.284 48.597-11.412 42.867-11.412 132.305-11.412 132.305s0 89.438 11.412 132.305c6.281 23.65 24.787 41.5 48.284 47.821C117.22 448 288 448 288 448s170.78 0 213.371-11.486c23.497-6.321 42.003-24.171 48.284-47.821 11.412-42.867 11.412-132.305 11.412-132.305s0-89.438-11.412-132.305zm-317.51 213.508V175.185l142.739 81.205-142.739 81.201z"/></svg></a></span></td></tr>
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>2022 R/Pharma Workshop</span></td>
<td headers="presenters" class="gt_row gt_left">Christina Fillmore, Ellis Hughes and Thomas Neitmann</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>Clinical Reporting in R (Day 2)</span></td>
<td headers="url_slides" class="gt_row gt_center"><br /></td>
<td headers="url_video" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://www.youtube.com/watch?v=rYflZhFDSZQ" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M549.655 124.083c-6.281-23.65-24.787-42.276-48.284-48.597C458.781 64 288 64 288 64S117.22 64 74.629 75.486c-23.497 6.322-42.003 24.947-48.284 48.597-11.412 42.867-11.412 132.305-11.412 132.305s0 89.438 11.412 132.305c6.281 23.65 24.787 41.5 48.284 47.821C117.22 448 288 448 288 448s170.78 0 213.371-11.486c23.497-6.321 42.003-24.171 48.284-47.821 11.412-42.867 11.412-132.305 11.412-132.305s0-89.438-11.412-132.305zm-317.51 213.508V175.185l142.739 81.205-142.739 81.201z"/></svg></a></span></td></tr>
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>2023 R/Pharma Workshop</span></td>
<td headers="presenters" class="gt_row gt_left">Thomas Neitmann, Pawel Rucki and Ellis Hughes</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>Leveraging and contributing to the the pharmaverse for clinical trial reporting in R</span></td>
<td headers="url_slides" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://github.com/posit-conf-2023/r-pharma" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M64 0C28.7 0 0 28.7 0 64V352c0 35.3 28.7 64 64 64H240l-10.7 32H160c-17.7 0-32 14.3-32 32s14.3 32 32 32H416c17.7 0 32-14.3 32-32s-14.3-32-32-32H346.7L336 416H512c35.3 0 64-28.7 64-64V64c0-35.3-28.7-64-64-64H64zM512 64V352H64V64H512z"/></svg></a></span></td>
<td headers="url_video" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>Posit conf 2024</span></td>
<td headers="presenters" class="gt_row gt_left">Daniel D. Sjoberg, Becca Krouse, Ellis Hughes, Andrew Bates and Casey Aguilar-Gervase</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>Flavors of the pharmaverse</span></td>
<td headers="url_slides" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://posit-conf-2024.github.io/pharmaverse/" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M64 0C28.7 0 0 28.7 0 64V352c0 35.3 28.7 64 64 64H240l-10.7 32H160c-17.7 0-32 14.3-32 32s14.3 32 32 32H416c17.7 0 32-14.3 32-32s-14.3-32-32-32H346.7L336 416H512c35.3 0 64-28.7 64-64V64c0-35.3-28.7-64-64-64H64zM512 64V352H64V64H512z"/></svg></a></span></td>
<td headers="url_video" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>Posit conf 2024</span></td>
<td headers="presenters" class="gt_row gt_left">Becca Krouse</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>Stitch by Stitch: The Art of Engaging New Users</span></td>
<td headers="url_slides" class="gt_row gt_center"><br /></td>
<td headers="url_video" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://www.youtube.com/watch?v=R3VMij_1aSE" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M549.655 124.083c-6.281-23.65-24.787-42.276-48.284-48.597C458.781 64 288 64 288 64S117.22 64 74.629 75.486c-23.497 6.322-42.003 24.947-48.284 48.597-11.412 42.867-11.412 132.305-11.412 132.305s0 89.438 11.412 132.305c6.281 23.65 24.787 41.5 48.284 47.821C117.22 448 288 448 288 448s170.78 0 213.371-11.486c23.497-6.321 42.003-24.171 48.284-47.821 11.412-42.867 11.412-132.305 11.412-132.305s0-89.438-11.412-132.305zm-317.51 213.508V175.185l142.739 81.205-142.739 81.201z"/></svg></a></span></td></tr>
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>R in Pharma 2024</span></td>
<td headers="presenters" class="gt_row gt_left">Daniel D. Sjoberg, Becca Krouse and Jack Talboys</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>Unlocking Analysis Results Datasets (ARDs)</span></td>
<td headers="url_slides" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://www.danieldsjoberg.com/ARD-RinPharma-workshop-2024/" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M64 0C28.7 0 0 28.7 0 64V352c0 35.3 28.7 64 64 64H240l-10.7 32H160c-17.7 0-32 14.3-32 32s14.3 32 32 32H416c17.7 0 32-14.3 32-32s-14.3-32-32-32H346.7L336 416H512c35.3 0 64-28.7 64-64V64c0-35.3-28.7-64-64-64H64zM512 64V352H64V64H512z"/></svg></a></span></td>
<td headers="url_video" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>PHUSE US Connect 2025</span></td>
<td headers="presenters" class="gt_row gt_left">Daniel D. Sjoberg and Becca Krouse</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>Analysis Results Datasets Using Open-Source Tools from the {pharmaverse}</span></td>
<td headers="url_slides" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://www.danieldsjoberg.com/ARD-PHUSE-workshop-2025/" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M64 0C28.7 0 0 28.7 0 64V352c0 35.3 28.7 64 64 64H240l-10.7 32H160c-17.7 0-32 14.3-32 32s14.3 32 32 32H416c17.7 0 32-14.3 32-32s-14.3-32-32-32H346.7L336 416H512c35.3 0 64-28.7 64-64V64c0-35.3-28.7-64-64-64H64zM512 64V352H64V64H512z"/></svg></a></span></td>
<td headers="url_video" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="venue" class="gt_row gt_left"><span class='gt_from_md'>Posit conf 2025</span></td>
<td headers="presenters" class="gt_row gt_left">Daniel D. Sjoberg, Becca Krouse, Ben Straub and Rammprasad Ganapathy</td>
<td headers="title" class="gt_row gt_left"><span class='gt_from_md'>End-to-End Submissions in R with the Pharmaverse</span></td>
<td headers="url_slides" class="gt_row gt_center"><span style="white-space: pre;"><a href="https://posit-conf-2025.github.io/pharmaverse/" target="_blank" style="color:#008B8B;text-decoration:underline;text-underline-position: under;display: inline-block;"><svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#808080;overflow:visible;position:relative;"><path d="M64 0C28.7 0 0 28.7 0 64V352c0 35.3 28.7 64 64 64H240l-10.7 32H160c-17.7 0-32 14.3-32 32s14.3 32 32 32H416c17.7 0 32-14.3 32-32s-14.3-32-32-32H346.7L336 416H512c35.3 0 64-28.7 64-64V64c0-35.3-28.7-64-64-64H64zM512 64V352H64V64H512z"/></svg></a></span></td>
<td headers="url_video" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  
</table>
</div>
