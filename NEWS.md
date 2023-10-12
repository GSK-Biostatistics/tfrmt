# tfrmt 0.0.4

New features:
* Improved column alignment capabilities (via `col_style_plan`). Alignment options now fall into two types: character (type = "char") and positional (type = "pos"). Positional alignment is new and allows for aligning across multiple positions. 
* Add `page_plan` for splitting tables across multiple pages
* Add ability to add a group/label header via the `col_plan`

Bug fixes:
* `frmt_combine` no longer throws error if group variable is named "var"
* `row_grp_plan` with post-space no longer throws error if character column contains NA values

# tfrmt 0.0.3

* Fixed bugs with JSON read/write 
* Added transformation capabilities to `frmt()`


# tfrmt 0.0.2

* Added a `NEWS.md` file to track changes to the package.
* Added functionality to read/write tfrmts to JSON files 
* Updates made to work with the newest version of dplyr 
