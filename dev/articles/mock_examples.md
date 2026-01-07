# Mock Examples

``` r
library(tfrmt)
```

## AE Mocks

*Reusable tfrmt base layer:*

``` r
ae_mock_table <- function(tfrmt_obj) {
  tfrmt(
    title = "Table Name",
    subtitle = "Study ID: GSK12345",
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default", label_val = ".default",
        frmt_combine(
          "{count} {percent}",
          count = frmt("XXX"),
          percent = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("(XX.X%)")
          )
        )
      ),
      frmt_structure(
        group_val = ".default", label_val = ".default",
        frmt_combine(
          "{num} ({lower}, {upper})",
          num = frmt("XX.X"),
          lower = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("XX.X%")
          ),
          upper = frmt_when(
            "==100" ~ frmt(""),
            "==0" ~ "",
            "TRUE" ~ frmt("XX.X%")
          )
        )
      )
    ),
    col_plan = col_plan(
      T1, T2, `T1&T2`, PL,
      everything(),
      -starts_with("ord")
    )
  )
}
```

Example 1: mock table with no groups

``` r
df1 <- tidyr::crossing(label = c("label 1", "label 2", "label 3"),
           column = c("T1", "T2", "T1&T2", "PL"),
           param = c("count", "percent")) 
df11 <- df1 |>
    dplyr::mutate(ord1 = rep(seq(1:length(unique(df1$label))), each = nrow(df1)/length(unique(df1$label)) ))
  
  
df2 <- tidyr::crossing(label = c("label 1", "label 2", "label 3"),
           column = c("risk T1-PL", "risk T2-PL", "risk T1&T2-PL"),
           param = c("num", "lower", "upper")) 
df22<- df2 |>
    dplyr::mutate(ord1 = rep(seq(1:length(unique(df2$label))), each = nrow(df2)/length(unique(df2$label)) ))

df <- dplyr::bind_rows(df11, df22)  |>
  dplyr::arrange_all()


ae_mock_table() |>
  tfrmt(
    label = "label",
    param = "param",
    column = "column",
    value = value,
    sorting_cols = vars(ord1)
  ) |>
  print_mock_gt(df)
```

| Table Name         |             |             |             |             |                     |                     |                     |
|:-------------------|-------------|-------------|-------------|-------------|---------------------|---------------------|---------------------|
| Study ID: GSK12345 |             |             |             |             |                     |                     |                     |
|                    | T1          | T2          | T1&T2       | PL          | risk T1&T2-PL       | risk T1-PL          | risk T2-PL          |
| label 1            | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
| label 2            | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
| label 3            | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |

Example 2: mock table with one level of grouping, indented labels

``` r
df <- dplyr::bind_rows(
  tidyr::crossing(
    group = c("grp1", "grp2"),
    label = c("label 1", "label 2", "label 3"),
    column = c("T1", "T2", "T1&T2", "PL"),
    param = c("count", "percent")
  ),
  tidyr::crossing(
    group = c("grp1", "grp2"),
    label = c("label 1", "label 2", "label 3"),
    column = c("risk T1-PL", "risk T2-PL", "risk T1&T2-PL"),
    param = c("num", "lower", "upper")
  )
) |> dplyr::arrange_all()

ae_mock_table() |>
  tfrmt(
    group = group,
    label = "label",
    param = "param",
    column = "column",
    value = value,
    row_grp_plan = row_grp_plan(row_grp_structure(group_val = ".default", element_block(post_space = "   "))),
    col_plan = col_plan(
      T1, T2, `T1&T2`, PL, `risk T1-PL`, `risk T2-PL`, `risk T1&T2-PL`,
      everything(),
      -starts_with("ord")
    )
  ) |>
  print_mock_gt(df)
```

[TABLE]

Example 3: mock table with two levels of grouping, indented labels

``` r
df <- dplyr::bind_rows(
  tidyr::crossing(
    grp1 = c("group 1.1", "group 1.2"),
    grp2 = c("group 2.1", "group 2.2"),
    label = c("label 1", "label 2"),
    column = c("T1", "T2", "T1&T2", "PL"),
    param = c("count", "percent")
  ),
  tidyr::crossing(
    grp1 = c("group 1.1", "group 1.2"),
    grp2 = c("group 2.1", "group 2.2"),
    label = c("label 1", "label 2"),
    column = c("risk T1-PL", "risk T2-PL", "risk T1&T2-PL"),
    param = c("num", "lower", "upper")
  )
) |> dplyr::arrange_all()

ae_mock_table() |>
  tfrmt(
    group = c(grp1, grp2),
    label = "label",
    param = "param",
    column = "column",
    value = value,
    row_grp_plan = row_grp_plan(row_grp_structure(group_val = ".default", element_block(post_space = "   "))),
    col_plan = col_plan(
      T1, T2, `T1&T2`, PL, `risk T1-PL`, `risk T2-PL`, `risk T1&T2-PL`,
      everything(),
      -starts_with("ord")
    )
  ) |>
  print_mock_gt(df)
```

[TABLE]

Example 4: mock table with two levels of grouping, column separated
labels

``` r
ae_mock_table() |>
  tfrmt(
    group = c(grp1, grp2),
    label = "label",
    param = "param",
    column = "column",
    value = value,
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "column")),
    col_plan = col_plan(
      T1, T2, `T1&T2`, PL, `risk T1-PL`, `risk T2-PL`, `risk T1&T2-PL`,
      everything(),
      -starts_with("ord")
    )
  ) |>
  print_mock_gt(df)
```

| Table Name         |           |         |             |             |             |             |                     |                     |                     |
|:-------------------|-----------|---------|-------------|-------------|-------------|-------------|---------------------|---------------------|---------------------|
| Study ID: GSK12345 |           |         |             |             |             |             |                     |                     |                     |
|                    |           |         | T1          | T2          | T1&T2       | PL          | risk T1-PL          | risk T2-PL          | risk T1&T2-PL       |
| group 1.1          | group 2.1 | label 1 | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
|                    |           | label 2 | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
|                    | group 2.2 | label 1 | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
|                    |           | label 2 | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
| group 1.2          | group 2.1 | label 1 | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
|                    |           | label 2 | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
|                    | group 2.2 | label 1 | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |
|                    |           | label 2 | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XXX (XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) | XX.X (XX.X%, XX.X%) |

Example 5: mock table with combined columns, using data_ae

``` r
fmt_spec <- tfrmt(
  group = AEBODSYS,
  label = AETERM,
  param = param,
  column = c(col2, col1),
  value = value,
  row_grp_plan = row_grp_plan(),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine("{n} {pct}",
        n = frmt("XXX"),
        pct = frmt_when(
          "==100" ~ "",
          "==0" ~ "",
          TRUE ~ frmt("(xx.x %)")
        )
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      AEs = frmt("[XXX]")
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      pval = frmt_when(
        ">0.99" ~ ">0.99",
        "<0.001" ~ "<0.001",
        "<0.05" ~ frmt("x.xxx*"),
        TRUE ~ frmt("x.xxx", missing = "--")
      )
    )
  ), col_plan = col_plan(-starts_with("ord"))
)

#
data_ae2 <- data_ae |>
  dplyr::group_by(AEBODSYS, AETERM) |>
  dplyr::mutate(pct_high = value[col2 == "Xanomeline High Dose" & param == "pct"]) |>
  dplyr::ungroup() |>
  dplyr::filter(pct_high > 10) |>
  dplyr::select(-pct_high)

data_ae2 |>
  dplyr::select(-value) |>
  dplyr::arrange(ord1, ord2) |>
  print_mock_gt(fmt_spec, .data=_)
```

[TABLE]

## Demography Mocks

``` r
tfrmt(
  # specify columns in the data
  group = c(rowlbl1, grp),
  label = rowlbl2,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),

  # Specify body plan
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt_combine("{n} {pct}",
      n = frmt("xxx"),
      pct = frmt_when(
        "==100" ~ "",
        "==0" ~ "",
        TRUE ~ frmt("(xx.x %)")
      )
    )),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Mean", "Median", "Min", "Max"), frmt("xxx.x")),
    frmt_structure(group_val = ".default", label_val = "SD", frmt("xxx.xx")),
    frmt_structure(group_val = ".default", label_val = ".default", p = frmt("")),
    frmt_structure(group_val = ".default", label_val = c("n", "<65 yrs", "<12 months", "<25"), p = frmt_when(
      ">0.99" ~ ">0.99",
      "<0.001" ~ "<0.001",
      TRUE ~ frmt("x.xxx", missing = "")
    ))
  ),

  # Specify row group plan
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "column")
  ),

  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(align = c(".", ",", " "), col = vars(everything()))
  ),

  # remove extra cols
  col_plan = col_plan(
    -grp,
    -starts_with("ord")
  )
) |>
  print_mock_gt(data_demog |> dplyr::select(-value))
```

|                       |                    | Placebo      | Xanomeline Low Dose | Xanomeline High Dose | Total        | p-value |
|:----------------------|:-------------------|:-------------|:--------------------|:---------------------|:-------------|---------|
|      Age (y)          |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       |      Mean          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |        SD          | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                       |    Median          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Min          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Max          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |                    |              |                     |                      |              |         |
|                       |       \<65 yrs     | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) | x.xxx   |
|                       |     65-80 yrs      | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |       \>80 yrs     | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |                    |              |                     |                      |              |         |
|      Sex              |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       |      Male          | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |    Female          | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |                    |              |                     |                      |              |         |
|     Race (Origin)     |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       | Caucasian          | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |   African Descent  | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |  Hispanic          | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |     Other          | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |                    |              |                     |                      |              |         |
|     MMSE              |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       |      Mean          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |        SD          | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                       |    Median          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Min          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Max          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |                    |              |                     |                      |              |         |
| Duration of disease   |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       |      Mean          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |        SD          | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                       |    Median          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Min          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Max          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |                    |              |                     |                      |              |         |
|                       |       \<12 months  | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) | x.xxx   |
|                       |      \>=12 months  | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |                    |              |                     |                      |              |         |
|    Years of education |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       |      Mean          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |        SD          | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                       |    Median          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Min          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Max          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |                    |              |                     |                      |              |         |
| Baseline weight(kg)   |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       |      Mean          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |        SD          | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                       |    Median          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Min          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Max          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |                    |              |                     |                      |              |         |
| Baseline height(cm)   |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       |      Mean          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |        SD          | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                       |    Median          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Min          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Max          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |                    |              |                     |                      |              |         |
| Baseline BMI          |         n          | xxx          | xxx                 | xxx                  | xxx          | x.xxx   |
|                       |      Mean          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |        SD          | xxx.xx       | xxx.xx              | xxx.xx               | xxx.xx       |         |
|                       |    Median          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Min          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |       Max          | xxx.x        | xxx.x               | xxx.x                | xxx.x        |         |
|                       |                    |              |                     |                      |              |         |
|                       |       \<25         | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) | x.xxx   |
|                       |    25-\<30         | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |      \>=30         | xxx (xx.x %) | xxx (xx.x %)        | xxx (xx.x %)         | xxx (xx.x %) |         |
|                       |                    |              |                     |                      |              |         |

## Efficacy Mocks

``` r
tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = list(group = "Change from Baseline"), element_block(post_space = " ")),
    row_grp_structure(group_val = list(group = "p-value (Dose Response)"), element_block(post_space = " ")),
    row_grp_structure(group_val = list(group = "p-value (Xan - Placebo)"), element_block(post_space = " "))
  ),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = "n", frmt("xx")),
    frmt_structure(group_val = ".default", label_val = "Median (Range)", frmt_combine("{median} ({min};{max})",
      median = frmt("xx.x"),
      min = frmt("xx"),
      max = frmt("xx"), missing = " "
    )),
    frmt_structure(group_val = ".default", label_val = "Mean (SD)", frmt_combine("{mean} ({sd})",
      mean = frmt("xx.x"),
      sd = frmt("xx.xx"), missing = " "
    )),
    frmt_structure(group_val = ".default", label_val = "Diff of LS Means (SE)", frmt_combine("{diff} ({diff_se})",
      diff = frmt("xx.x"),
      diff_se = frmt("xx.xx"), missing = " "
    )),
    frmt_structure(group_val = ".default", label_val = "95% CI", frmt_combine("({diff_lcl};{diff_ucl})",
      diff_lcl = frmt("xx.x"),
      diff_ucl = frmt("xx.x"), missing = " "
    )),
    frmt_structure(group_val = ".default", label_val = ".default", p.value = frmt_when(
      "<0.001" ~ "<0.001",
      ">0.99" ~ ">0.99",
      TRUE ~ frmt("x.xxx", missing = " ")
    ))
  ),
  col_plan = col_plan(
    group, label,
    contains("Placebo"),
    contains("Low"),
    contains("High")
  )
) |>
  print_mock_gt(data_efficacy |> dplyr::select(-value))
```

[TABLE]
