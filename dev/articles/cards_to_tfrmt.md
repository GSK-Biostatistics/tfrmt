# ARD-first tables using {cards}

``` r
library(tfrmt)
library(cards)
```

Preparing analysis results in a tidy ARD format is a prerequisite for a
{tfrmt} table. A useful tool for ARD creation is the
[{cards}](https://insightsengineering.github.io/cards/latest-tag/)
package. By design, the output of {cards} is display-agnostic and only
contains information about the summaries themselves. Thus, the output
requires a bit of manipulation to get the ARD in a display-ready format.
For example, a `cards` object will label all denominators as “N”, but
{tfrmt} expects header (i.e. population-level) big Ns to have a unique
label that sets them apart from other denominators, like “bigN”.

{tfrmt} contains several helper functions to ease the transition from
the `cards` output to a display-ready ARD fit for {tfrmt}. Below are two
examples utilizing these helpers for a demographics and AE table.

## Demog table

Demographics tables commonly contain a mix of categorical and continuous
summaries by treatment group. We can perform these summaries using the
following {cards} code:

``` r
ard_demog <- cards::ard_stack(
  data = pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y"),
  cards::ard_categorical(
    variables = "AGEGR1",
    statistic = ~ c("p")
  ),
  cards::ard_continuous(
    variables = "AGE",
    statistic = ~ cards::continuous_summary_fns(c("mean", "sd"))
  ),
  .by = "ARM"
)

ard_demog
#> {cards} data frame: 21 x 11
#>    group1 group1_level variable variable_level stat_name stat_label   stat
#> 1     ARM      Placebo   AGEGR1            >64         p          %  0.837
#> 2     ARM      Placebo   AGEGR1          18-64         p          %  0.163
#> 3     ARM      Placebo      AGE                     mean       Mean 75.209
#> 4     ARM      Placebo      AGE                       sd         SD   8.59
#> 5     ARM    Xanomeli…   AGEGR1            >64         p          %  0.869
#> 6     ARM    Xanomeli…   AGEGR1          18-64         p          %  0.131
#> 7     ARM    Xanomeli…      AGE                     mean       Mean 74.381
#> 8     ARM    Xanomeli…      AGE                       sd         SD  7.886
#> 9     ARM    Xanomeli…   AGEGR1            >64         p          %  0.905
#> 10    ARM    Xanomeli…   AGEGR1          18-64         p          %  0.095
#> ℹ 11 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, fmt_fun, warning, error
```

The first step in preparing the data for {tfrmt} is calling
[`shuffle_card()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/shuffle_card.md)
which performs general operations to get the results in a tidy data
frame without list-columns and extra metadata.

``` r
ard_demog_shuffled <- ard_demog |>
  shuffle_card()

ard_demog_shuffled
#> # A tibble: 21 × 8
#>    ARM           AGEGR1 AGE   context stat_variable stat_name stat_label    stat
#>    <chr>         <chr>  <chr> <chr>   <chr>         <chr>     <chr>        <dbl>
#>  1 Placebo       >64    NA    catego… AGEGR1        p         %           0.837 
#>  2 Placebo       18-64  NA    catego… AGEGR1        p         %           0.163 
#>  3 Placebo       NA     Over… contin… AGE           mean      Mean       75.2   
#>  4 Placebo       NA     Over… contin… AGE           sd        SD          8.59  
#>  5 Xanomeline H… >64    NA    catego… AGEGR1        p         %           0.869 
#>  6 Xanomeline H… 18-64  NA    catego… AGEGR1        p         %           0.131 
#>  7 Xanomeline H… NA     Over… contin… AGE           mean      Mean       74.4   
#>  8 Xanomeline H… NA     Over… contin… AGE           sd        SD          7.89  
#>  9 Xanomeline L… >64    NA    catego… AGEGR1        p         %           0.905 
#> 10 Xanomeline L… 18-64  NA    catego… AGEGR1        p         %           0.0952
#> # ℹ 11 more rows
```

Note that the variables have been transformed wide and renamed. NULL
values have also been filled with “Overall ”.

For our {tfrmt} table, `AGEGR1` and `AGE` need to be combined into a
single variable. We can use the
[`prep_combine_vars()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_combine_vars.md)
helper for this:

``` r
ard_demog_shuffled <- ard_demog_shuffled |>
  prep_combine_vars(vars = c("AGEGR1", "AGE"))

ard_demog_shuffled
#> # A tibble: 21 × 7
#>    ARM         variable_level context stat_variable stat_name stat_label    stat
#>    <chr>       <chr>          <chr>   <chr>         <chr>     <chr>        <dbl>
#>  1 Placebo     >64            catego… AGEGR1        p         %           0.837 
#>  2 Placebo     18-64          catego… AGEGR1        p         %           0.163 
#>  3 Placebo     Overall AGE    contin… AGE           mean      Mean       75.2   
#>  4 Placebo     Overall AGE    contin… AGE           sd        SD          8.59  
#>  5 Xanomeline… >64            catego… AGEGR1        p         %           0.869 
#>  6 Xanomeline… 18-64          catego… AGEGR1        p         %           0.131 
#>  7 Xanomeline… Overall AGE    contin… AGE           mean      Mean       74.4   
#>  8 Xanomeline… Overall AGE    contin… AGE           sd        SD          7.89  
#>  9 Xanomeline… >64            catego… AGEGR1        p         %           0.905 
#> 10 Xanomeline… 18-64          catego… AGEGR1        p         %           0.0952
#> # ℹ 11 more rows
```

Next, we want to make a row label column that is the variable level for
categorical `AGEGR1` and the stat label for continuous `AGE`. We can use
the
[`prep_label()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_label.md)
helper for this:

``` r
ard_demog_shuffled <- ard_demog_shuffled |>
  prep_label()

ard_demog_shuffled
#> # A tibble: 21 × 8
#>    ARM   variable_level context stat_variable stat_name stat_label    stat label
#>    <chr> <chr>          <chr>   <chr>         <chr>     <chr>        <dbl> <chr>
#>  1 Plac… >64            catego… AGEGR1        p         %           0.837  >64  
#>  2 Plac… 18-64          catego… AGEGR1        p         %           0.163  18-64
#>  3 Plac… Overall AGE    contin… AGE           mean      Mean       75.2    Mean 
#>  4 Plac… Overall AGE    contin… AGE           sd        SD          8.59   SD   
#>  5 Xano… >64            catego… AGEGR1        p         %           0.869  >64  
#>  6 Xano… 18-64          catego… AGEGR1        p         %           0.131  18-64
#>  7 Xano… Overall AGE    contin… AGE           mean      Mean       74.4    Mean 
#>  8 Xano… Overall AGE    contin… AGE           sd        SD          7.89   SD   
#>  9 Xano… >64            catego… AGEGR1        p         %           0.905  >64  
#> 10 Xano… 18-64          catego… AGEGR1        p         %           0.0952 18-64
#> # ℹ 11 more rows
```

Note that if we take a peek at our stats for the treatment groups, we
have `n`, `N`, `p` from the
[`ard_stack()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack.html)
call:

``` r
tail(ard_demog_shuffled)
#> # A tibble: 6 × 8
#>   ARM    variable_level context stat_variable stat_name stat_label    stat label
#>   <chr>  <chr>          <chr>   <chr>         <chr>     <chr>        <dbl> <chr>
#> 1 Xanom… NA             tabula… ARM           n         n           84     NA   
#> 2 Xanom… NA             tabula… ARM           N         N          254     NA   
#> 3 Xanom… NA             tabula… ARM           p         %            0.331 NA   
#> 4 Xanom… NA             tabula… ARM           n         n           84     NA   
#> 5 Xanom… NA             tabula… ARM           N         N          254     NA   
#> 6 Xanom… NA             tabula… ARM           p         %            0.331 NA
```

Commonly we only want to display the `n` in the column headers for
treatment group. To distinguish these as big N’s, we need to relabel the
stat, and drop the unnecessary `N` and `p`. The
[`prep_big_n()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_big_n.md)
helper takes care of this for us:

``` r
ard_demog_shuffled <- ard_demog_shuffled |>
  prep_big_n(vars = "ARM") # the grouping variable we want to prepare big N's for

ard_demog_shuffled
#> # A tibble: 15 × 8
#>    ARM   variable_level context stat_variable stat_name stat_label    stat label
#>    <chr> <chr>          <chr>   <chr>         <chr>     <chr>        <dbl> <chr>
#>  1 Plac… >64            catego… AGEGR1        p         %           0.837  >64  
#>  2 Plac… 18-64          catego… AGEGR1        p         %           0.163  18-64
#>  3 Plac… Overall AGE    contin… AGE           mean      Mean       75.2    Mean 
#>  4 Plac… Overall AGE    contin… AGE           sd        SD          8.59   SD   
#>  5 Xano… >64            catego… AGEGR1        p         %           0.869  >64  
#>  6 Xano… 18-64          catego… AGEGR1        p         %           0.131  18-64
#>  7 Xano… Overall AGE    contin… AGE           mean      Mean       74.4    Mean 
#>  8 Xano… Overall AGE    contin… AGE           sd        SD          7.89   SD   
#>  9 Xano… >64            catego… AGEGR1        p         %           0.905  >64  
#> 10 Xano… 18-64          catego… AGEGR1        p         %           0.0952 18-64
#> 11 Xano… Overall AGE    contin… AGE           mean      Mean       75.7    Mean 
#> 12 Xano… Overall AGE    contin… AGE           sd        SD          8.29   SD   
#> 13 Plac… NA             tabula… ARM           bigN      n          86      NA   
#> 14 Xano… NA             tabula… ARM           bigN      n          84      NA   
#> 15 Xano… NA             tabula… ARM           bigN      n          84      NA
```

Finally, we can do any additional table-specific manipulations and we’re
ready for our tfrmt!

``` r
ard_final <- ard_demog_shuffled |>
  # drop unneeded variables
  dplyr::select(-c(variable_level, context, stat_label)) |>
  # add sorting
  dplyr::mutate(ord1 = dplyr::case_when(
    label == "18-64" ~ 1,
    label == ">64" ~ 2,
    TRUE ~ 3
  ))

ard_final
#> # A tibble: 15 × 6
#>    ARM                  stat_variable stat_name    stat label  ord1
#>    <chr>                <chr>         <chr>       <dbl> <chr> <dbl>
#>  1 Placebo              AGEGR1        p          0.837  >64       2
#>  2 Placebo              AGEGR1        p          0.163  18-64     1
#>  3 Placebo              AGE           mean      75.2    Mean      3
#>  4 Placebo              AGE           sd         8.59   SD        3
#>  5 Xanomeline High Dose AGEGR1        p          0.869  >64       2
#>  6 Xanomeline High Dose AGEGR1        p          0.131  18-64     1
#>  7 Xanomeline High Dose AGE           mean      74.4    Mean      3
#>  8 Xanomeline High Dose AGE           sd         7.89   SD        3
#>  9 Xanomeline Low Dose  AGEGR1        p          0.905  >64       2
#> 10 Xanomeline Low Dose  AGEGR1        p          0.0952 18-64     1
#> 11 Xanomeline Low Dose  AGE           mean      75.7    Mean      3
#> 12 Xanomeline Low Dose  AGE           sd         8.29   SD        3
#> 13 Placebo              ARM           bigN      86      NA        3
#> 14 Xanomeline High Dose ARM           bigN      84      NA        3
#> 15 Xanomeline Low Dose  ARM           bigN      84      NA        3
```

Create the {tfrmt}

``` r
tfrmt(
  group = stat_variable,
  label = label,
  param = stat_name,
  value = stat,
  column = ARM,
  sorting_cols = ord1,
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt("xx.x")
    ),
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      p = frmt("xx%", transform = ~ . * 100)
    )
  ),
  big_n = big_n_structure(param_val = "bigN"),
  col_plan = col_plan(-ord1)
) |>
  print_to_gt(ard_final)
```

[TABLE]

## AE table

AE tables contain hierarchical summaries. We can perform these summaries
using the following {cards} code:

``` r
adae <- pharmaverseadam::adae |>
  dplyr::filter(SAFFL == "Y") |>
  dplyr::filter(AESOC %in% unique(AESOC)[1:2]) |>
  dplyr::group_by(AESOC) |>
  dplyr::filter(AEDECOD %in% unique(AEDECOD)[1:3]) |>
  dplyr::ungroup()

ard_ae <- cards::ard_stack_hierarchical(
  data = adae,
  variables = c(AESOC, AEDECOD),
  by = ARM,
  denominator = pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y"),
  id = USUBJID,
  statistic = ~ c("n", "p")
)

ard_ae
#> {cards} data frame: 57 x 13
#>    group1 group1_level group2 group2_level variable variable_level stat_name
#> 1    <NA>                <NA>                   ARM        Placebo         n
#> 2    <NA>                <NA>                   ARM        Placebo         N
#> 3    <NA>                <NA>                   ARM        Placebo         p
#> 4    <NA>                <NA>                   ARM      Xanomeli…         n
#> 5    <NA>                <NA>                   ARM      Xanomeli…         N
#> 6    <NA>                <NA>                   ARM      Xanomeli…         p
#> 7    <NA>                <NA>                   ARM      Xanomeli…         n
#> 8    <NA>                <NA>                   ARM      Xanomeli…         N
#> 9    <NA>                <NA>                   ARM      Xanomeli…         p
#> 10    ARM      Placebo   <NA>                 AESOC      GASTROIN…         n
#>    stat_label  stat
#> 1           n    86
#> 2           N   254
#> 3           % 0.339
#> 4           n    84
#> 5           N   254
#> 6           % 0.331
#> 7           n    84
#> 8           N   254
#> 9           % 0.331
#> 10          n    12
#> ℹ 47 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, fmt_fun, warning, error
```

Like the demog table example, the first step in preparing the data for
{tfrmt} is calling
[`shuffle_card()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/shuffle_card.md)
which performs general operations to get the results in a tidy data
frame without list-columns and extra metadata.

``` r
ard_ae_shuffled <- ard_ae |>
  shuffle_card()

ard_ae_shuffled
#> # A tibble: 57 × 8
#>    ARM          AESOC AEDECOD context stat_variable stat_name stat_label    stat
#>    <chr>        <chr> <chr>   <chr>   <chr>         <chr>     <chr>        <dbl>
#>  1 Placebo      NA    NA      tabula… ARM           n         n           86    
#>  2 Placebo      NA    NA      tabula… ARM           N         N          254    
#>  3 Placebo      NA    NA      tabula… ARM           p         %            0.339
#>  4 Xanomeline … NA    NA      tabula… ARM           n         n           84    
#>  5 Xanomeline … NA    NA      tabula… ARM           N         N          254    
#>  6 Xanomeline … NA    NA      tabula… ARM           p         %            0.331
#>  7 Xanomeline … NA    NA      tabula… ARM           n         n           84    
#>  8 Xanomeline … NA    NA      tabula… ARM           N         N          254    
#>  9 Xanomeline … NA    NA      tabula… ARM           p         %            0.331
#> 10 Placebo      GAST… NA      hierar… AESOC         n         n           12    
#> # ℹ 47 more rows
```

Notice that our calculations of any AE by system organ class (AESOC)
have a missing value for preferred term (AEDECOD):

``` r
ard_ae_shuffled |>
  dplyr::filter(!is.na(AESOC) & is.na(AEDECOD))
#> # A tibble: 12 × 8
#>    ARM          AESOC AEDECOD context stat_variable stat_name stat_label    stat
#>    <chr>        <chr> <chr>   <chr>   <chr>         <chr>     <chr>        <dbl>
#>  1 Placebo      GAST… NA      hierar… AESOC         n         n          12     
#>  2 Placebo      GAST… NA      hierar… AESOC         p         %           0.140 
#>  3 Placebo      GENE… NA      hierar… AESOC         n         n           9     
#>  4 Placebo      GENE… NA      hierar… AESOC         p         %           0.105 
#>  5 Xanomeline … GAST… NA      hierar… AESOC         n         n          10     
#>  6 Xanomeline … GAST… NA      hierar… AESOC         p         %           0.119 
#>  7 Xanomeline … GENE… NA      hierar… AESOC         n         n          28     
#>  8 Xanomeline … GENE… NA      hierar… AESOC         p         %           0.333 
#>  9 Xanomeline … GAST… NA      hierar… AESOC         n         n           8     
#> 10 Xanomeline … GAST… NA      hierar… AESOC         p         %           0.0952
#> 11 Xanomeline … GENE… NA      hierar… AESOC         n         n          27     
#> 12 Xanomeline … GENE… NA      hierar… AESOC         p         %           0.321
```

We need to populate these missing values for our table. In order to
display these as group summaries, the value for AEDECOD must match the
value for AESOC. We can use the
[`prep_hierarchical_fill()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_hierarchical_fill.md)
helper to fill the value from AESOC into AEDECOD:

``` r
ard_ae_shuffled <- ard_ae_shuffled |>
  prep_hierarchical_fill(
    vars = c("AESOC", "AEDECOD"),
    fill_from_left = TRUE
  )

ard_ae_shuffled |>
  dplyr::filter(AESOC == AEDECOD)
#> # A tibble: 12 × 8
#>    ARM          AESOC AEDECOD context stat_variable stat_name stat_label    stat
#>    <chr>        <chr> <chr>   <chr>   <chr>         <chr>     <chr>        <dbl>
#>  1 Placebo      GAST… GASTRO… hierar… AESOC         n         n          12     
#>  2 Placebo      GAST… GASTRO… hierar… AESOC         p         %           0.140 
#>  3 Placebo      GENE… GENERA… hierar… AESOC         n         n           9     
#>  4 Placebo      GENE… GENERA… hierar… AESOC         p         %           0.105 
#>  5 Xanomeline … GAST… GASTRO… hierar… AESOC         n         n          10     
#>  6 Xanomeline … GAST… GASTRO… hierar… AESOC         p         %           0.119 
#>  7 Xanomeline … GENE… GENERA… hierar… AESOC         n         n          28     
#>  8 Xanomeline … GENE… GENERA… hierar… AESOC         p         %           0.333 
#>  9 Xanomeline … GAST… GASTRO… hierar… AESOC         n         n           8     
#> 10 Xanomeline … GAST… GASTRO… hierar… AESOC         p         %           0.0952
#> 11 Xanomeline … GENE… GENERA… hierar… AESOC         n         n          27     
#> 12 Xanomeline … GENE… GENERA… hierar… AESOC         p         %           0.321
```

Note that if we take a peek at our stats for the treatment groups, we
have `n`, `N`, `p` from the
[`ard_stack()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack.html)
call:

``` r
ard_ae_shuffled |>
  dplyr::filter(is.na(AESOC))
#> # A tibble: 9 × 8
#>   ARM           AESOC AEDECOD context stat_variable stat_name stat_label    stat
#>   <chr>         <chr> <chr>   <chr>   <chr>         <chr>     <chr>        <dbl>
#> 1 Placebo       NA    NA      tabula… ARM           n         n           86    
#> 2 Placebo       NA    NA      tabula… ARM           N         N          254    
#> 3 Placebo       NA    NA      tabula… ARM           p         %            0.339
#> 4 Xanomeline H… NA    NA      tabula… ARM           n         n           84    
#> 5 Xanomeline H… NA    NA      tabula… ARM           N         N          254    
#> 6 Xanomeline H… NA    NA      tabula… ARM           p         %            0.331
#> 7 Xanomeline L… NA    NA      tabula… ARM           n         n           84    
#> 8 Xanomeline L… NA    NA      tabula… ARM           N         N          254    
#> 9 Xanomeline L… NA    NA      tabula… ARM           p         %            0.331
```

Commonly we only want to display the `n` in the column headers for
treatment group. To distinguish these as big N’s, we need to relabel the
stat, and drop the unnecessary `N` and `p`. The
[`prep_big_n()`](https://gsk-biostatistics.github.io/tfrmt/dev/reference/prep_big_n.md)
helper takes care of this for us:

``` r
ard_ae_shuffled <- ard_ae_shuffled |>
  prep_big_n(vars = "ARM") # the grouping variable we want to prepare big N's for

ard_ae_shuffled
#> # A tibble: 51 × 8
#>    ARM           AESOC AEDECOD context stat_variable stat_name stat_label   stat
#>    <chr>         <chr> <chr>   <chr>   <chr>         <chr>     <chr>       <dbl>
#>  1 Placebo       NA    NA      tabula… ARM           bigN      n          86    
#>  2 Xanomeline H… NA    NA      tabula… ARM           bigN      n          84    
#>  3 Xanomeline L… NA    NA      tabula… ARM           bigN      n          84    
#>  4 Placebo       GAST… GASTRO… hierar… AESOC         n         n          12    
#>  5 Placebo       GAST… GASTRO… hierar… AESOC         p         %           0.140
#>  6 Placebo       GENE… GENERA… hierar… AESOC         n         n           9    
#>  7 Placebo       GENE… GENERA… hierar… AESOC         p         %           0.105
#>  8 Placebo       GAST… DIARRH… hierar… AEDECOD       n         n           9    
#>  9 Placebo       GAST… DIARRH… hierar… AEDECOD       p         %           0.105
#> 10 Placebo       GAST… HIATUS… hierar… AEDECOD       n         n           1    
#> # ℹ 41 more rows
```

Finally, we can do any additional table-specific manipulations and we’re
ready for our tfrmt!

``` r
ard_final <- ard_ae_shuffled |>
  # drop uneeded variables
  dplyr::select(-c(context, stat_label, stat_variable))

ard_final
#> # A tibble: 51 × 5
#>    ARM                  AESOC                           AEDECOD stat_name   stat
#>    <chr>                <chr>                           <chr>   <chr>      <dbl>
#>  1 Placebo              NA                              NA      bigN      86    
#>  2 Xanomeline High Dose NA                              NA      bigN      84    
#>  3 Xanomeline Low Dose  NA                              NA      bigN      84    
#>  4 Placebo              GASTROINTESTINAL DISORDERS      GASTRO… n         12    
#>  5 Placebo              GASTROINTESTINAL DISORDERS      GASTRO… p          0.140
#>  6 Placebo              GENERAL DISORDERS AND ADMINIST… GENERA… n          9    
#>  7 Placebo              GENERAL DISORDERS AND ADMINIST… GENERA… p          0.105
#>  8 Placebo              GASTROINTESTINAL DISORDERS      DIARRH… n          9    
#>  9 Placebo              GASTROINTESTINAL DISORDERS      DIARRH… p          0.105
#> 10 Placebo              GASTROINTESTINAL DISORDERS      HIATUS… n          1    
#> # ℹ 41 more rows
```

Create the {tfrmt}

``` r
tfrmt(
  group = AESOC,
  label = AEDECOD,
  param = stat_name,
  value = stat,
  column = ARM,
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine(
        "{n} ({p}%)",
        n = frmt("xx"),
        p = frmt("xx", transform = ~ . * 100)
      )
    )
  ),
  big_n = big_n_structure(
    param_val = "bigN"
  )
) |>
  print_to_gt(ard_final)
```

|                                                      | Placebo N = 86 | Xanomeline High Dose N = 84 | Xanomeline Low Dose N = 84 |
|------------------------------------------------------|:--------------:|:---------------------------:|:--------------------------:|
| GASTROINTESTINAL DISORDERS                           |    12 (14%)    |          10 (12%)           |           8 (10%)          |
|   DIARRHOEA                                          |     9 (10%)    |           4 ( 5%)           |           5 ( 6%)          |
|   HIATUS HERNIA                                      |     1 ( 1%)    |           0 ( 0%)           |           0 ( 0%)          |
|   VOMITING                                           |     3 ( 3%)    |           7 ( 8%)           |           3 ( 4%)          |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS |     9 (10%)    |          28 (33%)           |          27 (32%)          |
|   APPLICATION SITE ERYTHEMA                          |     3 ( 3%)    |          15 (18%)           |          12 (14%)          |
|   APPLICATION SITE PRURITUS                          |     6 ( 7%)    |          22 (26%)           |          22 (26%)          |
|   FATIGUE                                            |     1 ( 1%)    |           5 ( 6%)           |           5 ( 6%)          |
