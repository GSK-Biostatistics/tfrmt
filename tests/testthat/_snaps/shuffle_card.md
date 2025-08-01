# shuffle/trim works

    Code
      ard_simple_shuffled
    Output
                          ARM         AGE    context stat_variable stat_name
      1               Placebo Overall AGE continuous           AGE         N
      2               Placebo Overall AGE continuous           AGE      mean
      3               Placebo Overall AGE continuous           AGE        sd
      4               Placebo Overall AGE continuous           AGE    median
      5               Placebo Overall AGE continuous           AGE       p25
      6               Placebo Overall AGE continuous           AGE       p75
      7               Placebo Overall AGE continuous           AGE       min
      8               Placebo Overall AGE continuous           AGE       max
      9  Xanomeline High Dose Overall AGE continuous           AGE         N
      10 Xanomeline High Dose Overall AGE continuous           AGE      mean
      11 Xanomeline High Dose Overall AGE continuous           AGE        sd
      12 Xanomeline High Dose Overall AGE continuous           AGE    median
      13 Xanomeline High Dose Overall AGE continuous           AGE       p25
      14 Xanomeline High Dose Overall AGE continuous           AGE       p75
      15 Xanomeline High Dose Overall AGE continuous           AGE       min
      16 Xanomeline High Dose Overall AGE continuous           AGE       max
      17  Xanomeline Low Dose Overall AGE continuous           AGE         N
      18  Xanomeline Low Dose Overall AGE continuous           AGE      mean
      19  Xanomeline Low Dose Overall AGE continuous           AGE        sd
      20  Xanomeline Low Dose Overall AGE continuous           AGE    median
      21  Xanomeline Low Dose Overall AGE continuous           AGE       p25
      22  Xanomeline Low Dose Overall AGE continuous           AGE       p75
      23  Xanomeline Low Dose Overall AGE continuous           AGE       min
      24  Xanomeline Low Dose Overall AGE continuous           AGE       max
         stat_label      stat fmt_fun warning error
      1           N 86.000000       0    NULL  NULL
      2        Mean 75.209302       1    NULL  NULL
      3          SD  8.590167       1    NULL  NULL
      4      Median 76.000000       1    NULL  NULL
      5          Q1 69.000000       1    NULL  NULL
      6          Q3 82.000000       1    NULL  NULL
      7         Min 52.000000       1    NULL  NULL
      8         Max 89.000000       1    NULL  NULL
      9           N 84.000000       0    NULL  NULL
      10       Mean 74.380952       1    NULL  NULL
      11         SD  7.886094       1    NULL  NULL
      12     Median 76.000000       1    NULL  NULL
      13         Q1 70.500000       1    NULL  NULL
      14         Q3 80.000000       1    NULL  NULL
      15        Min 56.000000       1    NULL  NULL
      16        Max 88.000000       1    NULL  NULL
      17          N 84.000000       0    NULL  NULL
      18       Mean 75.666667       1    NULL  NULL
      19         SD  8.286051       1    NULL  NULL
      20     Median 77.500000       1    NULL  NULL
      21         Q1 71.000000       1    NULL  NULL
      22         Q3 82.000000       1    NULL  NULL
      23        Min 51.000000       1    NULL  NULL
      24        Max 88.000000       1    NULL  NULL

---

    Code
      ard_shuffled[1:5, ]
    Output
                         ARM  AGE AGEGR1     context stat_variable stat_name
      1              Placebo <NA>   <NA> categorical           ARM         n
      2              Placebo <NA>   <NA> categorical           ARM         N
      3              Placebo <NA>   <NA> categorical           ARM         p
      4 Xanomeline High Dose <NA>   <NA> categorical           ARM         n
      5 Xanomeline High Dose <NA>   <NA> categorical           ARM         N
        stat_label        stat
      1          n  86.0000000
      2          N 254.0000000
      3          %   0.3385827
      4          n  84.0000000
      5          N 254.0000000

---

    Code
      ard_shuff_trim[1:5, ]
    Output
                         ARM  AGE AGEGR1     context stat_variable stat_name
      1              Placebo <NA>   <NA> categorical           ARM         n
      2              Placebo <NA>   <NA> categorical           ARM         N
      3              Placebo <NA>   <NA> categorical           ARM         p
      4 Xanomeline High Dose <NA>   <NA> categorical           ARM         n
      5 Xanomeline High Dose <NA>   <NA> categorical           ARM         N
        stat_label        stat
      1          n  86.0000000
      2          N 254.0000000
      3          %   0.3385827
      4          n  84.0000000
      5          N 254.0000000

# shuffle_card notifies user about warnings/errors before dropping

    Code
      shuffle_card(cards::ard_continuous(cards::ADSL, variables = AGEGR1))
    Message
      "warning" column contains messages that will be removed.
    Output
      # A tibble: 8 x 6
        AGEGR1         context    stat_variable stat_name stat_label stat 
        <chr>          <chr>      <chr>         <chr>     <chr>      <chr>
      1 Overall AGEGR1 continuous AGEGR1        N         N          254  
      2 Overall AGEGR1 continuous AGEGR1        mean      Mean       <NA> 
      3 Overall AGEGR1 continuous AGEGR1        sd        SD         <NA> 
      4 Overall AGEGR1 continuous AGEGR1        median    Median     <NA> 
      5 Overall AGEGR1 continuous AGEGR1        p25       Q1         65-80
      6 Overall AGEGR1 continuous AGEGR1        p75       Q3         >80  
      7 Overall AGEGR1 continuous AGEGR1        min       Min        65-80
      8 Overall AGEGR1 continuous AGEGR1        max       Max        >80  

# shuffle_card fills missing group levels if the group is meaningful

    Code
      shuffle_card(dplyr::filter(cards::bind_ard(cards::ard_continuous(cards::ADSL,
      by = "ARM", variables = "AGE", statistic = ~ cards::continuous_summary_fns(
        "mean")), dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p",
        stat_label = "p", stat = list(0.05))), dplyr::row_number() <= 5L))
    Output
      # A tibble: 4 x 7
        ARM                  AGE      context stat_variable stat_name stat_label  stat
        <chr>                <chr>    <chr>   <chr>         <chr>     <chr>      <dbl>
      1 Placebo              Overall~ contin~ AGE           mean      Mean       75.2 
      2 Xanomeline High Dose Overall~ contin~ AGE           mean      Mean       74.4 
      3 Xanomeline Low Dose  Overall~ contin~ AGE           mean      Mean       75.7 
      4 Overall ARM          Overall~ <NA>    AGE           p         p           0.05

---

    Code
      shuffle_card(dplyr::filter(cards::bind_ard(cards::ard_continuous(cards::ADSL,
      variables = "AGE", statistic = ~ cards::continuous_summary_fns("mean")), dplyr::tibble(
        group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", stat = list(
          0.05))), dplyr::row_number() <= 5L))
    Output
      # A tibble: 2 x 7
        ARM         AGE         context    stat_variable stat_name stat_label  stat
        <chr>       <chr>       <chr>      <chr>         <chr>     <chr>      <dbl>
      1 <NA>        Overall AGE continuous AGE           mean      Mean       75.1 
      2 Overall ARM Overall AGE <NA>       AGE           p         p           0.05

---

    Code
      as.data.frame(shuffle_card(cards::bind_ard(dplyr::slice(cards::ard_categorical(
        cards::ADSL, by = ARM, variables = AGEGR1), 1), dplyr::slice(cards::ard_categorical(
        cards::ADSL, variables = AGEGR1), 1), dplyr::slice(cards::ard_continuous(
        cards::ADSL, by = SEX, variables = AGE), 1), dplyr::slice(cards::ard_continuous(
        cards::ADSL, variables = AGE), 1)), by = c("ARM", "SEX")))
    Output
                ARM         SEX AGEGR1         AGE     context stat_variable
      1     Placebo        <NA>  65-80        <NA> categorical        AGEGR1
      2 Overall ARM        <NA>  65-80        <NA> categorical        AGEGR1
      3        <NA> Overall SEX   <NA> Overall AGE  continuous           AGE
      4        <NA>           F   <NA> Overall AGE  continuous           AGE
        stat_name stat_label stat
      1         n          n   42
      2         n          n  144
      3         N          N  254
      4         N          N  143

---

    Code
      shuffle_card(cards::bind_ard(dplyr::slice(cards::ard_categorical(cards::ADSL,
      by = c(ARM, SEX), variables = AGEGR1), 1), dplyr::slice(cards::ard_categorical(
        cards::ADSL, by = SEX, variables = AGEGR1), 1), dplyr::slice(cards::ard_categorical(
        cards::ADSL, variables = AGEGR1), 1)), by = c("ARM", "SEX"))
    Output
      # A tibble: 3 x 8
        ARM         SEX        AGEGR1 context stat_variable stat_name stat_label  stat
        <chr>       <chr>      <chr>  <chr>   <chr>         <chr>     <chr>      <int>
      1 Placebo     F          65-80  catego~ AGEGR1        n         n             22
      2 Overall ARM F          65-80  catego~ AGEGR1        n         n             78
      3 Overall ARM Overall S~ 65-80  catego~ AGEGR1        n         n            144

---

    Code
      shuffle_card(cards::bind_ard(cards::ard_continuous(adsl_new, variables = "AGE",
        statistic = ~ cards::continuous_summary_fns("mean")), cards::ard_continuous(
        adsl_new, by = "ARM", variables = "AGE", statistic = ~ cards::continuous_summary_fns(
          "mean"))), by = "ARM")
    Message
      i "Overall ARM" already exists in the `ARM` column. Using "Overall ARM.1".
    Output
      # A tibble: 4 x 7
        ARM                  AGE      context stat_variable stat_name stat_label  stat
        <chr>                <chr>    <chr>   <chr>         <chr>     <chr>      <dbl>
      1 Overall ARM.1        Overall~ contin~ AGE           mean      Mean        75.1
      2 Overall ARM          Overall~ contin~ AGE           mean      Mean        75.2
      3 Xanomeline High Dose Overall~ contin~ AGE           mean      Mean        74.4
      4 Xanomeline Low Dose  Overall~ contin~ AGE           mean      Mean        75.7

# shuffle_card fills missing group levels if the group is meaningful for cardx output

    Code
      as.data.frame(shuffle_card(ard_cardx))
    Output
                ARM         SEX         AGEGR1          context stat_variable
      1 Overall ARM        <NA> Overall AGEGR1 stats_chisq_test        AGEGR1
      2 Overall ARM        <NA> Overall AGEGR1 stats_chisq_test        AGEGR1
      3        <NA> Overall SEX Overall AGEGR1 stats_chisq_test        AGEGR1
      4        <NA> Overall SEX Overall AGEGR1 stats_chisq_test        AGEGR1
        stat_name          stat_label       stat
      1 statistic X-squared Statistic 5.07944167
      2   p.value             p-value 0.07888842
      3 statistic X-squared Statistic 1.03944200
      4   p.value             p-value 0.59468644

# shuffle_card() messages about 'Overall <var>' or 'Any <var>'

    Code
      dplyr::mutate(test_data, dplyr::across(ARM:TRTA, .derive_overall_labels))
    Message
      i "Overall ARM" already exists in the `ARM` column. Using "Overall ARM.1".
    Output
      # A tibble: 5 x 2
        ARM           TRTA    
        <chr>         <chr>   
      1 Overall ARM.1 <NA>    
      2 Overall ARM   <NA>    
      3 <NA>          Any TRTA
      4 BB            C       
      5 <NA>          C       

---

    Code
      shuffled_ard <- shuffle_card(ard)
    Message
      i "Overall TRTA" already exists in the `TRTA` column. Using "Overall TRTA.1".
      i "Any AESOC" already exists in the `AESOC` column. Using"Any AESOC.1".

