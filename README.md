# tlang


tfmt(
 title = "", 
 group = vars(), label = vars(), param = vars(), values = vars(), column = vars(), 
 title_txt_style = element_text(), 
 subtitle = "", 
 subtitle_txt_style = element_text(), 
 col_label_txt_style = element_text(),
 spanning_label_txt_style = element_text(),
 footer = "", 
 footer_txt_style = element_text(), 
 txt_style =  element_text(),
 row_grp_txt_style =  element_text(), ?maybe pass a named list 
 row_txt_style =  element_text(),
 row_grp_style = list("lab1" = element_block(), "lab2" = element_block(post_space = "---")), #the style between blocking 
 rounding = 1, 
 body_style = element_style(
    fmt_str(group = ".default", label = ".default",  
    fmt_combine("{count} ({percent})%",
      count = fmt(),
      percent = fmt()
      )
    ),
    fmt_str(group = "Age", label = "Min., Max.", 
    fmt_combine("{min}, {min}", 
      min = fmt(),
      max = fmt()
      )
    ),
    fmt_str(group = c("Age", "Weight"), label = "n", fmt())
  ),
 body_align = element_align(left = vars(), right = vars(), dec_pl = vars()), 
 #And now the things that do this!! 
 sorting_cols = vars(sort_ord1, -TRTA, TRTB, TRTC),
 page_vars = vars(), 
 row_grp = element_grp(cols, out_col?) #assume 
 col_labels = element_label(..., wrap_txt = 30),
 spanning_label_grp = list("span" = vars(), "sapn2" = vars()),
 col_select = "default" 
)





body_style = element_style(rowwise = element_rowwise(ref_col, c(".default" = fmt_str(), 
 "n" = fmt_str("{COLNAME1} ({COLNAME2}%)",
          fmt(rounding = "XX",  bounds = element_bounds(upper_exp = ">5", lower_exp, upper_lab, lower_lab), padding = " "),
          fmt(...), cols = NULL, output = NULL),
 "mean" =  fmt_str(
     "{(.*)_mean} ({(.*)_std}%)",
      fmt(rounding = "XX", padding = " ", bounds = element_bounds(upper_exp = ">5", lower_exp, upper_lab, lower_lab)),
      fmt(...),
     cols = vars(palcebo_mean:total_std),output = "(.*)"),  "CV%" = fmt_str("X.XX %") ect.)),
  colwise = element_colwise(fmt_str("{P_VAL}", p_val_fmt())),


# How to identify what I am talking about 
fmt_combo()

fmt_id(ref_col = vars(row_lab2), ref_val = "Mean", cols = vars(across(is.numeric)), grp = c("row_label1" = "weight"))
fmt_id(ref_col = vars(row_lab2), ref_val = "Std", cols = vars(across(is.numeric)), grp = c("row_label1" = "weight"))




#Function to help make this a bit easier to deal with 
gsk_tfmt() %>%
pull_current_fmt("footer")
#Bucketlist 
print_preview(tfmt, output_type = c("gt", "flextable"))
print_preview(c("gt", "flextable")) 

#Create Mocks
print_preview(tfmt, output_type = "gt")

#Layerable 
gsk_tfmt() %>% 
demog_tfmt() %>% 
tfmt(title = "Hello World") %>% 
print_to_gt(, .data)

#Flexible
print_to_gt(tfmt, .data)
print_to_flextable(tfmt, .data)
print_to_dt(tfmt, .data)

sumstatrow_tfmt <- function(og_tfmt, base_round, n_df){
thingy <- pull_current_fmt(og_fmt)
old_lbs <- thingy$col_labels
str_extract(old_labels, (<?=\\{).(?=}))



}

pt_profile <- function(usubjid, ADSL, plot_dt){
  gt <- adsl %>% 
  filter(USUBJID == usubjid) %>% 
  print_to_get(pp_tfmt, .)
  
  
  
}




gsk_tfmt <- tfm(title = "hello")
