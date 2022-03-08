# tlang


tfmt(
 .data, 
 title = "", 
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
 row_grp_stype = list("lab1" = element_block(), "lab2" = element_block(post_space = "---")), #the style between blocking 
 rounding = 1, 
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
 body_align = element_align(left = vars(), right = vars(), dec_pl = vars()), 
 #And now the things that do this!! 
 sorting_cols = vars(),
 page_vars = vars(), 
 row_grp = element_grp(cols, out_col?) #assume 
 col_labels = element_label(..., wrap_txt = 30),
 spanning_label_grp = list("span" = vars(), "sapn2" = vars()),
 col_select = "default" 
)

#Function to help make this a bit easier to deal with 
gsk_tfmt() %>%
pull_current_fmt("rouning_rowwise")
#Bucketlist 
print_preview(c("gt", "flextable")) 

dataset %>% 
gsk_tfmt() %>% 
sumstatrow_tfmt(base_round = 2) %>% 
tfmt(title = "Hello World") %>% 
print_to_gt() 
