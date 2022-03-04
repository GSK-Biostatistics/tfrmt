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
 rounding = 1, 
 rounding_rowwise = element_rowwise(cols, ref_col, c("n" = fmt_str("X"), "mean" = fmt_str("X.XX"),  "CV%" = fmt_str("X.XX %") ect.)),
 rouning_colwise = element_colwise(),
 body_align = element_align(left = vars(), right = vars(), dec_pl = vars()), 
 #And now the things that do this!! 
 sorting_cols = vars(),  
 combine_data_col = elment_combo(new_col = "{COLNAME1} {COLNAME2}", new_col2 = "{COLNAME1} {COLNAME2}%"),
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
