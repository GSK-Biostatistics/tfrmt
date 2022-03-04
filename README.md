# tlang


tfmt(
 title = "", 
 title_txt_style = element_text(), 
 subtitle = "", 
 subtitle_txt_style = element_text(), 
 footer = "", 
 footer_txt_style = element_text(), 
 txt_style =  element_text(),
 row_grp_txt_style =  element_text(),
 row_txt_style =  element_text(),
 sort_row,  
 rounding = 1, 
 rounding_rowwise = element_rowwise(cols, ref_col, c("n" = fmt_str("X"), "mean" = fmt_str("X.XX"),  "CV%" = fmt_str("X.XX %") ect.)),
 rouning_colwise = element_colwise()
 mean
 
 
)

#Function to help make this a bit easier to deal with 
gsk_tfmt() %>%
pull_current_fmt("rouning_rowwise")


dataset %>% 
gsk_tfmt() %>% 
sumstatrow_tfmt(base_round = 2) %>% 
tfmt(title = "Hello World") %>% 
print_to_gt() 
