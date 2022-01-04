
get_segment_table <- function(data_master, application){
  
factor_vars <- names(data_master %>% select_if(is.factor))
data_master <- data_master %>%
  mutate_at(vars(factor_vars), as.character)

cust_segment <- data_master[data_master$TARGET == 0 &
  data_master$NAME_HOUSING_TYPE%in%c(as.character(application$NAME_HOUSING_TYPE)) &
                              data_master$NAME_EDUCATION_TYPE%in% c(as.character(application$NAME_EDUCATION_TYPE)) &
                              data_master$NAME_INCOME_TYPE%in% c(as.character(application$NAME_INCOME_TYPE)) &
                              data_master$NAME_TYPE_SUITE%in% c(as.character(application$NAME_TYPE_SUITE)) &
                              data_master$FLAG_OWN_CAR%in% c(as.character(application$FLAG_OWN_CAR)) &
                              data_master$CODE_GENDER%in% c(as.character(application$CODE_GENDER)) ,]

numeric_vars <- names(data_master %>% select_if(is.numeric))
compare_vars <- c("AMT_INCOME_TOTAL","AMT_CREDIT","AMT_ANNUITY","AMT_CREDIT_SUM_OVERDUE","AMT_CREDIT_SUM",
                  "DAYS_EMPLOYED")
segment_table <- cust_segment%>%
  select(compare_vars)%>%
  summarise_all(mean, na.rm = T)

segment_table <- data.frame('Mean_Value'=t(segment_table))
segment_table$variable <- rownames(segment_table)
rownames(segment_table) <-NULL

cust <- data.frame('App_Value'=t(application))
cust$variable <- rownames(cust)
rownames(cust) <-NULL

segment_table <- segment_table %>%
  left_join(cust, by = "variable")%>%
  select(variable , App_Value, Mean_Value)

return(segment_table)
}
  
  