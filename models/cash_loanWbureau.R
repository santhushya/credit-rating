library(dlookr)
#---------------------------- Refresh button - extract data

#source('controllers/extract_data.R')
application_test <- read_csv("data/application_test.csv")
app_test <- application_test[1:521,]
app_test$APPLICATION_DATE <- '2022-01-02'

#----------------------------- prepare data
source('controllers/prepare_data.R')
cash_loan <- prepare_app_test(app_test)
load('rdata/bureau_data.RData')

#---------------------- Full Dataset with transaction and bureau details
bureau_data1$SK_ID_CURR <- as.numeric(bureau_data1$SK_ID_CURR)
cash_loanWbureau <- cash_loan %>%
  left_join(bureau_data1, by = "SK_ID_CURR") #%>%
  #mutate(BUCKET = max(STATUS, na.rm = T))%>%
  
character_variables <- names(cash_loanWbureau %>% select_if(is.character))
cash_loanWbureau <- cash_loanWbureau %>%
  mutate_at(vars(character_variables), as.factor)

bureau_variables <- c("DAYS_CREDIT","CREDIT_DAY_OVERDUE","DAYS_CREDIT_ENDDATE","CNT_CREDIT_PROLONG",
                      "AMT_CREDIT_SUM","AMT_CREDIT_SUM_DEBT","AMT_CREDIT_SUM_OVERDUE", "DAYS_CREDIT_UPDATE")
cash_loanWbureau <- cash_loanWbureau%>%
  mutate_at(vars(bureau_variables), ~replace_na(.,0))%>%
  mutate_at(vars(BUCKET), as.character)%>%
  mutate(BUCKET = replace_na(BUCKET,'N/A'))

full_application <- cash_loanWbureau
save(full_application, file = 'rdata/full_application.RData')
#-------------------------- selected variables from elastic net. 
vars_selected <- c("APPLICATION_DATE","SK_ID_CURR","AMT_INCOME_TOTAL","AMT_CREDIT","AMT_ANNUITY","BUCKET", "AMT_CREDIT_SUM_OVERDUE","AMT_CREDIT_SUM","DAYS_CREDIT_ENDDATE","DAYS_CREDIT",
                   "DAYS_LAST_PHONE_CHANGE","DEF_60_CNT_SOCIAL_CIRCLE","DEF_30_CNT_SOCIAL_CIRCLE","REG_CITY_NOT_WORK_CITY",
                   "REG_CITY_NOT_LIVE_CITY","HOUR_APPR_PROCESS_START","REGION_RATING_CLIENT","REGION_RATING_CLIENT_W_CITY",
                   "FLAG_PHONE","FLAG_WORK_PHONE","FLAG_EMP_PHONE","DAYS_ID_PUBLISH","DAYS_BIRTH","DAYS_EMPLOYED","DAYS_REGISTRATION",
                   "NAME_HOUSING_TYPE","NAME_EDUCATION_TYPE","NAME_INCOME_TYPE","NAME_TYPE_SUITE","AMT_GOODS_PRICE",
                   "FLAG_OWN_CAR","CODE_GENDER")
cash_loanWbureau <- cash_loanWbureau %>% select(vars_selected)

save(cash_loanWbureau, file = 'rdata/cash_loanWbureau.RData')

