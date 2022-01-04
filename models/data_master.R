library(readr)
library(dplyr)
library(tidyverse)
data_master <- read_csv("cash_loanWbureau.csv")
character_variables <- names(data_master %>% select_if(is.character))
data_master <- data_master %>%
  mutate_at(vars(character_variables), as.factor)

bureau_variables <- c("DAYS_CREDIT","CREDIT_DAY_OVERDUE","DAYS_CREDIT_ENDDATE","CNT_CREDIT_PROLONG",
                      "AMT_CREDIT_SUM","AMT_CREDIT_SUM_DEBT","AMT_CREDIT_SUM_OVERDUE", "DAYS_CREDIT_UPDATE")
data_master <- data_master%>%
  mutate_at(vars(bureau_variables), ~replace_na(.,0))%>%
  mutate_at(vars(BUCKET), as.character)%>%
  mutate(BUCKET = replace_na(BUCKET,'N/A'))


#-------------------------- selected variables from elastic net. 
vars_selected <- c("SK_ID_CURR","TARGET","AMT_INCOME_TOTAL","AMT_CREDIT","AMT_ANNUITY","BUCKET", "AMT_CREDIT_SUM_OVERDUE","AMT_CREDIT_SUM","DAYS_CREDIT_ENDDATE","DAYS_CREDIT",
                   "DAYS_LAST_PHONE_CHANGE","DEF_60_CNT_SOCIAL_CIRCLE","DEF_30_CNT_SOCIAL_CIRCLE","REG_CITY_NOT_WORK_CITY",
                   "REG_CITY_NOT_LIVE_CITY","HOUR_APPR_PROCESS_START","REGION_RATING_CLIENT","REGION_RATING_CLIENT_W_CITY",
                   "FLAG_PHONE","FLAG_WORK_PHONE","FLAG_EMP_PHONE","DAYS_ID_PUBLISH","DAYS_BIRTH","DAYS_EMPLOYED","DAYS_REGISTRATION",
                   "NAME_HOUSING_TYPE","NAME_EDUCATION_TYPE","NAME_INCOME_TYPE","NAME_TYPE_SUITE","AMT_GOODS_PRICE",
                   "FLAG_OWN_CAR","CODE_GENDER")
data_master <- data_master %>% select(vars_selected)
data_master$STATUS <- ifelse(data_master$TARGET == 0, 'Accept','Reject')

#------------------- Dates -------------------
start_date <- as.Date('2016-11-03')
end_date <- as.Date('2022-01-04')
all_dates <- seq(from = start_date, to = end_date, by =1)
set.seed(10)
list_dates <- sample(all_dates, size = nrow(data_master), replace = T)
data_master$APPLICATION_DATE <- list_dates
save(data_master, file = 'rdata/data_master.RData')
