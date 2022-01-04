library(dplyr)
library(brglm2)
load('rdata/prediction_model.RData')
load('rdata/cash_loanWbureau.RData')

vars_selected <- c("BUCKET", "AMT_CREDIT_SUM_OVERDUE","AMT_CREDIT_SUM","DAYS_CREDIT_ENDDATE","DAYS_CREDIT",
                   "DAYS_LAST_PHONE_CHANGE","DEF_60_CNT_SOCIAL_CIRCLE","DEF_30_CNT_SOCIAL_CIRCLE","REG_CITY_NOT_WORK_CITY",
                   "REG_CITY_NOT_LIVE_CITY","HOUR_APPR_PROCESS_START","REGION_RATING_CLIENT","REGION_RATING_CLIENT_W_CITY",
                   "FLAG_PHONE","FLAG_WORK_PHONE","FLAG_EMP_PHONE","DAYS_ID_PUBLISH","DAYS_BIRTH","DAYS_EMPLOYED","DAYS_REGISTRATION",
                   "NAME_HOUSING_TYPE","NAME_EDUCATION_TYPE","NAME_INCOME_TYPE","NAME_TYPE_SUITE","AMT_GOODS_PRICE",
                   "FLAG_OWN_CAR","CODE_GENDER")

brg.predict <- predict.glm(brglm_model, newdata = cash_loanWbureau%>% select(vars_selected), type = "response")

cash_loanWbureau$PD <- brg.predict
save(cash_loanWbureau, file = 'rdata/cash_loanWbureau.RData')
