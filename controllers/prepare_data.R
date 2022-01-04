library(dplyr)
library(tidyverse)

prepare_app_test <- function(application_train){
  cash_loan <- application_train[application_train$NAME_CONTRACT_TYPE=="Cash loans",]
  cash_loan <- cash_loan%>%
    select(-c("FLAG_DOCUMENT_2","FLAG_DOCUMENT_3","FLAG_DOCUMENT_4","FLAG_DOCUMENT_5","FLAG_DOCUMENT_6","FLAG_DOCUMENT_7",
              "FLAG_DOCUMENT_8","FLAG_DOCUMENT_9","FLAG_DOCUMENT_10","FLAG_DOCUMENT_11","FLAG_DOCUMENT_12","FLAG_DOCUMENT_13",            
              "FLAG_DOCUMENT_14","FLAG_DOCUMENT_15","FLAG_DOCUMENT_16","FLAG_DOCUMENT_17","FLAG_DOCUMENT_18","FLAG_DOCUMENT_19",            
              "FLAG_DOCUMENT_20","FLAG_DOCUMENT_21","EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3"))
  
  cash_loan <- cash_loan %>% select(-c(AMT_REQ_CREDIT_BUREAU_DAY,AMT_REQ_CREDIT_BUREAU_HOUR,AMT_REQ_CREDIT_BUREAU_MON,
                                       AMT_REQ_CREDIT_BUREAU_QRT,AMT_REQ_CREDIT_BUREAU_YEAR))
  character_variables <- names(cash_loan %>% select_if(is.character))
  cash_loan <- cash_loan %>%
    mutate_at(vars(character_variables), as.factor)%>%
    select(-NAME_CONTRACT_TYPE)
  
  return(cash_loan)
}

