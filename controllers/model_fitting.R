library(brglm2)

vars_selected <- c("BUCKET", "AMT_CREDIT_SUM_OVERDUE","AMT_CREDIT_SUM","DAYS_CREDIT_ENDDATE","DAYS_CREDIT",
                   "DAYS_LAST_PHONE_CHANGE","DEF_60_CNT_SOCIAL_CIRCLE","DEF_30_CNT_SOCIAL_CIRCLE","REG_CITY_NOT_WORK_CITY",
                   "REG_CITY_NOT_LIVE_CITY","HOUR_APPR_PROCESS_START","REGION_RATING_CLIENT","REGION_RATING_CLIENT_W_CITY",
                   "FLAG_PHONE","FLAG_WORK_PHONE","FLAG_EMP_PHONE","DAYS_ID_PUBLISH","DAYS_BIRTH","DAYS_EMPLOYED","DAYS_REGISTRATION",
                   "NAME_HOUSING_TYPE","NAME_EDUCATION_TYPE","NAME_INCOME_TYPE","NAME_TYPE_SUITE","AMT_GOODS_PRICE",
                   "FLAG_OWN_CAR","CODE_GENDER","TARGET")
categorical_vars <- c("BUCKET","REG_CITY_NOT_WORK_CITY","REG_CITY_NOT_LIVE_CITY","REGION_RATING_CLIENT",
                      "REGION_RATING_CLIENT_W_CITY","FLAG_PHONE","FLAG_WORK_PHONE","FLAG_EMP_PHONE",
                      "NAME_HOUSING_TYPE","NAME_EDUCATION_TYPE","NAME_INCOME_TYPE","NAME_TYPE_SUITE",
                      "FLAG_OWN_CAR","CODE_GENDER","TARGET")
cash_loanWbureau <- cash_loanWbureau %>%
  mutate_at(vars(categorical_vars), as.factor)
cash_loanWbureau$NAME_TYPE_SUITE <- recode_factor(cash_loanWbureau$NAME_TYPE_SUITE, `Spouse, partner`="Spouse/partner" )
# ---------------------- Data Splitting -----------------#
set.seed(103)
index <- sample(1:nrow(cash_loanWbureau), round(nrow(cash_loanWbureau)*0.75), replace = F)
train.set <- cash_loanWbureau[index,]%>%select(-ORGANIZATION_TYPE)%>% select(vars_selected)
test.set <- cash_loanWbureau[-index,]%>%select(-ORGANIZATION_TYPE)%>% select(vars_selected)
y.test <- as.factor(test.set$TARGET)

#------------------------ c5.0 --------------------------- #
library(C50)
c50_mod <- C5.0(TARGET ~ ., data = train.set)
summary(c50_mod)
c50_pred <- predict(c50_mod, newdata = test.set)
confusionMatrix(c50_pred, y.test)

#------------------------ c5.0 with SMOTE -----------------#
library(smotefamily)
smote.train <- train.set%>%
  mutate_at(vars(categorical_vars), as.numeric)

balanced_data <- SMOTE(X = smote.train%>% select(-TARGET),target = smote.train$TARGET)
table(balanced_data$data$TARGET)
balanced.data <- balanced_data$data
balanced.data$class <- as.factor(balanced.data$class)
smote_mod <- C5.0(class ~ ., data = balanced.data)
summary(smote_mod)

smote.test <- test.set%>%
  mutate_at(vars(categorical_vars), as.numeric)
smote_pred <- predict(smote_mod, smote.test)
levels(smote_pred) <- c(0,1)
confusionMatrix(smote_pred, y.test)

#------------------------ Fitting the brglm model -----------------------------#
# MPL_Jeffreys
MPL_model <-  glm(TARGET ~ ., family = "binomial", data = train.set,
                    method = "brglmFit", type = "MPL_Jeffreys")
# As_Mixed
mixed_model <-  glm(TARGET ~ ., family = "binomial", data = train.set,
                    method = "brglmFit", type = "AS_mixed")
# As_Mixed
corr_model <-  glm(TARGET ~ ., family = "binomial", data = train.set,
                    method = "brglmFit", type = "correction")
# As_Mixed
ML_model <-  glm(TARGET ~ ., family = "binomial", data = train.set,
                    method = "brglmFit", type = "ML")


mpl.pred <- predict(MPL_model, newdata = test.set%>% select(-TARGET), type = "response")
mpl.pred <- ifelse(mpl.pred>=0.5,1,0)
confusionMatrix(y.test, as.factor(mpl.pred))

mixed.pred <- predict(mixed_model, newdata = test.set%>% select(-TARGET), type = "response")
mixed.pred <- ifelse(mixed.pred>=0.5,1,0)
confusionMatrix(y.test, as.factor(mixed.pred))

corr.pred <- predict(corr_model, newdata = test.set%>% select(-TARGET), type = "response")
corr.pred <- ifelse(corr.pred>=0.5,1,0)
confusionMatrix(y.test, as.factor(corr.pred))

ml.pred <- predict(ML_model, newdata = test.set%>% select(-TARGET), type = "response")
ml.pred <- ifelse(ml.pred>=0.5,1,0)
confusionMatrix(y.test, as.factor(ml.pred))

predictions <- data.frame('MPL_Jeff' = mpl.pred, 'Mixed' = mixed.pred, 'Correction' = corr.pred, 'ML'=ml.pred)

#---------------- Selected Model ----------#
summary(MPL_model)
