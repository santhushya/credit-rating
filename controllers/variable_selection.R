library(caret)
library(glmnet)
library(dplyr)
load("~/San/Documents/iit/iit_Credit/iit_Credit/cash_loanWbureau.RData")
cash_loanWbureau <- cash_loanWbureau%>% select(-c(SK_ID_CURR))

set.seed(98)

# Elastic Net
index <- sample(1:nrow(cash_loanWbureau), round(nrow(cash_loanWbureau)*0.75), replace = F)
train.set <- cash_loanWbureau[index,] %>%select(-ORGANIZATION_TYPE)
test.set <- cash_loanWbureau[-index,]%>%select(-ORGANIZATION_TYPE)
rm(cash_loanWbureau, index)
gc()

#------------ Parameter Optimization --------------#
model_2 <- train(
  TARGET ~., data = train.set, method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneLength = 10
)

#------------- Best tuning parameter ---------------#
params = model_2$bestTune

# Bias reduced GLM
x <- model.matrix(TARGET~.,cash_loanWbureau %>%select(-ORGANIZATION_TYPE))[,-1]
train.set <- x[index,]
y  <- as.factor(cash_loanWbureau$TARGET)
y.train <- y[index]
set.seed(1)

test.set <- x[-index,]
test.set <- test.set[-index,]
y.test <- y[-index]
model_brg <- glmnet(x = train.set, y = y.train , alpha = params$alpha, lambda = params$lambda,
                      method = "brglmFit", family = "binomial")
coef(model_brg)

