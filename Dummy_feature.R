#Term: total credit / annuity
train$Term <- train$AMT_CREDIT/train$AMT_ANNUITY

#OVER_EXPECT_CREDIT: actual credit larger than goods price

train$over_expect_credit <- ifelse(train$AMT_CREDIT > train$AMT_GOODS_PRICE,1,0)

#MEAN_BUILDING_SCORE_TOTAL: the sum of all building AVG score
#train$mean_building_avg_score <- 
  
  
train %<>% mutate(
          over_expect_credit = ifelse(AMT_CREDIT > AMT_GOODS_PRICE,1,0),
          Mean_building_score_avg = rowMeans(.[45:58]),
          total_building_score_avg = rowSums(.[45:58]),
          flag_document_total = rowSums(.[97:116]),
          amt_req_credit_bureau_total = rowSums(.[117:122]),
          birth_employed_interval = DAYS_EMPLOYED - DAYS_BIRTH,
          birth_registration_interval = DAYS_REGISTRATION - DAYS_BIRTH,
          income_per_family_member = AMT_INCOME_TOTAL / CNT_FAM_MEMBERS,
          season_remaining = AMT_INCOME_TOTAL/4 - AMT_ANNUITY,
          children_ratio = CNT_CHILDREN / CNT_FAM_MEMBERS,Term = AMT_CREDIT/AMT_ANNUITY) 


# Dummy features 
library(mlr)
sapply(train, function(x) class(x)) %>% data.frame()
col_10 <- mlr::createDummyFeatures(train[fct][1:10])
col_16 <- mlr::createDummyFeatures(train[fct][11:16][-2])
train[fct] <- NULL
train <- cbind.data.frame(train,col_10,col_16)

# Logistics Regression.
train$SK_ID_CURR <- NULL
logitmod <- glm(formula = TARGET ~ .,data = train)


sc <- spark_connect(master = "local")
