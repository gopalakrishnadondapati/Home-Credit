
# Read the datsets using the fread function in data.table
library(data.table)
  setwd('E:\\Kaggle\\Home_credit')
train <- data.table::fread('application_train.csv',na.strings = c('NA',""),stringsAsFactors = T)
test <- data.table::fread('application_test.csv',na.strings = c('NA',''),stringsAsFactors = T) 

# Convert data.table to data.frame

train <- data.frame(train)
test <- data.frame(test)


