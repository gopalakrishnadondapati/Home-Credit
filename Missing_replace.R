library(tidyverse)
library(magrittr)
# Dimensions of dataset
dim(train);dim(test)

# Respose value 

prop.table(table(train$TARGET))*100

View(train)

# Column types 
sapply(train, function(x) class(x)) %>% data.frame()



# Missing Values
# Total Missing Percentage in the dataset

paste("The total Missing Percentage in dataset ",
      round(sum(is.na(train))/(sum(is.na(train))+sum(!is.na(train))) * 100,4))

# Column wise Missing Percentage.
 sapply(train, function(x) {round((sum(is.na(x))/nrow(train))*100,2) } ) %>% 
   
  as.data.frame() %>% 
   
   rownames_to_column()  %>% 
   
   setNames(.,c('colname','% of missing values')) %>% 
   
   arrange(.,desc(`% of missing values`)) %>% 
   
   dplyr::filter(., `% of missing values` > 40) %>% 
   
   ggplot(data = .,aes(x = reorder(colname,-`% of missing values`) , y = `% of missing values`,fill = colname )) + 
   
   geom_bar(stat = 'identity') +
   
   theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
   
   ggtitle("Column Wise Missing Data") + xlab('Features')+ ylab( "Missing")
   

# # Consider the Missing Percentage Below 10 Columns .
# vv <-  sapply(train, function(x) { round((sum(is.na(x))/nrow(train))*100,2) } ) %>% 
#    
#    as.data.frame() %>% 
#    
#    rownames_to_column()  %>% 
#    
#    setNames(.,c('colname','missing')) %>% 
#    
#    dplyr::filter(.,missing < 10 ,missing != 0) 
#     
# vv <- mutate(vv,Class = sapply(vv$colname, function(x) class(train[,x])))
# 
#   
# # Here Expect NAME_TYPE_SUITE column remaining Columns are all numeric type.
# 
# # Replace the  Missing values of NAME_TYPE_SUITE Column with most repitive values.
# prop.table(table(train$NAME_TYPE_SUITE))*100
# 
# # Here "Unaccompanied" is the most repeting value so replace NA .
# 
# train$NAME_TYPE_SUITE[is.na(train$NAME_TYPE_SUITE)] <- 'Unaccompanied'
# 
# # Now Check  is their any outliers are present in the remaining columns.
# 
# sapply(vv$colname,function(x) summary(train[,x]))
#    
#    
# lapply(vv$colname,function(x) table(train[,x])) 
# 

 # Factor Columns.
 fct <- names(train[sapply(train, function(x) class(x) == 'factor')])
 
 paste("The total number of categorical (factor) Columns ",length(fct))
 
 
 # Now seperate the discrete numeric and continuous features
 
 # Discrete-Numeric.
 not_fct <- names(train) %in% fct
 
 dist_num <- names(train[!(not_fct)][sapply(names(train[!(not_fct)]),
                                            
                                            function(x)  
                                              length(unique(train[,x])) < 26)])
 
 paste("The total number of Discrete numeric  Columns ",length(dist_num))
 
 
 cont_num <- names(train[!(names(train) %in% c(fct,dist_num))])
 
 paste("The total number of Continuous numeric  Columns ",length(cont_num))
 
 
 
 # Cross Check any column names  are overlapping
 intersect(cont_num,c(dist_num,fct))
 
 # Now applying the "MODE" for fct and dist_num columns.
 
 Mode <- function(x) {
   ux <- na.omit(unique(x) )
   tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
 }
 
 f_d <- c(fct,dist_num)
 
 for(i in 1:ncol(train[f_d])){
   
   train[f_d][is.na(train[f_d][,i]), i] <- Mode(train[f_d][,i])
 }
 
 
 # Applying the Median to the cont_num columns
 for(i in 1:ncol(train[cont_num])){
   
   train[cont_num][is.na(train[cont_num][,i]),i] <- median(train[cont_num][,i],na.rm = T)
   
 }
 
 
 