# Before move further for visualiztion part cross check No Missing Values.

lapply(train, function(x) data.frame(table(is.na(x))))

lapply(fct, function(x) data.frame(table(train[,x]))) %>% set_names(fct)


cate_graph <- function(x,Title = NULL,angle = 0){
  
    data.frame(table(train[,x])) %>% 
    
    ggplot(data = .,aes(x = reorder(Var1,-Freq) ,y = Freq,fill = Var1)) +
    
           geom_bar(stat = 'identity')+labs(title = Title,y = "",x = "") + 
    
    theme(axis.text.x = element_text(angle = angle), legend.position = "none",
          plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = 'white'),
          axis.line = element_line(colour = 'black'))

}

cate_graph('TARGET',Title = 'train set: label')
cate_graph('HOUSETYPE_MODE')

cate_graph('NAME_CONTRACT_TYPE')

cate_graph("WALLSMATERIAL_MODE")

cate_graph("EMERGENCYSTATE_MODE")

cate_graph("FONDKAPREMONT_MODE")

cate_graph("NAME_HOUSING_TYPE",Title = "House Type")

cate_graph("ORGANIZATION_TYPE",Title = "Organization Type",angle = 90)

cate_graph("NAME_EDUCATION_TYPE",Title = "Education")

cate_graph("OCCUPATION_TYPE",Title = "Occupation Type",angle = 90)

cate_graph("NAME_INCOME_TYPE",Title = "Income Type")

cate_graph('FLAG_DOCUMENT_18')

cate_graph('AMT_REQ_CREDIT_BUREAU_QRT')

cate_graph("FLAG_DOCUMENT_2")
cate_graph("DEF_30_CNT_SOCIAL_CIRCLE")

# Continuous features

numeric_plot <- function(x,Title = NULL){
  
  ggplot(data = train,aes_string(x = x)) +
    
  geom_histogram(aes(y = ..density..),fill = "#56B4E9",bins = 50)+
    
  stat_density(geom = "line",color = 'darkblue') + 
    
  scale_x_continuous(labels = scales::comma) +
    
  scale_y_continuous(labels = scales::comma)+
    
  theme_bw() + labs(y = "",x = x,title = paste("Distribution of ",x))

}

numeric_plot('AMT_CREDIT')
numeric_plot('AMT_ANNUITY')
numeric_plot('DAYS_EMPLOYED')

# Here we need recheck the Days_Employed Column
summary(train$DAYS_EMPLOYED/365)
# 1000 Years of EMploment is not possible 
#so it's be outlier replace the rows with the NA ,for further calculation
train[train$DAYS_EMPLOYED==max(train$DAYS_EMPLOYED),'DAYS_EMPLOYED'] <- NA
# Now once again recheck the plot


# Catrgorical fratures with labels.

# cateby_label_graph <- function(x,Title = NULL,angle = 0){
#   #x <- 'ORGANIZATION_TYPE'
#   x <- syms(x)
#   
#  train%>% group_by(!!!x,TARGET) %>% summarise(Freq = n()) %>% ungroup() %>% setNames(.,c('Type','Target','Freq')) %>% 
#   
#   ggplot(data = . ,aes(x = reorder(Type,-Freq),y = Freq,fill = Type)) +
#   
#   geom_bar(stat = 'identity')+labs(title = Title,y = "",x = "") +
# 
#   facet_grid(Target~.,scales="free",labeller = )+theme(legend.position = 'none',axis.text.x = element_text(angle = 90))
#   
# }
# 
# 
# cateby_label_graph("ORGANIZATION_TYPE",Title = "Income Type")
# 
# cateby_label_graph("CODE_GENDER",Title = "Gender")



cateby_label_graph <- function( x,Title = NULL,angle = 0) {
  
  x <- sym(x)
  
  train %>% filter(!is.na(!!x)) %>% 
    group_by(!! x, TARGET) %>%
    summarise(Freq = n()) %>% 
    ggplot(data = ., aes(x = reorder(!! x, -Freq), y = Freq, fill = !! x)) +
    geom_col() + 
    scale_y_continuous(labels = scales::comma)+
    labs(y = "", x = "") +
    facet_wrap(. ~ TARGET, scales = "free",strip.position = "top",
               labeller = as_labeller(c(`0` = paste0('Non-default :',Title),`1` = paste0('Default :',Title)))) + 
    theme(legend.position = 'none',
          panel.background = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.text.x = element_text(angle = angle,hjust = 0.5,vjust = 0.5))
}

cateby_label_graph(x =  'CODE_GENDER','Gender')
cateby_label_graph(x = 'NAME_EDUCATION_TYPE',Title = 'Education Type',angle = 10)
cateby_label_graph(x = 'NAME_INCOME_TYPE',Title = 'Income Type',angle = 90)
cateby_label_graph(x = 'OCCUPATION_TYPE',Title = 'OCCUPATION_TYPE',angle = 90)
cateby_label_graph(x = 'NAME_HOUSING_TYPE',Title = 'Housing Type',angle = 90)


# Numeric  fratures with labels.
numby_label_graph <- function(x){
  
  
  ggplot(data = train,aes_string(x = x)) +
  
  geom_density(aes(fill = factor(TARGET)),alpha = 0.5) + 
  
  scale_x_continuous(labels = scales::comma) +
  
  scale_y_continuous(labels = scales::comma)+
  
  scale_fill_manual('TARGET',values=c("orange","red"))+
  
  theme_bw() + labs(y = "Density",x = x,title = paste("Distribution of ",x,"by Target Value"),
                subtitle = paste("The Correlation between the ",x,"and the TARGET is :",
                                round(cor(train[,x],train[,'TARGET'],use = "na.or.complete"),4))) +
  theme(plot.title = element_text(hjust = 0.5,face = 'bold.italic'),
        plot.subtitle = element_text(hjust = 0.5,face = 'italic',color = 'blue'))
}

numby_label_graph('EXT_SOURCE_1')
numby_label_graph('EXT_SOURCE_2')
numby_label_graph('EXT_SOURCE_3')
numby_label_graph('OWN_CAR_AGE')
numby_label_graph('ENTRANCES_MEDI')
numby_label_graph('DAYS_BIRTH')

# Income of the client by Housing Type.
Income_Dist <- function(x,Title = NULL,angle = 0){
  
  x <- sym(x)
  
  train %>% filter(!is.na(!!x)) %>% 
  ggplot(aes(x = !!x,fill = !!x))+
  scale_y_log10(
    breaks = scales::trans_breaks('log10',function(x) 10^x),
    labels = scales::trans_format("log10",scales::math_format(10^.x)))+
  geom_boxplot(aes(y = AMT_INCOME_TOTAL))+
    theme_bw()+
  labs(y = "Income of Client " ,
       x = x ,title = paste("Distribution of Client Income by",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = angle))
}

Income_Dist('NAME_INCOME_TYPE')
Income_Dist('NAME_EDUCATION_TYPE')
Income_Dist('NAME_FAMILY_STATUS')
Income_Dist('NAME_HOUSING_TYPE')
Income_Dist('OCCUPATION_TYPE',angle = 90)



# Days Birth
ggplot(data = train,aes(x = DAYS_BIRTH/-365))+geom_histogram(bins = 25,fill = 'blue',color="black")+
  theme_bw()+labs(x = 'Age (years)',y = 'Count',title = 'Age of Client')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(from = 0,to = 17500, by = 2500))


ggplot(data = train,aes(x = DAYS_BIRTH/-365))+geom_density(aes(color = factor(TARGET)),alpha = 0.5)+
  theme_bw()+labs(x = 'Age (years)',y = 'Density',title = 'Distribution of the Age ')+
  theme(plot.title = element_text(hjust = 0.5))+scale_color_manual('TARGET',values=c("darkblue","red"))


  train %>% 
    select(TARGET,DAYS_BIRTH) %>% 
      mutate(Year = DAYS_BIRTH/-365,
           Years_Binned = cut(DAYS_BIRTH/-365,breaks = seq(from = 20,to = 70,by = 5))) %>% 
     group_by(Years_Binned) %>% 
          summarise_all(mean) %>% 
    ggplot(.,aes(Years_Binned,TARGET*100))+
        geom_bar(stat = 'identity',fill = 'orange') +
    labs(y = 'Failure to Repay(%)', x= 'Age Group (years)', title = 'Failure to Repay by Age Group') +
     theme_bw()

  
# Most of Young age peoples are failure to repay the money.
  
  
