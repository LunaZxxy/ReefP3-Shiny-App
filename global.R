library(ggthemes)
library(tidyverse)
library(dplyr)
library(plotly)
library(lubridate)
library(leaflet)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(C50)
library(ggcorrplot)
library(ggpubr)
library(gbm)
library(janitor)
library(knitr)
library("ggpubr")
library(Metrics)
library(cvTools)
#install.packages("rpart")
library(rpart)
library(rpart.plot)
#install.packages("randomForest")
library(randomForest)
#install.packages("xgboost")
library(xgboost)
library(lime)
library(reshape2)

set.seed(3888)

# If you want to update the data, set it as TRUE. 
# Or read csv directly from the link of the data
update_data <- FALSE

if (!dir.exists("data")) {
  dir.create("data")
}

if (update_data | !file.exists("data/bcodmo_dataset_773466_712b_5843_9069.csv")) {
  download.file("https://erddap.bco-dmo.org/erddap/tabledap/bcodmo_dataset_773466.csv",
                destfile = file.path(getwd(), "/data/https://erddap.bco-dmo.org/erddap/tabledap/bcodmo_dataset_773466.csv"))

}

reef <- read.csv("data/bcodmo_dataset_773466_712b_5843_9069.csv",
                       stringsAsFactors = FALSE,
                       check.names =  FALSE)

reef=reef[-1,]# delect the first labels row
reef$Date=mdy(reef$Date)# change as date 

#load world map
world_map = map_data("world")

# change variable
reef$Average_Bleaching=as.numeric(reef$Average_Bleaching)

# load cleaned data
data<-read.csv("data/clean_reef.csv")

#change as three part
data$label = ''
data_label= data %>% mutate(
  label = case_when(
    (latitude >= -16.3)  ~ 'Northern',
    (latitude < -16.3 & latitude >= -20.6)  ~ 'Inner',
    (latitude < -20.6 ) ~ 'Southern',
    TRUE ~ as.character(label))
) %>%drop_na()


# from pic
#               Northern
# -16.3 ---------
#               Inner
# -20.6 ---------
#               Southern






# map type
#map_theme_choose=c("Esri.WorldStreetMap","Esri.NatGeoWorldMap","Stamen.TonerLabels")
map_theme_choose=c("defult","Street Map","Nat Geo World Map")

change_map_type <- function(type_in){
  if(type_in=="Street Map") {
    return("Esri.WorldStreetMap")
  } else if (type_in=="Nat Geo World Map") {
    return("Esri.NatGeoWorldMap")
  } else if (type_in=="defult") {
    return("Stamen.TonerLabels")
  }
}

#factor
factor_li=c("SSTA(Sea Surface Temperature Anomaly)","Temperature(kelvin)",
"TSA Thermal Stress Anomaly","Windspeed","Climatological sea surface temperature",
            "Depth(distance from surface to study site)",
 "mean rad par", "mean atmp","SSTA frequency","mean air","mean sw" 
)

change_temp_factor <- function(input_label,type_in){
 if(type_in=="SSTA(Sea Surface Temperature Anomaly)") {
    return(input_label$ssta)
  } else if (type_in=="Temperature(kelvin)") {
    return(input_label$temperature_kelvin)
  } else if (type_in=="TSA Thermal Stress Anomaly") {
    return(input_label$tsa)
  }else if (type_in=="Windspeed") {
    return(input_label$windspeed)
  }else if (type_in=="Climatological sea surface temperature") {
    return(input_label$tsa_dhw)
  }else if (type_in=="Depth(distance from surface to study site)") {
    return(input_label$depth)
  }
  else if (type_in=="mean rad par") {
    return(input_label$mean_rad_par_1)
  }
  else if (type_in=="mean sw") {
    return(input_label$mean_sw_1)
  }
  else if (type_in=="mean air") {
    return(input_label$mean_airt_1)
  }
  else if (type_in=="SSTA frequency") {
    return(input_label$ssta_frequency)
  }
  else if (type_in=="mean atmp") {
    return(input_label$mean_atmp)
  }

}


#temp_factor=c("average_bleaching",                    
#              "ssta",                                 
#              "ssta_dhw",                             
#              "tsa",                                  
#              "tsa_mean",                             
#              "tsa_frequency",                        
#              "tsa_dhw",                                                    
#              "mean_airt_1",                          
#              "mean_atmp",                            
#              "mean_rad_par_1",                       
#              "mean_sw_1",
#)



temp_factor=c("average_bleaching","depth",
              "temperature_kelvin",
              "clim_sst","windspeed","ssta",
              "ssta_frequency","ssta_dhw","tsa",
              "tsa_frequency","tsa_dhw","mean_airt_1",
              "mean_atmp","mean_rad_par_1","mean_sw_1")



# for ml dataset
df=data_label
df$label<-as.factor(df$label)
df$average_bleaching<-asin(sqrt(df$average_bleaching/100))
data_location=rename(df, c("location"="label"))
df <- data_location %>% dplyr::select(c("average_bleaching","depth",
                             "temperature_kelvin",
                             "clim_sst","windspeed","ssta",
                             "ssta_frequency","ssta_dhw","tsa",
                             "tsa_frequency","tsa_dhw","mean_airt_1",
                             "mean_atmp","mean_rad_par_1","mean_sw_1",
                             "location"))



## Modelling
dfmodel<-na.omit(df)

set.seed(3888)
sam<-sample(nrow(dfmodel),nrow(dfmodel)*0.7)
dftrain<-dfmodel[sam,]
dftest<-dfmodel[-sam,]




# Function for evaluating model
eval_recap <- function(true, pretest){
  
  df_new <- data.frame(true = true,
                       pretest = pretest)
  
  data.frame(RMSE = sqrt(mean((pretest-true)^2)),
             MAE = mae(true,pretest),
             "R-Square" = 1- sum((pretest - true) ^ 2)/sum((true - mean(true)) ^ 2), #1 - rss/tss
             check.names = F
  ) 
  
}

#Baseline Model(using Mean)
pretest<-rep(mean(dftrain[,1]),nrow(dftest))
true<-dftest[,1]
baseline_accuracy <- eval_recap(true,pretest)


#gbdt
modelgbdt<-gbm(average_bleaching~.,dftrain,distribution = "gaussian",interaction.depth=4,
               shrinkage=0.1,n.trees=1000,n.cores=4,cv.fold=5)

bestntrees<-gbm.perf(modelgbdt,method="cv",plot.it = FALSE)

dfht<-summary(modelgbdt,n.trees=bestntrees,plotit = F)

pretest<-predict(modelgbdt,n.trees=bestntrees,dftest,type='response')
true<-dftest[,1]
gbdt_accuracy <- eval_recap(true, pretest)

set.seed(515)
folds <- createFolds(dfmodel$average_bleaching, k=10)
table_gbdt <- data.frame(
  RMSE = c(),
  MAE = c(),
  "R-Square" = c()
  
)

for(i in 1:10){
  
  fold_test <- dfmodel[folds[[i]],] 
  fold_train <- dfmodel[-folds[[i]],] 
  fold_true = fold_test$average_bleaching
  fold_predict <- predict(modelgbdt,n.trees=bestntrees,fold_test,type='response')
  t <- eval_recap(fold_true, fold_predict)
  table_gbdt <- rbind(table_gbdt,t)
}

gbdt_times <- as.numeric()
set.seed(515)

for (i in 1:9) {
  n = i*0.1
  sam<-sample(nrow(dfmodel),nrow(dfmodel)*n)
  train<-dfmodel[sam,]
  test<-dfmodel[-sam,]
  
  s = Sys.time()
  modelgbdt<-gbm(average_bleaching~.,
                 data = dftrain,
                 distribution = "gaussian",
                 interaction.depth=4,
                 shrinkage=0.1,
                 n.trees=1000,
                 n.cores=4,
                 cv.fold=5)
  e = Sys.time()
  gbdt_times <- append(gbdt_times,e-s)
  
}

explainer <- lime::lime( dftrain %>% dplyr::select(-average_bleaching), model = modelgbdt)

model_type.gbm <- function(x){
  return("regression") # for regression problem
}
predict_model.gbm <- function(x, newdata, type = "response") {
  
  # return prediction value
  predict(x, newdata) %>% as.data.frame()
  
}


# Select only the first 4 observations
selected_data <- dftest %>% dplyr::select(-average_bleaching) %>% dplyr::slice(1:4)

explanation <- lime::explain(x = selected_data, 
                             explainer = explainer, 
                             feature_select = "none", # Method of feature selection for lime
                             n_features = 10, # Number of features to explain the model
                             kernel_width = .75,
                             n_permutations = 5000,
                             n_labels = 1) #Because it is a binary classification model: 1


# randomForest
modelrf<-randomForest(average_bleaching~.,dftrain)

pretest<-predict(modelrf,dftest)
true<-dftest[,1]
rf_accuracy<-eval_recap(true, pretest)

set.seed(515)
folds <- createFolds(dfmodel$average_bleaching, k=10)
table_rf <- data.frame(
  RMSE = c(),
  MAE = c(),
  "R-Square" = c()
  
)

for(i in 1:10){
  
  fold_test <- dfmodel[folds[[i]],] 
  fold_train <- dfmodel[-folds[[i]],] 
  fold_true = fold_test$average_bleaching
  fold_predict <- predict(modelrf,fold_test)
  t <- eval_recap(fold_true, fold_predict)
  table_rf <- rbind(table_rf,t)
}

rf_times <- as.numeric()
set.seed(515)

for (i in 1:9) {
  n = i*0.1
  sam<-sample(nrow(dfmodel),nrow(dfmodel)*n)
  train<-dfmodel[sam,]
  test<-dfmodel[-sam,]
  
  s = Sys.time()
  modelrf<-randomForest(average_bleaching~.,train)
  e = Sys.time()
  rf_times <- append(rf_times,e-s)
  
}

explainer <- lime::lime( dftrain %>% dplyr::select(-average_bleaching), model = modelrf)

model_type.randomForest <- function(x){
  return("regression") # for regression problem
}
predict_model.randomForest <- function(x, newdata, type = "response") {
  
  # return prediction value
  predict(x, newdata) %>% as.data.frame()
  
}


# Select only the first 4 observations
selected_data <- dftest %>% dplyr::select(-average_bleaching) %>% dplyr::slice(1:4)

explanation <- lime::explain(x = selected_data, 
                             explainer = explainer, 
                             feature_select = "none", # Method of feature selection for lime
                             n_features = 10, # Number of features to explain the model
                             kernel_width = .75,
                             n_permutations = 5000,
                             n_labels = 1) #Because it is a binary classification model: 1


# Decision Tree
modelrpart<-rpart(average_bleaching~.,dftrain)
dfht<-data.frame(imp=modelrpart$variable.importance)
pretest<-predict(modelrpart,dftest)
true<-dftest$average_bleaching
rpart_accuracy<-eval_recap(true, pretest)

set.seed(515)
folds <- createFolds(dfmodel$average_bleaching, k=10)
table_rpart <- data.frame(
  RMSE = c(),
  MAE = c(),
  "R-Square" = c()
  
)

for(i in 1:10){
  
  fold_test <- dfmodel[folds[[i]],] 
  fold_train <- dfmodel[-folds[[i]],] 
  fold_true = fold_test$average_bleaching
  fold_predict <- predict(modelrpart,fold_test)
  t <- eval_recap(fold_true, fold_predict)
  table_rpart <- rbind(table_rpart,t)
}

rpart_times <- as.numeric()
# train & test -- 70:30
set.seed(515)

for (i in 1:9) {
  n = i*0.1
  sam<-sample(nrow(dfmodel),nrow(dfmodel)*n)
  train<-dfmodel[sam,]
  test<-dfmodel[-sam,]
  
  s = Sys.time()
  modelrpart<-rpart(average_bleaching~.,train)
  e = Sys.time()
  rpart_times <- append(rpart_times,e-s)
  
}

explainer <- lime::lime( dftrain %>% dplyr::select(-average_bleaching), model = modelrpart)

model_type.rpart <- function(x){
  return("regression") # for regression problem
}
predict_model.rpart <- function(x, newdata, type = "response") {
  
  # return prediction value
  predict(x, newdata) %>% as.data.frame()
  
}


# Select only the first 4 observations
selected_data <- dftest %>% dplyr::select(-average_bleaching) %>% dplyr::slice(1:4)

explanation <- lime::explain(x = selected_data, 
                             explainer = explainer, 
                             feature_select = "none", # Method of feature selection for lime
                             n_features = 10, # Number of features to explain the model
                             kernel_width = .75,
                             n_permutations = 5000,
                             n_labels = 1) #Because it is a binary classification model: 1



#xgboost
dfmodel2<-dfmodel
dfmodel3<-model.matrix(~.-1,dfmodel2)
set.seed(123)
sam1<-sample(nrow(dfmodel3),nrow(dfmodel3)*0.7)
dftrain<-dfmodel3[sam1,]
dftest<-dfmodel3[-sam1,]
modelxgb<-xgboost(data=as.matrix(dftrain[,-1]),
                  label=dftrain[,1],
                  objective="reg:squarederror",
                  max_depth=4,
                  early_stopping_rounds=4,
                  nrounds=500,
                  eta=0.1,
                  verbose=F)


variimp<-xgb.importance(colnames(as.matrix(dftrain[,-1])),modelxgb)

pretest<-predict(modelxgb,as.matrix(dftest[,-1]))
true<-dftest[,1]
xgb_accuracy<-eval_recap(true, pretest)

set.seed(515)
folds <- createFolds(dfmodel$average_bleaching, k=10)
table_xgb <- data.frame(
  RMSE = c(),
  MAE = c(),
  "R-Square" = c()
  
)

for(i in 1:10){
  
  fold_test <- dfmodel3[folds[[i]],] 
  fold_train <- dfmodel3[-folds[[i]],] 
  fold_true = fold_test[,1]
  fold_predict <- predict(modelxgb,as.matrix(fold_test[,-1]))
  t <- eval_recap(fold_true, fold_predict)
  table_xgb <- rbind(table_xgb,t)
}

xgb_times <- as.numeric()
set.seed(515)

for (i in 1:9) {
  n = i*0.1
  sam<-sample(nrow(dfmodel3),nrow(dfmodel3)*n)
  train<-dfmodel3[sam,]
  test<-dfmodel3[-sam,]
  
  s = Sys.time()
  modelxgb<-xgboost(data=as.matrix(train[,-1]),
                    label=train[,1],
                    objective="reg:squarederror",
                    max_depth=4,
                    early_stopping_rounds=4,
                    nrounds=500,
                    eta=0.1,
                    verbose=F)
  e = Sys.time()
  xgb_times <- append(xgb_times,e-s)
  
}

baseline_accuracy$type = 'baseline'
gbdt_accuracy$type = 'gbdt'
rf_accuracy$type = 'rf'
rpart_accuracy$type = 'rpart'
xgb_accuracy$type = 'xgb'
