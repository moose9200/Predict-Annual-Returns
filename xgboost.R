rm(list = ls())

setwd("C:/Users/Moose/Desktop/Brainwaves/bw1")
options(scipen = 999)
library(data.table)
df = fread("train.csv", header = T , stringsAsFactors = FALSE, check.names = FALSE)

library(anytime)
df$sell_date = anydate(df$sell_date)
df$creation_date = anydate(df$creation_date)
df$start_date= anydate(df$start_date)

df$time_diff  = df$sell_date - df$creation_date

sapply(df,function(x) sum(is.na(x)))


df= subset(df,select=-c(portfolio_id,desk_id,office_id,start_date,creation_date,sell_date,
                        indicator_code,
                        hedge_value,
                        status ))



df = df[complete.cases(df),]

df$time_diff[df$time_diff<0] = NA
df = df[complete.cases(df),]

df1 = df[,c(1,3,5,8)]
df = df[,-c(1,3,5,8)]

df1 =  model.matrix(~.+0,data = df1 )

df = cbind(df1,df)
remove(df1)
df$time_diff = as.numeric(df$time_diff)

library(xgboost)
library(caret)
library(data.table)

df = as.data.frame(df)
labels = df$return

df = df[,-c(23)]
        
df = as.matrix(df)

dtrain <- xgb.DMatrix(data = df,label = labels) 

params <- list(booster = "gbtree", objective = "reg:linear", 
               eta=0.2, max_depth=50)



#to select how many nrounds will be better (where test error is minimun))

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 500, nfold = 5, 
                 showsd = T, stratified = T, print_every_n= 10, early_stop_round = 20, maximize = F)



xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 101, 
                   print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = RMSE)



xgb.importance(xgb1)




###################################################################################

library(data.table)
df = fread("test.csv", header = T , stringsAsFactors = FALSE, check.names = FALSE)

library(anytime)
df$sell_date = anydate(df$sell_date)
df$creation_date = anydate(df$creation_date)
df$start_date= anydate(df$start_date)

df$time_diff  = df$sell_date - df$creation_date

sapply(df,function(x) sum(is.na(x)))


df= subset(df,select=-c(portfolio_id,desk_id,office_id,start_date,creation_date,sell_date,
                        indicator_code,
                        hedge_value,
                        status ))


df$time_diff = abs(df$time_diff)

df1 = df[,c(1,3,5,8)]
df = df[,-c(1,3,5,8)]

df1 =  model.matrix(~.+0,data = df1 )

df = cbind(df1,df)
remove(df1)
df$time_diff = as.numeric(df$time_diff)

df = as.data.frame(df)

df = as.matrix(df)

dtrain <- xgb.DMatrix(data = df) 


#to select how many nrounds will be better (where test error is minimun))


pred = predict(xgb1,dtrain)

df = fread("test.csv", header = T , stringsAsFactors = FALSE, check.names = FALSE)

output = cbind(pred,df$portfolio_id)

output = output[,c(2,1)]


output = as.data.frame(output)
names(output)[1] = "portfolio_id"
names(output)[2] = "return"

write.csv(output,"output.csv",row.names = F)





