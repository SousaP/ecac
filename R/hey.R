library(plyr)
library(rpart)


genders_function<- function(data){

  genders <- c()

  for (birthdate in data$birth_number){

    year = birthdate %/% 10000
    month = (birthdate %% 10000) %/% 100
    day = ((birthdate %% 10000) %% 100)

    if(month >= 50){
      genders <- append(genders, "F")
    }
    else{
      genders <- append(genders, "M")
    }
  }
  data$gender <- genders
  data
}

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

df_clients <- read.csv(file="data/client.csv", header=TRUE,sep=",")

df_district <- read.csv(file="data/district.csv", header=TRUE,sep=",")

df_trans <- read.csv(file="data/trans_train.csv", header=TRUE,sep=",")

df_trans_test <- read.csv(file="data/trans_test.csv", header=TRUE,sep=",")

df_disp <- read.csv(file="data/disp.csv", header=TRUE,sep=",")

df_loan <- read.csv(file="data/loan_train.csv", header=TRUE,sep=",")

df_loan_test <- read.csv(file="data/loan_test.csv", header=TRUE,sep=",")

df_clients <- genders_function(df_clients)


#train
df_train <- merge(x = df_clients, y = df_district[, c('code', "unemploymant.rate..96","average.salary")], by.x = "district_id", by.y = "code", all.x=TRUE)
#test
df_test <- merge(x = df_clients, y = df_district[, c('code', "unemploymant.rate..96","average.salary")], by.x = "district_id", by.y = "code", all.x=TRUE)

#train
df_trans <- ddply(df_trans,~account_id,function(x){x[which.max(x$date),]})
#test
df_trans_test<- ddply(df_trans_test,~account_id,function(x){x[which.max(x$date),]})



#train
df_account_balance <-  merge(x = df_disp, y = df_trans[, c('account_id', "balance")], by.x = "account_id", by.y = "account_id", all.x=TRUE)
df_account_balance<-df_account_balance[!(df_account_balance$type=="DISPONENT"),]


df_train <- merge(x = df_train, y = df_account_balance[, c('client_id', "balance",'account_id')], by.x = "client_id", by.y = "client_id", all.x=TRUE)

df_train <- merge(x = df_train,y = df_loan,by="account_id")



#test
df_account_balance_test <-  merge(x = df_disp, y = df_trans_test[, c('account_id', "balance")], by.x = "account_id", by.y = "account_id", all.x=TRUE)
df_account_balance_test<-df_account_balance_test[!(df_account_balance_test$type=="DISPONENT"),]


df_test <- merge(x = df_test, y = df_account_balance_test[, c('client_id', "balance",'account_id')], by.x = "client_id", by.y = "client_id", all.x=TRUE)

df_test <- merge(x = df_test,y = df_loan_test,by="account_id")



tree <- rpart(status ~ unemploymant.rate..96 +average.salary + balance  +amount +duration, data = df_train, method = "class")

pred <- predict(tree, df_test, type = 'class')

df_test$status = pred
df_test$status<- as.numeric(as.character(df_test$status))

loan_previsto <- df_test[c('loan_id','status')]
colnames(loan_previsto) <- c('Id', 'Predicted')
#write.csv(loan_previsto, file ="loan_previsto.csv",row.names=FALSE, quote=FALSE)

#write.csv(df_test, file ="data/test.csv",row.names=FALSE, quote=FALSE)



tree
plot(tree, uniform=TRUE,margin=0.2)
sum(loan_previsto$Predicted == -1)
