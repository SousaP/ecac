library(plyr)
library("rpart")

ages_function <- function(data){

  ages <-c()
  birth_dates <- data$birth_number
  loan_dates <- data$date

  for(i in 1:length(birth_dates)){

    year = birth_dates[i] %/% 10000
    month = (birth_dates[i] %% 10000) %/% 100
    day = ((birth_dates[i] %% 10000) %% 100)

    year_loan = loan_dates[i] %/% 10000
    month_loan = (loan_dates[i] %% 10000) %/% 100
    day_loan = ((loan_dates[i] %% 10000) %% 100)

    if(month > 50)
      month = month - 50

    year = paste("19", toString(year), sep="")
    year_loan = paste("19", toString(year_loan), sep="")
    birthdate <- paste(year, toString(month), toString(day), sep="-")
    loan_date <- paste(year_loan, toString(month_loan), toString(day_loan), sep="-")

    birthdate <- as.Date(birthdate)
    loan_date <- as.Date(loan_date)
    weeks <- difftime(loan_date, birthdate, units="weeks")

    age <- as.numeric(weeks)%/%52.177457
    ages <- append(ages, age)
  }

  data$age <- ages
  data
}

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

df_trans <- read.csv(file="data/trans_train_dates.csv", header=TRUE,sep=",")

df_trans_test <- read.csv(file="data/trans_test_dates.csv", header=TRUE,sep=",")

df_disp <- read.csv(file="data/disp.csv", header=TRUE,sep=",")

df_loan <- read.csv(file="data/loan_train.csv", header=TRUE,sep=",")

df_loan_test <- read.csv(file="data/loan_test.csv", header=TRUE,sep=",")

df_clients <- genders_function(df_clients)

#train
df_train <- merge(x = df_clients, y = df_district[, c('code', "unemploymant.rate..96","average.salary")], by.x = "district_id", by.y = "code", all.x=TRUE)
#test
df_test <- merge(x = df_clients, y = df_district[, c('code', "unemploymant.rate..96","average.salary")], by.x = "district_id", by.y = "code", all.x=TRUE)

#train
df_trans_balance <- ddply(df_trans,~account_id,function(x){x[which.max(x$date),]})
#test
df_trans_test_balance<- ddply(df_trans_test,~account_id,function(x){x[which.max(x$date),]})



#train
df_account_balance <-  merge(x = df_disp, y = df_trans_balance[, c('account_id', "balance")], by.x = "account_id", by.y = "account_id", all.x=TRUE)
df_account_balance<-df_account_balance[!(df_account_balance$type=="DISPONENT"),]


df_account_withdraw <- merge(x = df_disp, y = df_trans[, c('account_id', 'date',"k_symbol","type","amount")], by.x = "account_id", by.y = "account_id", all.x=TRUE)

df_account_withdraw$date <- as.Date(df_account_withdraw$date,"%Y-%m-%d")

df_account_withdraw$Month <- as.Date(cut(df_account_withdraw$date, breaks = "month"))

df_account_withdraw<-df_account_withdraw[!(df_account_withdraw$type.y =="credit"),]


df_account_withdraw <- df_account_withdraw %>%
  group_by(Month, account_id) %>%
  summarise(withdraw=sum(amount))



df_account_credit <- merge(x = df_disp, y = df_trans[, c('account_id', 'date',"k_symbol","type","amount")], by.x = "account_id", by.y = "account_id", all.x=TRUE)

df_account_credit$date <- as.Date(df_account_credit$date,"%Y-%m-%d")

df_account_credit$Month <- as.Date(cut(df_account_credit$date, breaks = "month"))

df_account_credit<-df_account_credit[(df_account_credit$type.y =="credit"),]


df_account_credit <- df_account_credit %>%
  group_by(Month, account_id) %>%
  summarise(credit=sum(amount))


df_account_withcredit <- merge(x = df_account_withdraw, y = df_account_credit, by =c('account_id','Month'),all = TRUE)
df_account_withcredit$withdraw[is.na(df_account_withcredit$withdraw)] <- 0
df_account_withcredit$credit[is.na(df_account_withcredit$credit)] <- 0

df_account_withcredit$month_balance <- (df_account_withcredit$credit - df_account_withcredit$withdraw)

df_account_withcredit <- df_account_withcredit %>%
  group_by(account_id) %>%
  summarise(month_balance=mean(month_balance))
######

dft_account_withdraw <- merge(x = df_disp, y = df_trans_test[, c('account_id', 'date',"k_symbol","type","amount")], by.x = "account_id", by.y = "account_id", all.x=TRUE)

dft_account_withdraw$date <- as.Date(dft_account_withdraw$date,"%Y-%m-%d")

dft_account_withdraw$Month <- as.Date(cut(dft_account_withdraw$date, breaks = "month"))

dft_account_withdraw<-dft_account_withdraw[!(dft_account_withdraw$type.y =="credit"),]


dft_account_withdraw <- dft_account_withdraw %>%
  group_by(Month, account_id) %>%
  summarise(withdraw=sum(amount))



dft_account_credit <- merge(x = df_disp, y = df_trans_test[, c('account_id', 'date',"k_symbol","type","amount")], by.x = "account_id", by.y = "account_id", all.x=TRUE)

dft_account_credit$date <- as.Date(dft_account_credit$date,"%Y-%m-%d")

dft_account_credit$Month <- as.Date(cut(dft_account_credit$date, breaks = "month"))

dft_account_credit<-dft_account_credit[(dft_account_credit$type.y =="credit"),]


dft_account_credit <- dft_account_credit %>%
  group_by(Month, account_id) %>%
  summarise(credit=sum(amount))


dft_account_withcredit <- merge(x = dft_account_withdraw, y = dft_account_credit, by =c('account_id','Month'),all = TRUE)
dft_account_withcredit$withdraw[is.na(dft_account_withcredit$withdraw)] <- 0
dft_account_withcredit$credit[is.na(dft_account_withcredit$credit)] <- 0

dft_account_withcredit$month_balance <- (dft_account_withcredit$credit - dft_account_withcredit$withdraw)

dft_account_withcredit <- dft_account_withcredit %>%
  group_by(account_id) %>%
  summarise(month_balance=mean(month_balance))

####################

df_train <- merge(x = df_train, y = df_account_balance[, c('client_id', "balance",'account_id')], by = "client_id", all.x=TRUE)

df_train <- merge(x = df_train,y = df_loan,by="account_id")



df_train <- ages_function(df_train)

df_train <- merge(x = df_train,y = df_account_withcredit,by="account_id")


#test
df_account_balance_test <-  merge(x = df_disp, y = df_trans_test_balance[, c('account_id', "balance")], by.x = "account_id", by.y = "account_id", all.x=TRUE)
df_account_balance_test<-df_account_balance_test[!(df_account_balance_test$type=="DISPONENT"),]



df_test <- merge(x = df_test, y = df_account_balance_test[, c('client_id', "balance",'account_id')], by.x = "client_id", by.y = "client_id", all.x=TRUE)


df_test <- merge(x = df_test,y = df_loan_test,by="account_id")

df_test <- ages_function(df_test)


df_test <- merge(x = df_test,y = dft_account_withcredit,by="account_id")



tree <- rpart(status ~ unemploymant.rate..96  + age + amount+ duration   + month_balance + balance , data = df_train, method = "class")

pred <- predict(tree, df_test, type = 'class')

df_test$status = pred
df_test$status<- as.numeric(as.character(df_test$status))

loan_previsto <- df_test[c('loan_id','status')]
colnames(loan_previsto) <- c('Id', 'Predicted')
write.csv(loan_previsto, file ="loan_previsto.csv",row.names=FALSE, quote=FALSE)

#write.csv(df_test, file ="data/test.csv",row.names=FALSE, quote=FALSE)

tree
plot(tree)
png(file="decision_tree.png")
# plots
plot(tree)
text(tree, pretty = 0)

sum(loan_previsto$Predicted == -1)
dev.off()

