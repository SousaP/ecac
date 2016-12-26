# Create df loan only owners
accountDisp <- merge(account, disp[ , c("account_id", "client_id", "type")], by = "account_id")
accountDisp<-accountDisp[!(accountDisp$type=="DISPONENT"),]
accountDispDistr <- merge(accountDisp, client[ , c("client_id", "district_id")], by ="client_id")
accountDispDistr$district_id.x <- NULL
accountDispDistr$date <- NULL
colnames(accountDispDistr)[5] <- "district_id"
temp <- merge(accountDispDistr, district[ , c("code", "average salary")], by.x ="district_id", by.y = "code")
df_loan <- merge(temp, loan_train[ , c("loan_id", "account_id", "amount", "duration", "payments", "status")], by = "account_id")
# %
df_loan$rel <- (df_loan$payments * 100 / df_loan$`average salary`)
# 0 - 1
df_loan$rel <- (df_loan$payments / df_loan$`average salary`)
df_loan <- merge(df_loan, district[ , c("code", "unemploymant rate '96")], by.x = "district_id", by.y = "code")
colnames(df_loan)[13] <- "rate"
# Normalize unemployment rate
df_loan[13] <- lapply(df_loan[13], scale)
#Another Aproach
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
df_loan[13] <- range01(df_loan[13])

grpLoan <- kmeans(df_loan[,c("status","rel")], centers=4)
grpLoan

grpDur <- kmeans(df_loan[,c("duration","status")], centers=3)
grpDur

grpAmount <- kmeans(df_loan[,c("amount","status")], centers=5)
grpAmount

grpUnem <- kmeans(df_loan[,c("rate","status")], centers=6)
grpUnem

grpPay <- kmeans(df_loan[,c("payments","status")], centers=6)
grpPay

wss <- (nrow(df_loan[,c("payments","status")])-1)*sum(apply(df_loan[,c("payments","status")],2,var))
for (i in 2:25) wss[i] <- sum(kmeans(df_loan[,c("payments","status")], 
                                     centers=i)$withinss)
plot(1:25, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Normallize with professor formula
#subtracting the average and dividing the result by the sample standard deviation
df_loan$norm_rel <- ((df_loan$rel - mean(df_loan$rel)) / (sd(df_loan$rel)) * sqrt((nrow(df_loan)-1)/nrow(df_loan)))
