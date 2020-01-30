clients.all.vars <- read.table("/bank-additional-cleaned.csv", header=TRUE, sep=";")

clients<-data.frame(age, job, marital, education, contact,month, day_of_week, campaign, poutcome, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed, y)

clients<-data.frame(month, poutcome, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed, y)

sindex <- sample(1:nrow(clients))
clientsrr <- clients[sindex,]
traindata=clientsrr[c(1:(0.8*nrow(clientsrr))),]
testdata=clientsrr[c(((0.8*nrow(clientsrr))+1):(nrow(clientsrr)+1)),]

model<-rpart(y~.,data=traindata, method='class', parms =list(split="gini"),
             control = rpart.control(minsplit = 1000, minbucket = 
                                       1000, xval=0))


rpart.plot(model)
summary(model)

predict=predict(model, testdata, type='class')
tab<-table(predict, testdata$y)
print(tab)
sum(tab)
sum(tab)-sum(diag(tab))
print(100* sum(diag(tab))/sum(tab))

library(rpart.plot)
model<-rpart(y~.,data=traindata, method='class', parms =list(split="gini"),
             control = rpart.control(minsplit = 500, minbucket = 
                                       500, xval=0))


rpart.plot(model)
summary(model)

predict=predict(model, testdata, type='class')
tab<-table(predict, testdata$y)
print(tab)
sum(tab)
sum(tab)-sum(diag(tab))
print(100* sum(diag(tab))/sum(tab))

library(rpart.plot)
model<-rpart(y~.,data=traindata, method='class', parms =list(split="gini"),
             control = rpart.control(minsplit = 200, minbucket = 
                                       100, xval=0, cp=0,001))


rpart.plot(model)
summary(model)

predict=predict(model, testdata, type='class')
tab<-table(predict, testdata$y)
print(tab)
sum(tab)
sum(tab)-sum(diag(tab))
print(100* sum(diag(tab))/sum(tab))




library(rpart.plot)
model<-rpart(y~.,data=traindata, method='class', parms =list(split="gini"),
             control = rpart.control(minsplit = 200, minbucket = 
                                       100, xval=20, cp =0,001))


rpart.plot(model)
summary(model)

predict=predict(model, testdata, type='class')
tab<-table(predict, testdata$y)
print(tab)
sum(tab)
sum(tab)-sum(diag(tab))
print(100* sum(diag(tab))/sum(tab))



