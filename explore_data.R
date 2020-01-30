clients <- read.table("C:/Users/errry/Documents/R skrypty/bank-additional-full.csv", header=TRUE, sep=";")
install.packages("plyr")
library(plyr)
library(RColorBrewer)
install.packages("viridis")
library("viridis")

###EKSPLORACJA DANYCH###
summary(clients)
#klienci którzy wykupili lokatê terminow¹:
clients.yes <- clients[clients$y == "yes",]
summary(clients.yes)

##PODSTAWOWE BANKOWE INFORMACJE##

#(age)
boxplot(clients$age, xlab="Age of clients", horizontal=TRUE)

boxplot(clients$age,clients.yes$age, xlab="Age of clients",col=c("gray","lightgreen"), horizontal=TRUE)
legend("topleft", fill=c("gray","lightgreen"), legend=c("Clients","Clients will subscribe a term deposit"), horiz=TRUE)

hist(clients$age, xlab="age", main="Age of clients that will subscribe a term deposit", col="gray")
hist(clients.yes$age, xlab="age", main="Age of clients that will subscribe a term deposit", col="lightgreen", add=T)

#(job)
jobs <- count(clients, "job")
coul <- brewer.pal(12, "Set3") 
barplot(jobs$freq, names.arg=jobs$job, main="Jobs of clients", col=coul)

jobs.yes <- count(clients.yes, "job")
barplot(jobs.yes$freq, names.arg=jobs.yes$job, main="Jobs of clients that will subscribe a term deposit", col=coul)

jobs.sales$type <- jobs$job
jobs.sales$rate <- jobs.yes$freq/jobs$freq
jobs.sales <- jobs.sales[order(jobs.sales$rate),]
barplot(jobs.sales$rate, names.arg=jobs.sales$type, main="Sales conversion rate organized by jobs", col=viridis(12))

#(marital)
marital <- count(clients, "marital")
marital.yes <- count(clients.yes, "marital")

barplot(marital$freq, names.arg=marital$marital, main="Marital of clients", col="coul")

barplot(marital$freq, names.arg=marital$marital, main="Marital of clients that will subscribe a term deposit", col="gray")
barplot(marital.yes$freq, names.arg=marital.yes$marital, col=coul, add=T)

marital.sales <- marital[]
marital.sales$type <- marital$marital
marital.sales$rate <- marital.yes$freq/marital$freq
marital.sales <- marital.sales[order(marital.sales$rate),]
barplot(marital.sales$rate, names.arg=marital.sales$type, main="Sales conversion rate organized by marital", col=viridis(4))

#(education)
education <- count(clients, "education")
education.yes <- count(clients.yes, "education")

barplot(education$freq, names.arg=education$education, main="Education of clients", col=coul)

barplot(education$freq, names.arg=education$education, main="Education of clients that will subscribe a term deposit", col="gray")
barplot(education.yes$freq, names.arg=education.yes$education, col=coul, add=T)

education.sales <- education[]
education.sales$type <- education$education
education.sales$rate <- education.yes$freq/education$freq
education.sales <- education.sales[order(education.sales$rate),]
barplot(education.sales$rate, names.arg=education.sales$type, main="Sales conversion rate organized by education", col=viridis(8))

#(default)
default <- count(clients, "default")
default.yes <- count(clients.yes, "default")

barplot(default$freq, names.arg=default$default, main="Clients with credit in deafult that will subscribe a term deposit", col="gray")
barplot(default.yes$freq, names.arg=default.yes$default, col=c("lightgreen", "blue", "red"), add=T)

#(housing)
housing <- count(clients, "housing")
housing.yes <- count(clients.yes, "housing")

barplot(housing$freq, names.arg=housing$housing, main="Clients with housing loan that will subscribe a term deposit", col="gray")
barplot(housing.yes$freq, names.arg=housing.yes$housing, col=c("lightgreen", "blue", "red"), add=T)

housing.yes$freq/housing$freq

#(loan)
loan <- count(clients, "loan")
loan.yes <- count(clients.yes, "loan")

barplot(loan$freq, names.arg=loan$loan, main="Clients with loan that will subscribe a term deposit", col="gray")
barplot(loan.yes$freq, names.arg=loan.yes$loan, col=c("lightgreen", "blue", "red"), add=T)

loan.yes$freq/loan$freq

##INFORMACJE ZWI¥ZANE Z BIE¯¥C¥ KAMPANI¥ MARKETINGOW¥##

#(contact)
contact <- count(clients, "contact")
contact.yes <- count(clients.yes, "contact")

barplot(contact$freq, names.arg=contact$contact, main="Clients by contact that will subscribe a term deposit", col="gray")
barplot(contact.yes$freq, names.arg=contact.yes$contact, col=c("lightgreen", "blue", "red"), add=T)

contact.yes$freq/contact$freq

#(month)
month <- count(clients, "month")
month.yes <- count(clients.yes, "month")

barplot(month$freq, names.arg=month$month, main="Clients by month that will subscribe a term deposit", col="gray")
barplot(month.yes$freq, names.arg=month.yes$month, col="lightgreen", add=T)

month.yes$freq/month$freq

#(day_of_week)
day_of_week <- count(clients, "day_of_week")
day_of_week.yes <- count(clients.yes, "day_of_week")

barplot(day_of_week$freq, names.arg=day_of_week$day_of_week, main="Clients by day of week that will subscribe a term deposit", col="gray")
barplot(day_of_week.yes$freq, names.arg=day_of_week.yes$day_of_week, col="lightgreen", add=T)

##INNE INFORMACJE O KAMPANII##

#(campaign)
campaign <- count(clients, "campaign")
campaign.yes <- count(clients.yes, "campaign")

barplot(campaign$freq, names.arg=campaign$campaign, main="Clients by campaign that will subscribe a term deposit", col="gray")
barplot(campaign.yes$freq, names.arg=campaign.yes$campaign, col="lightgreen", add=T)

campaign.yes[1:6,]$freq/campaign[1:6,]$freq

boxplot(clients$campaign,clients.yes$campaign, xlab="Number of previous campaign during calls",col=c("gray","lightgreen"), horizontal=TRUE)
legend("topleft", fill=c("gray","lightgreen"), legend=c("Clients","Clients will subscribe a term deposit"), horiz=TRUE)

#(pdays)
pdays <- count(clients, "pdays")
pdays.yes <- count(clients.yes, "pdays")

pdays
pdays.yes

#(previous)
previous <- count(clients, "previous")
previous.yes <- count(clients.yes, "previous")

barplot(previous$freq, names.arg=previous$previous, main="Clients by count of previous contacts that will subscribe a term deposit", col="gray")
barplot(previous.yes$freq, names.arg=previous.yes$previous, col="lightgreen", add=T)

previous.yes$freq/previous$freq

#(poutcome)
poutcome <- count(clients, "poutcome")
poutcome.yes <- count(clients.yes, "poutcome")

barplot(poutcome$freq, names.arg=poutcome$poutcome, main="Clients by poutcome that will subscribe a term deposit", col="gray")
barplot(poutcome.yes$freq, names.arg=poutcome.yes$poutcome, col="lightgreen", add=T)

poutcome.yes$freq/poutcome$freq
poutcome.yes$freq[3]/nrow(clients.yes)

##KONTEKST SPO£ECZNY I EKONOMICZNY##

#(emp.var.rate)
boxplot(clients$emp.var.rate, xlab="Employment variaton rate during calls", horizontal=TRUE)

boxplot(clients$emp.var.rate,clients.yes$emp.var.rate, xlab="Employment variaton rate during calls",col=c("gray","lightgreen"), horizontal=TRUE)
legend("topleft", fill=c("gray","lightgreen"), legend=c("Clients","Clients will subscribe a term deposit"), horiz=TRUE)

##(cons.price.idx)
boxplot(clients$cons.price.idx, xlab="Consumer price index during calls", horizontal=TRUE)

boxplot(clients$cons.price.idx,clients.yes$cons.price.idx, xlab="Consumer price index during calls",col=c("gray","lightgreen"), horizontal=TRUE)
legend("topleft", fill=c("gray","lightgreen"), legend=c("Clients","Clients will subscribe a term deposit"), horiz=TRUE)

##(cons.conf.idx)
boxplot(clients$cons.conf.idx, xlab="Consumer confidence index during calls", horizontal=TRUE)

boxplot(clients$cons.conf.idx,clients.yes$cons.conf.idx, xlab="Consumer confidence index during calls",col=c("gray","lightgreen"), horizontal=TRUE)
legend("topleft", fill=c("gray","lightgreen"), legend=c("Clients","Clients will subscribe a term deposit"), horiz=TRUE)

##(euribor3m)
boxplot(clients$euribor3m, xlab="Euribor index during calls", horizontal=TRUE)

boxplot(clients$euribor3m,clients.yes$euribor3m, xlab="Euribor index during calls",col=c("gray","lightgreen"), horizontal=TRUE)
legend("topleft", fill=c("gray","lightgreen"), legend=c("Clients","Clients will subscribe a term deposit"), horiz=TRUE)

hist(clients$euribor3m, xlab="euribor3m", main="Euribor index during calls with clients that will subscribe a term deposit", col="gray")
hist(clients.yes$euribor3m, xlab="euribor3m", col="lightgreen", add=T)

##(nr.employed)
boxplot(clients$nr.employed, xlab="Number of employees during calls", horizontal=TRUE)

boxplot(clients$nr.employed,clients.yes$nr.employed, xlab="Number of employees during calls",col=c("gray","lightgreen"), horizontal=TRUE)
legend("topleft", fill=c("gray","lightgreen"), legend=c("Clients","Clients will subscribe a term deposit"), horiz=TRUE)




