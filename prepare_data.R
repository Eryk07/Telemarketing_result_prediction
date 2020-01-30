clients.all.vars <- read.table("bank-additional-full.csv", header=TRUE, sep=";")

myvars <- c("age", "job", "marital", "education", "contact", "month", "day_of_week", "campaign", "poutcome", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed", "y")
clients <- clients.all.vars[myvars]
summary(clients)

##(job)
#usuniemy klientów z 'unknown', jest ich zaledwie 330
clients2 <- clients[(clients$job!='unknown'),]
summary(clients2$job)

##(marital)
#skoro married jest najwiêcej, to niewielk¹ liczbê rekordów z unknown zmienimy na najpopularniejsz¹ kategoriê - 'married":
summary(clients2$marital)
index <- clients2$marital == 'unknown'
clients2$marital[index] <- 'married' 
summary(clients2$marital)

##(campaign)
# usuniêcie outlayerów, czyli powy¿ej 6 kontaktów
campaign <- count(clients2, "campaign")
campaign
clients2 <- clients2[(clients2$campaign <= 6),]
campaign <- count(clients2, "campaign")
campaign

nrow(clients)
nrow(clients2)


write.table(clients2,file="bank-additional-cleaned.csv", sep=";",row.names=FALSE)

