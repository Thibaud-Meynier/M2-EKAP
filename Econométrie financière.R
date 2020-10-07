library(readxl)
gold=read_xlsx(file.choose())
gold$clot=as.numeric(gold$clot)
gold$logPrix=log(gold$clot)

diflogPrix=diff(gold$logPrix)

gold=gold[-c(1),]
gold$return=diflogPrix

gold_return=ts(gold$return,start = c(2010,10), frequency = 250)

plot(gold_return, xlab="Date", ylab="daily return", main="Rendement journalier de l'or")

library(PerformanceAnalytics)
library(robustbase)

gold_return2=ts(gold_return)
gold_return_clean=Return.clean(gold_return2,method="boudt")

gold_return_clean=ts(gold_return_clean)

gold_cours=ts(gold$clot,start=c(2010,10),freq=254)
plot(gold_cours)

plot(gold_return2,col="red")
lines(gold_return_clean,col="blue")
legend("topleft",legend=c("raw gold return", "cleaned gold return"),col=c("red","blue"),lty=1, cex=0.7,bg="lightblue")

hist(gold_return_clean, ylim=c(0,700))

#stat descriptives

mean(gold_return_clean)
sd(gold_return_clean)
median(gold_return_clean)
summary(gold_return_clean)
skewness(gold_return_clean)
kurtosis(gold_return_clean)

V1=(skewness(gold_return_clean)-0)/((6/2481)^(1/2))
V1

V2=(kurtosis(gold_return_clean)-3)/((24/2481)^(1/2))
V2

library(ggplot2)
gold_return_clean2=as.data.frame(gold_return_clean)
gold_return_clean2$`Series 1`

#plot clean return & loi normal par rapport à la série
ggplot(gold_return_clean2, aes(x = `Series 1`)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(-0.04, 0.04, by = 0.005), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(gold_return_clean2$`Series 1`), sd = sd(gold_return_clean2$`Series 1`)))

