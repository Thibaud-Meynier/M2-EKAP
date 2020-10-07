library(readxl)
gold=read_xlsx(file.choose())
gold$clot=as.numeric(gold$clot)
gold$logPrix=log(gold$clot)

diflogPrix=diff(gold$logPrix)

gold=gold[-c(1),]
gold$return=diflogPrix

gold_return=ts(gold$return,start = c(2010,10), frequency = 250)

plot(gold_return, )
