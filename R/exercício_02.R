
exer2<-read.csv2("./Data/exercicio_02_modificado.csv")

uptake<-exer2$uptake
uptake
uptake[22:42]
Quebec_chilled<-uptake[22:42]
mean(Quebec_chilled)

mean(exer2[exer2$local=="Quebec" & exer2$tratamento=="chilled","uptake"])

