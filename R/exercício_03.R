library(vegan)

read.csv2("./Data/exercicio_03_modificado.csv")

exer03<-read.csv2("./Data/exercicio_03_modificado.csv", row.names = 1) #Se eu quiser fazer cálculo eu preciso informar que a minha primeira coluna contém nomes

head(exer03)

indiv_parcela<-colSums(exer03)
indiv_parcela

