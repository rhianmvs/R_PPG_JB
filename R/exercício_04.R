read.delim("./Aulas/analise_de_dados_ENBT_2019-master/aula04/data/0012594-190621201848488.csv")

?read.csv

dados<-read.delim("./Aulas/analise_de_dados_ENBT_2019-master/aula04/data/0012594-190621201848488.csv")

dados$species


dados_sp_lat_long <- dados[c("species","decimalLatitude","decimalLongitude")]

write.table(dados_sp_lat_long, file = "./Data/exercicio_04.csv",sep=",",dec=".")
