##TUTORIAL AULA 08

library(raster)
library(rgdal)
library(rgeos)

readOGR("./Aulas/analise_de_dados_ENBT_2019-master/aula08/data/GoTRelease/political.shp",encoding = "UTF-8")

westeros<-readOGR("./Aulas/analise_de_dados_ENBT_2019-master/aula08/data/GoTRelease/political.shp",encoding = "UTF-8")

plot(westeros,las=1,axes=T)        # axes = T mostra os eixos
abline(h=0,lty = 2, col = "tomato")   #plotando a linha do 'equador'
                                      #lty = line type, tipo da linha
                                      #h ?

names(westeros)    #ver o nome das colunas da tabela de atributos
westeros$ClaimedBy

# Selecionando ‘The North’ (região reivindicada pela casa Stark):

stark <- westeros[westeros$ClaimedBy == "Stark",]
plot(stark, axes = T, las = 1)

# Criando pontos

pontos <- spsample(stark, 10, 'random')      # 1º gerando pontos aleatorios (objeto, N, tipo)
plot(stark, axes = T)                        # 2º plotar o mapa
points(pontos, pch = "+", col = "tomato", cex = 1.5)     # 3º usar a função 'point' chamando o objeto onde foram gerados os pontos, no caso foi criado o objeto 'pontos'; 'pch' é o tipo de ponto e 'cex' a sua dimensão

# Agora criando buffer

pontos.buffer <- buffer(pontos, width = 200000, dissolve = TRUE)  #'width' é a extensão do buffer e está em metros se o mapa tem long/lat CRS; 'dissolve' = T retira sobreposições

plot(stark, axes = T)
plot(pontos.buffer, add = T, col = "grey60") # 'add' adiciona o mapa
points(pontos, col = 'red', pch = 16) #esse comando adiciona os pontos no mapa dentro dos buffers criandos

    #com o argumento 'dissolve' FALSE

pontos.buffer <- buffer(pontos, width = 200000, dissolve = FALSE)
plot(stark, axes = T)
plot(pontos.buffer, add = T, col = "grey60")
points(pontos, col = 'red', pch = 16)

stark.buffer <- buffer(stark, width = 2, dissolve = TRUE)
plot(stark.buffer, col = "grey80", axes = T)
plot(stark, add = T, col = "lightblue")

#Incluir novos atributos em um objeto Spatial é similar a incluir uma nova coluna em um data frame.

westeros

westeros$regiao<-c(rep(1:3,each=4)) #incluindo uma nova coluna na tabela de atributos
names(westeros)

westeros_contorno = aggregate(westeros)
plot(westeros_contorno, axes = T)         #A função aggregate do pacote raster pode ser utilizada também para juntar polígonos com base em um

new_westeros = aggregate(westeros, by = "regiao")
plot(new_westeros, axes = T, col = terrain.colors(4)) #também podemos unir os polígonos indexados; na 'col' foi usada a paleta 'terrain.colors' e o número é a variação dessa paleta

plot(westeros, axes = T, col = terrain.colors(12))

##salvando com a funcao 'writeORG'; só salva dados vetoriais!!

writeOGR(
  as(westeros_contorno, "SpatialPolygonsDataFrame") , #nome do objeto a ser salvo como output. Tem que colocar a função as(x, "saída")
  dsn = "./Results", #diretorio a serem salvos os resultados
  layer = "westeros_contorno", #nome do arquivo
  driver = "ESRI Shapefile" #formato pretendido para exportação
)


ogrDrivers()


##convertendo em raster


westeros_raster <- raster(westeros_contorno, res = 0.08)
westeros_raster <- rasterize(westeros_contorno, westeros_raster) #deixando com o mesmo extent
plot(westeros_raster)

#MANIPULAÇÃO DE RASTER

#Importando

var1 <- raster("./Aulas/analise_de_dados_ENBT_2019-master/aula08/data/vars.tif")
var1
plot(var1)

#multiplos rasters: função 'stack'

lista <- list.files("./Aulas/analise_de_dados_ENBT_2019-master/aula08/data", pattern = "tif$", full.names = T)
vars <- stack(lista)
plot(vars)

#um raster multi-bandas

vars <- stack("./Aulas/analise_de_dados_ENBT_2019-master/aula08/data/vars.tif")
plot(vars)


## salvando raster

writeRaster(var1, "./Results/vars1.tif")  # se tiver arquivo de mesmo nome, pode usar o argumento overwrite = T


## Álgebra de raster

media <- mean(vars)
plot(media)


## Modificando Raster

westeros<-readOGR("./Aulas/analise_de_dados_ENBT_2019-master/aula08/data/GoTRelease/political.shp", encoding = "UTF-8")

stark <- westeros[westeros$ClaimedBy == "Stark",]
stark

plot(westeros, axes = T, las = 1)
plot(stark, add = T, col = "tomato")

plot(var1)
plot(westeros, add = T)

var1_croped <- crop(var1, stark) #função crop
var1_croped
plot(var1_croped)
plot(stark, add = T)

var1_masked <- mask(var1, stark) #função mask
var1_masked
plot(var1_masked)
plot(stark, add = T)

var1.masked2 = mask(crop(var1,stark), stark)
var1.masked2
plot(var1.masked2)
plot(stark, add = T)

# Alterando a resolução do raster

var1.aggregated<- aggregate(var1, fact = 5, fun = "mean")
var1.aggregated
plot(var1.aggregated)
