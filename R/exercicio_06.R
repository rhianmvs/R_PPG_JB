#tutorial aula 06

# o Quarteto de Anscombe

data("anscombe")

dim(anscombe) #dimensões; nº de linhas x colunas
head(anscombe)
class(anscombe)
str(anscombe) #estrutura do dado

mean(anscombe$x1) #em vez de fazer as médias dos valores de cada coluna assim
mean (anscombe$x2)

apply(anscombe[,1:4],2,mean) #pode ser feito com essa função em 1 linha
?apply

apply(anscombe[,5:8],2,mean)

cor(anscombe$x1,anscombe$y1) #correlação entre os dados
cor(anscombe$x2,anscombe$y2)

m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4) #função para modelos lineares; para regressão (variável y em função da variável x)

mlist <- list(m1, m2, m3, m4)
mlist[[1]] #chamar o primeiro elemento da mlist

summary(mlist[[1]]) #se adequa a natureza do objeto

lapply(mlist, coef) #aplicar o coeficiente no objeto mlist



# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2,2), #abre uma janela gráfica com 2 linhas  e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L
plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y2 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y3 ~ anscombe$x4)
abline(mlist[[4]])
par(mfrow=c(1,1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

# padrões morfológicos de iris

data("iris")

head(iris)
summary(iris)

table(iris$Species) #quantas informações por espécie


tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean) # media do comprimento de sepala por especie

aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean) # a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída

aggregate(Sepal.Length ~ Species, data=iris, mean) # ainda a mesma tarefa, com a mesma função mas em uma notação diferente; (coluna dos dados em função da coluna de spp, de onde vem os dados, função) MELHOR!!

aggregate(Sepal.Length ~ Species, data=iris, mean)
aggregate(Sepal.Width ~ Species, data=iris, mean)
aggregate(Petal.Length ~ Species, data=iris, mean)

aggregate(Sepal.Length ~ Species,data=iris, sd)
aggregate(Sepal.Length ~ Species, data=iris, sd)
aggregate(Sepal.Width ~ Species, data=iris, sd)
aggregate(Petal.Length ~ Species, data=iris, sd)


medias <- matrix(NA, ncol=3, nrow=4) # criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica

medias

colnames(medias) <- unique(iris$Species) #valores únicos
rownames(medias) <- names(iris)[-5]  # definindo o nome das colunas e das linhas da matriz

for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
}

medias

desvpad<-matrix(NA, ncol=3,nrow = 4)
colnames(desvpad)<-unique(iris$Species) #valores únicos
rownames(desvpad)<-names(iris)[-5]

iris

for (i in 1:4) {
  desvpad[i,]<- aggregate(iris[,i], list(iris$Species),sd)$x
}

desvpad

#medidas de tendência central
vars <- iris[,-5] #média
apply(vars, 2, mean)

apply(vars, 2, median) #mediana

freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]    #moda - valor mais frequente

apply(vars, 2, var) #variância

sd01 <- apply(vars, 2, sd)     #desvio padrão
# outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01==sd02

cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)                                    #coeficiente de variação


apply(vars, 2, quantile)# sumario de 5 numeros         #quantis ou percentis
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))# 5%, 50% e 95%


apply(vars, 2, range)   # a funcao range nos retorna os valores minimo e maximo
# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
# nunca nomeie um objeto com um nome já existente
my_range <- function(x){
  diff(range(x))
}
apply(vars, 2, my_range)     #o range estatístico é a diferença entre o maximo e o minimo, por isso o diff

apply(vars, 2, IQR)        #intervalo interquartil (IIQ) é a diferença entre o quartil superior e o quartil inferior

cor(vars)                   #correlação

## GRÁFICOS ##

barplot(table(iris$Species))     #gráfico de barras

par(mfrow=c(2,2))                #separa colunas e linhas na interface do plot
hist(iris$Sepal.Length)          #histogramas
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1,1))                #voltar ao normal

par(mfrow=c(1,2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)     #determina nº de breaks; exemplo na primeira coluna tem todos os indivíduos com largura de sépala entre 2.0 e 2.5

par(mfrow=c(1,2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)       #curva de densidade



plot(density(iris$Sepal.Width))           # plot da curva de densidade
hist(iris$Sepal.Width, freq = FALSE)      # plot da curva de densidade sobre o histograma de densidade
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE


###Agora é com você. Construa uma figura contendo quatro janelas gráficas (duas linhas e duas colunas) contendo em cada janela o histograma de densidade das variáveis contínuas do objeto iris com a adição da curva de densidade probabilística em cada um dos gráficos. As distribuições das variáveis são simétricas ou bimodais?

par(mfrow=c(2,2))

hist(iris$Sepal.Length, freq=FALSE)
lines(density(iris$Sepal.Length),col='blue')

hist(iris$Sepal.Width, freq=FALSE)
lines(density(iris$Sepal.Width),col='blue')

hist(iris$Petal.Length, freq=FALSE)
lines(density(iris$Petal.Length),col='blue')

hist(iris$Petal.Width, freq=FALSE)
lines(density(iris$Petal.Width),col='blue')

boxplot(iris$Sepal.Length)               #criando boxplot
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

boxplot(Sepal.Length ~ Species, data=iris)
boxplot(Sepal.Width ~ Species, data=iris)
boxplot(Petal.Length ~ Species, data=iris)
boxplot(Petal.Width ~ Species, data=iris)        #criando boxplot das variaveis em função das spp

#verificando os outliers

boxplot(iris$Sepal.Width)

my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE) #quando o plot=FALSE o gráfico não é plotado e as estatísticas por trás do gráfico são mostradas
my_boxplot
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers) # vamos usar a POSICAO para indexar o objeto, esse comando mostra quais as posicoes dos valores em iris$Sepal.Wigth estão no objeto outliers

iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")] #aqui é possível ver os outliers por sp

#verificando outliers em função das spp

boxplot(Sepal.Width ~ Species, data=iris)

my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 & iris$Species=="setosa",c("Sepal.Width", "Species")]

#distribuicao

par(mfrow=c(1,3))
qqnorm(iris$Sepal.Length[iris$Species=="setosa"],
       main="setosa")
qqline(iris$Sepal.Length[iris$Species=="setosa"])
qqnorm(iris$Sepal.Length[iris$Species=="versicolor"],
       main="versicolor")
qqline(iris$Sepal.Length[iris$Species=="versicolor"])
qqnorm(iris$Sepal.Length[iris$Species=="virginica"],
       main="virginica")
qqline(iris$Sepal.Length[iris$Species=="virginica"])

pairs(iris[-5]) #relação entre as varáveis

ggpairs(iris[-5])

