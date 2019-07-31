### Tutorial aula 07

# lendo os dados
sal <- read.csv("https://raw.githubusercontent.com/AndreaSanchezTapia/analise_de_dados_ENBT_2019/master/aula07/data/salarios.csv")

# explore os dados com as funções head e summary
head(sal)
summary(sal)

# criando objetos para auxiliar a construção do gráfico
# criando modelos lineares
mh <- lm(salario ~ experiencia, data=sal[sal$sexo=="H",])
mm <- lm(salario ~ experiencia, data=sal[sal$sexo=="M",])
coefh <- coef(mh)
coefm <- coef(mm)

# definindo os limites dos eixos
limy <- c(min(sal$salario),max(sal$salario))
limx <- c(min(sal$experiencia),max(sal$experiencia))

## definindo os nomes dos eixos
labx <- "Experiência (anos)"
laby <- "Salário (R$)"

#criando o gráfico em si
# define parametros graficos
par(mfrow=c(1,2), las=1, bty="l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas

# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="tomato", #col=define a cor
     ylim=limy, xlim=limx, #ylim/xlim definem os valores dos eixos
     ylab=laby, xlab=labx) #ylab/xlab definem os rótulos dos eixos
# linha do previsto pelo modelo
## a + b*x
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2) #lwd define a espessura da linha que será criada
mtext("A", 3, adj=0, font=2) #título do gráfico

## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",],
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)

#exportando o gráfico no formato PNG
# a funcao png cria o arquivo, daqui pra frente você não vai mais ver o gráfico
png("./figs/figura01.png", res=300, width=2400, height=1200)
# define parametros graficos
par(mfrow=c(1,2), las=1, bty="l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
mtext("A", 3, adj=0, font=2)
## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",],
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)
# para finalizar o gráfico e gerar o arquivo, precisamos rodar o dev.off()
dev.off()

#usando a função legend() em gráficos de dispersão
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
## usando points para adicionar os pontos do salario das mulheres
points(salario ~ experiencia, data=sal[sal$sexo=="M",],
       col="navy")
# linha do previsto pelo modelo das mulheres
## a + b*x
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)
# incluindo a legenda
legend("topleft", legend=c("homens", "mulheres"), #posicao, "caracterer da legenda",
       col=c("tomato", "navy"),
       lty=1, bty='n') #lty é o tipo de linha e o bty é a caixinha que fica a legenda

#usando o argumento FONT
# criando vetor de cores
cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")
# criando vetor com o nome das espécies
sp <- paste("I.", unique(iris$Species), sep=" ") #"I." é o nome do gênero, valores únicos da coluna Species do data=iris, separação por espaço

par(mfrow=c(2,2), bty='l', las=1)
boxplot(Sepal.Length ~ Species, data=iris, xlab="", col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)    #"1" é a posição do eixo X. Ele lê em sentido horário == 2=y, 3=em cima e 4=embaixo
boxplot(Sepal.Width ~ Species, data=iris, xlab="", col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Petal.Length ~ Species, data=iris,  col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Petal.Width ~ Species, data=iris, col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
par(mfrow=c(1,1))

#Gráfico de média com desvio padrão com arrows
# fixando uma semente de numeros aleatorios para manter o mesmo resultado no sample
set.seed(42)
# criando um data frame com valores medios e desvio padrão de uma variável
d2 <- data.frame(name=letters[1:5],
                 value=sample(seq(4,15),5),
                 sd=c(1,0.2,3,2,4))

plot(x=1:5, d2$value, las=1, bty='l', ylim=c(0, 18), pch=19, xaxt='n',
     xlab="names", ylab="value")
axis(1, at=1:5, labels=d2$name)
arrows(x0=1:5,
       y0=d2$value+d2$sd,
       y1=d2$value-d2$sd, angle=90, length=0.05, code=3)





#Exercícios:

#  Vamos voltar aos dados de iris. Use data(iris) para carregar os dados

#Construa uma janela gráfica com 1 linha e três colunas seguindo as instruções abaixo
#Comprimento da pétala no eixo x e largura da sépala no eixo y, sendo cada espécie em uma janela gráfica
#Padronize os eixos x e y para todas as espécies NÃO FAZER
#Plote cada espécie com uma cor diferente
#Adicione a reta do modelo linear
#Inclua rótulos A, B e C em cada uma das janelas gráficas
#Crie um diretório /figs em seu repositório
#Salve o gráfico em png em uma boa resolução, com o tamanho dos pontos e eixos legíveis no diretório /figs
#DESAFIO
#Construa um gráfico de pontos, contendo cada uma das variáveis (comprimento da sépala, largura da sépala, comprimento da pétala, largura da pétala) no eixo x e os valores médios no eixo y. Inclua as barras de erro (representando o desvio padrão em torno da média). Salve o gráfico em png no diretório /figs

data("iris")

par(mfrow=c(1,1))

par(mfrow=c(1,3))


# I. setosa
comp_petal_s<-iris$Petal.Length[iris$Species=="setosa"]
larg_sepal_s<-iris$Sepal.Width[iris$Species=="setosa"]

msetosa<-lm(larg_sepal_s~comp_petal_s)
coefsetosa<-coef(msetosa)

ly<-"Largura das sépalas"
lx<-"Comprimento das pétalas"

plot(larg_sepal_s~comp_petal_s,
     col="red", #col=define a cor
     ylab=ly, xlab=lx, las=1, bty="l") #ylab/xlab definem os rótulos dos eixos
# linha do previsto pelo modelo
## a + b*x
abline(a=coefsetosa[1], b=coefsetosa[2],
       col="red", lwd=2) #lwd define a espessura da linha que será criada
mtext("I. setosa", 3, adj=0, font=3) #título do gráfico



#I. versicolor
comp_petal_vers<-iris$Petal.Length[iris$Species=="versicolor"]
larg_sepal_vers<-iris$Sepal.Width[iris$Species=="versicolor"]

mversicolor<-lm(larg_sepal_vers~comp_petal_vers)
coefversicolor<-coef(mversicolor)

plot(larg_sepal_vers~comp_petal_vers,
     col="green", #col=define a cor
     ylab=ly, xlab=lx, las=1,bty="l") #ylab/xlab definem os rótulos dos eixos
# linha do previsto pelo modelo
## a + b*x
abline(a=coefversicolor[1], b=coefversicolor[2],
       col="green", lwd=2) #lwd define a espessura da linha que será criada
mtext("I. versicolor", 3, adj=0, font=3) #título do gráfico


#I. virginica
comp_petal_virg<-iris$Petal.Length[iris$Species=="virginica"]
larg_sepal_virg<-iris$Sepal.Width[iris$Species=="virginica"]

mvirginica<-lm(larg_sepal_virg~comp_petal_virg)
coefvirginica<-coef(mvirginica)

plot(larg_sepal_virg~comp_petal_virg,
     col="blue", #col=define a cor
     ylab=ly, xlab=lx, las=1,bty="l") #ylab/xlab definem os rótulos dos eixos
# linha do previsto pelo modelo
## a + b*x
abline(a=coefvirginica[1], b=coefvirginica[2],
       col="blue", lwd=2) #lwd define a espessura da linha que será criada
mtext("I. virginica", 3, adj=0, font=3) #título do gráfico



#exportando o gráfico no formato PNG
# a funcao png cria o arquivo, daqui pra frente você não vai mais ver o gráfico
png("./figs/figura02_iris.png", res=300, width=2400, height=1200)
par(mfrow=c(1,3))
plot(larg_sepal_s~comp_petal_s,
     col="red", #col=define a cor
     ylab=ly, xlab=lx, las=1, bty="l") #ylab/xlab definem os rótulos dos eixos
# linha do previsto pelo modelo
## a + b*x
abline(a=coefsetosa[1], b=coefsetosa[2],
       col="red", lwd=2) #lwd define a espessura da linha que será criada
mtext("I. setosa", 3, adj=0, font=3) #título do gráfico
plot(larg_sepal_vers~comp_petal_vers,
     col="green", #col=define a cor
     ylab=ly, xlab=lx, las=1,bty="l") #ylab/xlab definem os rótulos dos eixos
# linha do previsto pelo modelo
## a + b*x
abline(a=coefversicolor[1], b=coefversicolor[2],
       col="green", lwd=2) #lwd define a espessura da linha que será criada
mtext("I. versicolor", 3, adj=0, font=3) #título do gráfico
plot(larg_sepal_virg~comp_petal_virg,
     col="blue", #col=define a cor
     ylab=ly, xlab=lx, las=1,bty="l") #ylab/xlab definem os rótulos dos eixos
# linha do previsto pelo modelo
## a + b*x
abline(a=coefvirginica[1], b=coefvirginica[2],
       col="blue", lwd=2) #lwd define a espessura da linha que será criada
mtext("I. virginica", 3, adj=0, font=3)
dev.off()

par(mfrow=c(1,1))

#Construa um gráfico de pontos, contendo cada uma das variáveis (comprimento da sépala, largura da sépala, comprimento da pétala, largura da pétala) no eixo x e os valores médios no eixo y. Inclua as barras de erro (representando o desvio padrão em torno da média). Salve o gráfico em png no diretório /figs

matrix_pecasflorais<-matrix(NA, ncol=4, nrow=3) #matriz das médias
colnames(matrix_pecasflorais)<-names(iris[1:4])
rownames(matrix_pecasflorais)<-unique(iris$Species)

matrix_pecasflorais_sd<-matrix(NA, ncol=4, nrow=3) #matriz dos desvpads
colnames(matrix_pecasflorais_sd)<-names(iris[1:4])
rownames(matrix_pecasflorais_sd)<-unique(iris$Species)

for(i in 1:4){   #calculo das medias das peças florais das especies de iris
        matrix_pecasflorais[,i]<-tapply(iris[,i],iris$Species,mean)
}

for(i in 1:4){    #calculo do desvpad das pças florais das especies de iris
        matrix_pecasflorais_sd[,i]<-tapply(iris[,i],iris$Species,sd)
}

par(mfrow=c(1,3), las=1, bty="l")

#setosa
plot(x=1:4,matrix_pecasflorais[1,],ylim=c(0,8),xaxt="n",xlab="Peças florais de I. setosa",ylab="Mean", col="red")
axis(1, at=1:4, labels=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"))
arrows(x0=1:4,
       y0=matrix_pecasflorais[1,]+matrix_pecasflorais_sd[1,],
       y1=matrix_pecasflorais[1,]-matrix_pecasflorais_sd[1,], angle=90, length=0.05, code=3, col="red")

#versicolor
plot(x=1:4,matrix_pecasflorais[2,],ylim=c(0,8),xaxt="n",xlab="Peças florais de I. versicolor",ylab="Mean", col="blue")
axis(1, at=1:4, labels=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"))
arrows(x0=1:4,
       y0=matrix_pecasflorais[2,]+matrix_pecasflorais_sd[2,],
       y1=matrix_pecasflorais[2,]-matrix_pecasflorais_sd[2,], angle=90, length=0.05, code=3, col="blue")

#virginica
plot(x=1:4,matrix_pecasflorais[3,],ylim=c(0,8),xaxt="n",xlab="Peças florais de I. virginica",ylab="Mean", col="green")
axis(1, at=1:4, labels=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"))
arrows(x0=1:4,
       y0=matrix_pecasflorais[3,]+matrix_pecasflorais_sd[3,],
       y1=matrix_pecasflorais[3,]-matrix_pecasflorais_sd[3,], angle=90, length=0.05, code=3, col="green")



#salvando em png
png("./figs/desafio_07.png", res=300, width=4000, height=1200)
par(mfrow=c(1,3), las=1, bty="l")

#setosa
plot(x=1:4,matrix_pecasflorais[1,],ylim=c(0,8),xaxt="n",xlab="Peças florais de I. setosa",ylab="Mean", col="red")
axis(1, at=1:4, labels=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"))
arrows(x0=1:4,
       y0=matrix_pecasflorais[1,]+matrix_pecasflorais_sd[1,],
       y1=matrix_pecasflorais[1,]-matrix_pecasflorais_sd[1,], angle=90, length=0.05, code=3, col="red")

#versicolor
plot(x=1:4,matrix_pecasflorais[2,],ylim=c(0,8),xaxt="n",xlab="Peças florais de I. versicolor",ylab="Mean", col="blue")
axis(1, at=1:4, labels=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"))
arrows(x0=1:4,
       y0=matrix_pecasflorais[2,]+matrix_pecasflorais_sd[2,],
       y1=matrix_pecasflorais[2,]-matrix_pecasflorais_sd[2,], angle=90, length=0.05, code=3, col="blue")

#virginica
plot(x=1:4,matrix_pecasflorais[3,],ylim=c(0,8),xaxt="n",xlab="Peças florais de I. virginica",ylab="Mean", col="green")
axis(1, at=1:4, labels=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"))
arrows(x0=1:4,
       y0=matrix_pecasflorais[3,]+matrix_pecasflorais_sd[3,],
       y1=matrix_pecasflorais[3,]-matrix_pecasflorais_sd[3,], angle=90, length=0.05, code=3, col="green")

dev.off()

