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
#Padronize os eixos x e y para todas as espécies
#Plote cada espécie com uma cor diferente
#Adicione a reta do modelo linear
#Inclua rótulos A, B e C em cada uma das janelas gráficas
#Crie um diretório /figs em seu repositório
#Salve o gráfico em png em uma boa resolução, com o tamanho dos pontos e eixos legíveis no diretório /figs
#DESAFIO
#Construa um gráfico de pontos, contendo cada uma das variáveis (comprimento da sépala, largura da sépala, comprimento da pétala, largura da pétala) no eixo x e os valores médios no eixo y. Inclua as barras de erro (representando o desvio padrão em torno da média). Salve o gráfico em png no diretório /figs

data("iris")

par(mfrow=c(1,3))

comp_petal_s<-iris$Petal.Length[iris$Species=="setosa"]
larg_sepal_s<-iris$Sepal.Width[iris$Species=="setosa"]

msetosa<-lm(comp_petal_s~larg_sepal_s)
coefsetosa<-coef(msetosa)

plot(comp_petal_s~larg_sepal_s)

