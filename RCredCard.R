getwd()
setwd("C:/Users/ADVANCE/Documents/Python Scripts/DataScienceAcademy")

#install.packages("Amelia")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("reshape")
#install.packages("randomForest")

library(Amelia)
library(ggplot2)
library(caret)
library(reshape)
library(randomForest)
library(dplyr)

data <- read.csv("credit.csv")

View(data)
str(data)
head(data)

#Idade
head(data$AGE)
data$AGE <-cut(data$AGE, c(0,30,50,100), labels = c("Jovem","Adulto","Idoso"))
head(data$AGE)


#Sexo
data$SEX <- cut(data$SEX, c(0,1,2), labels = c("Masculino", "Feminino"))
head(data$SEX)

#Escolaridade
data$EDUCATION <- cut(data$EDUCATION, c(0,1,2,3,4), labels = c ("Pos-Graduado","Graduado","Ensino Medio","Outros"))
head(data$EDUCATION)

#Estado Civil
data$MARRIAGE <- cut(data$MARRIAGE, c(-1,0,1,2,3), labels = c("Desconhecido","Casado","Solteiro","Outros"))
head(data$MARRIAGE)


#Pagamentos
data$PAY_0 <- as.factor(data$PAY_0)
data$PAY_2 <- as.factor(data$PAY_2)
data$PAY_3 <- as.factor(data$PAY_3)
data$PAY_4 <- as.factor(data$PAY_4)
data$PAY_5 <- as.factor(data$PAY_5)
data$PAY_6 <- as.factor(data$PAY_6)

#Alterando Variavel dependente para o tipo de fator 
data$default.payment.next.month <- as.factor(data$default.payment.next.month)
head(data)
str(data)

#Renomeando a coluna de classe.
colnames(data)
colnames(data)[25] <-"inadimplente"
colnames(data)

#Verificando Missing Value
sapply(data, function(x) sum(is.na(x)))
missmap(data, main= "Valores Missing Observados")
data <- na.omit(data)

#Removendo a primeira Coluna
data$ID <- NULL

#Total inadimplentes vs no-inadimplentes
table(data$inadimplente)

#plot da distribuição com ggplot
qplot(inadimplente, data = data, geom = "bar") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#set the seed
set.seed(12345)

#Amostragem estratificada. Selecione as linhas de acordo com a variável inadimplente como strata
TrainingDataIndex <- createDataPartition(data$inadimplente, p= 0.45, list = FALSE)
TrainingDataIndex


#Criar  Dados de Treinamento como subconjunto do conjunto de dados com números de indice de linhas
#conforme identificado acima  e todas as colunas.
trainData <- data[TrainingDataIndex,]
table(trainData$inadimplente)

#veja porcentagem entre as classes
prop.table(table(trainData$inadimplente))

# Numero de linhas no dataset de training
nrow(trainData)

#Compara as porcentagem entre as classes de treinamento e dados originais
DistributionCompare <- cbind(prop.table(table(trainData$inadimplente)),prop.table(table(data$inadimplente)))
colnames(DistributionCompare) <- c()