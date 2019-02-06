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
colnames(DistributionCompare) <- c("Treinamento", "Original")
DistributionCompare

#Melt Data - Converte COluna em linhas
meltedDComp <- melt(DistributionCompare)
meltedDComp

#Plot para ver a distribuicao do treinamento vs original - eh representativo ou existe sobre/ sob amostragem ??
ggplot(meltedDComp, aes(x = X1, y = value) ) + geom_bar( aes(fill = X2 ), stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Tudo o que nao esta no dataset de treinamento esta no dataset de teste. Observe o sinal - (menos)
testData <- data[-TrainingDataIndex,]

#Usaremos a validacao cruzada de 10 folds para treinar  e avaliar o modelo.
TrainingParameters <- trainControl(method = "cv", number = 10)




######################################################################################
###################### Random Forest Classificaion Model #############################
######################################################################################


#construindo o modelo
rf_model <- randomForest(inadimplente ~ ., data = trainData)
rf_model

#conferindo o erro do modelo
plot(rf_model, ylim= c(0,0.36))
legend('topright',colnames(rf_model$err.rate), col = 1:3,fill = 1:3)

#importancia das variaveis preditoras para as previsoes 
varImpPlot(rf_model)


#obtendo as variaveis mais importantes.
importance <- importance(rf_model)
varImportance <- data.frame(variables = row.names(importance), Importance = round(importance[,'MeanDecreaseGini'],2))


#Construindo o rank das variaveis baseados na importancia.
rankImportance <-varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))


# Usando ggplot2 para visualizar a importancia relativa das variaveis
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 

# Previsoes
predictionrf <- predict(rf_model, testData)

# Confusion Matrix
cmrf <- confusionMatrix(predictionrf, testData$inadimplente, positive = "1")
cmrf

# Salvando o modelo
saveRDS(rf_model, file = "rf_model.rds")

# Carregando o modelo
modelo <- readRDS("rf_model.rds")

# Calculando Precision, Recall e F1-Score, que sao metricas de avaliacao do modelo preditivo
y <- testData$inadimplente
predictions <- predictionrf

precision <- posPredValue(predictions, y)
precision

recall <- sensitivity(predictions, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1
