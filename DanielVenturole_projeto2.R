getwd()
setwd("C:/FCD/bigdatarazure/Projetos-1-2v2/Acoustic_Extinguisher_Fire_Dataset")

#Bibliotecas
library(readxl)
library(caTools)
library(h2o)
library(C50)
library(class)
library(dplyr)
library(caret)
library(e1071)
library(rpart)

#Baixando os dados
dados <- read_excel("C:/FCD/bigdatarazure/Projetos-1-2v2/Acoustic_Extinguisher_Fire_Dataset/Acoustic_Extinguisher_Fire_Dataset.xlsx")
View(dados)

#Análise Exploratória
summary(dados)
str(dados)

#Substituindo os valores 0 e 1 da variável target
dados$STATUS <- sapply(dados$STATUS, function(x){
  ifelse (x == 0,"Falhou","Extinguiu")
  })

#reordenando as colunas antes de realizar a normalização
dados <- dados %>%
  select(STATUS, FUEL, everything())

#Normalização dos dados
dados_norm <- as.data.frame(scale(dados[3:7]))
View(dados_norm)
dados_norm_final <- as.data.frame(cbind(dados$STATUS, dados$FUEL, dados_norm))
View(dados_norm_final)

names(dados_norm_final) <- c("STATUS", "FUEL", "SIZE", "DISTANCE", "DESIBEL",
                             "AIRFLOW","FREQUENCY")

#Os dados estão balaceados?
table(dados_norm_final$STATUS) #Sim
str(dados_norm_final)

#Transformar as variáveis Status e Fuel em fator
dados_norm_final$STATUS <- as.factor(dados_norm_final$STATUS)
dados_norm_final$FUEL <- as.numeric(dados_norm_final$FUEL)
str(dados_norm_final)


#separando os dados de treino e teste
?createDataPartition
split <- createDataPartition(dados_norm_final$STATUS, p = 0.7, list = FALSE)
dados_treino <- dados_norm_final[split,]
dados_teste <- dados_norm_final[-split,]
dados_treino_labels <- dados_treino[,1]
dados_teste_labels <- dados_teste[,1]
View(dados_teste)

sum(is.na(dados_norm_final))
sum(is.na(dados_treino))
sum(is.na(dados_teste))
sum(is.na(dados_treino_labels))
sum(is.na(dados_teste_labels))

str(dados_norm_final)

#criando os modelos de classificação
modelo_c50 <- C5.0(STATUS ~., data = dados_treino)
modelo_svm <- svm(STATUS ~., data = dados_treino, type = 'C-classification', 
                  kernel = 'radial')
modelo_forest <- rpart(STATUS ~., data = dados_treino)


#Previsões
previsao_c50 <- predict(modelo_c50, dados_teste)
previsao_svm <- predict(modelo_svm, dados_teste)
previsao_forest <- predict(modelo_forest, dados_teste, type = "class")
?predict

#Confusion Matrix
table(previsao_c50, dados_teste$STATUS)
table(previsao_svm, dados_teste$STATUS)
table(previsao_forest, dados_teste$STATUS)

#Calculando o % de acertos
mean(previsao_c50==dados_teste$STATUS) #melhor modelo
mean(previsao_svm==dados_teste$STATUS)
mean(previsao_forest==dados_teste$STATUS)