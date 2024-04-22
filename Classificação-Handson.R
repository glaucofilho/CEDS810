# ---
# CLASSIFICACAO - Codigo R
# Requisito de Analise: PERFIL DE SOBREVIVENTE DO TITANIC
# Profa. Emilia Colonese Carrard
# ---

# Utilize os pacotes
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(tidyverse)

setwd("C:/ITA/APEX/BigData")

################################################
# Importando os Dados
################################################
# Carregue os dados do dataset titanic.
tit <- read_csv2("titanic.csv", col_types = "ffcfnnnnncfc")

################################################
# Explorando e Transformando o dataset 
################################################

dim(tit)
glimpse(tit)
summary(tit)

# Verificando a existencia de nulos nas variaveis.

sapply(tit, function(x) sum(is.na(x)))

# Tratamento dos nulos

tit$HomeDest[is.na(tit$HomeDest)] = "Nao Informado"
tit$Cabin[is.na(tit$Cabin)] = "Nao Informado"
tit$Ticket[is.na(tit$Ticket)] = 0
tit$Fare[is.na(tit$Fare)] = 0

# Verificando a existencia de brancos.

tit%>%filter(Embarked=="")
tit%>%filter(SibSp=="")
tit%>%filter(ParCh=="")
tit%>%filter(Cabin=="")
tit%>%filter(HomeDest=="")

tit$SibSp = as.factor(tit$SibSp)
tit$ParCh = as.factor(tit$ParCh)

#################
# tratar Embarked

tit$Embarked = as.character(tit$Embarked)
tit$Embarked[is.na(tit$Embarked)] = "Z" # Nao Informado
tit$Embarked = as.factor(tit$Embarked)

tit1 <- tit

# Nao precisaremos da variavel Name para graficos ou modelagem, portanto 
# ela pode ser removida.
tit1$Name <- NULL

# Barplots de dados categoricos
p1 <- ggplot(tit1, aes(x = Sex)) + geom_bar(aes(fill = Survived)) + 
  geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', position = position_dodge(.1), size = 3)
p1

chisq.test(tit1$Sex, tit1$Survived)

p2 <- ggplot(tit1, aes(x = Class)) + geom_bar(aes(fill = Survived)) + 
  geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', position = position_dodge(.1), size = 3)
p2

p3 <- ggplot(tit1, aes(x = Embarked)) + geom_bar(aes(fill = Survived)) + 
  geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', position = position_dodge(.1), size = 3)
p3

p4 <- ggplot(tit1, aes(x = SibSp)) + geom_bar(aes(fill = Survived)) + 
  geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', position = position_dodge(.1), size = 3)
p4

p5 <- ggplot(tit1, aes(x = ParCh)) + geom_bar(aes(fill = Survived)) + 
  geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', position = position_dodge(.1), size = 3)
p5

p6 <- ggplot(tit1, aes(x = HomeDest)) + geom_bar(aes(fill = Survived)) + 
  geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', position = position_dodge(.1), size = 3)
p6

# Checando as proporcoes nulos de Age no dataset.
sum(is.na(tit1$Age))/nrow(tit1)

# Esse subconjunto eh 0,20% de nossos dados. Rmovemos as linhas com problema.
# Ficaremos com um dataset menor: 1046 linhas.
tit2<- tit1[complete.cases(tit1), ]

# Plot - Variaveis quantitativas
# Age histogram
p7 <- ggplot(data = tit2, aes(Age, color = Survived))+ geom_freqpoly(binwidth = 5, 
      size = 1)
p7

# Fare histogram
p8 <- ggplot(data = tit2, aes(Fare, color = Survived))+ geom_freqpoly(binwidth = 5, 
                                                                     size = 1)
p8

# Barplots da variavel alvo
p9 <- ggplot(tit2, aes(x = Survived)) + geom_bar(aes(fill = Survived)) + 
      geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')),
                stat = 'count', position = position_dodge(.1), size = 3)

p9

# Removendo o atributo HomeDest, Cabin e Ticket pois nao possuem relevancia para a analise.
tit2$Ticket <- NULL
tit2$Cabin <- NULL
tit2$HomeDest <- NULL

titanic <- tit2

################################################
# Preparando o dataset para analise
################################################

set.seed(1234)

aux = sample.int(nrow(titanic))      #auxiliar para seleção aleatoria de dados
aux = aux[1:floor(0.7*length(aux))]  #70% dos dados para treinamento
titanic[sample.int(nrow(titanic)),]  #visualiza uma sele  o aleat ria

treinamento = titanic[aux,]
validacao = titanic[-aux,]

#############################
# Criando a Arvore 
#############################

resultado = rpart(Survived ~., treinamento)
rpart.plot(resultado)


##################################
# Desempenho da Arvore de Decisao
##################################

predicao = predict(resultado, newdata = validacao[,-1], type = "class")
matriz_confusao = table(predicao, validacao$Survived)
matriz_confusao

acuracia = sum(diag(matriz_confusao))/sum(matriz_confusao)
acuracia

precisao_sobreviver = diag(matriz_confusao)[1]/sum(matriz_confusao[1,])
precisao_sobreviver

precisao_naosobreviver = diag(matriz_confusao)[2]/sum(matriz_confusao[2,])
precisao_naosobreviver

# Balancear os Dados para melhorar o resultado

titanic%>%
  group_by(Survived)%>%
  summarise(n())

sobrevivente <- titanic%>%filter(Survived==1)%>%sample_n(425)
nsobrevivente <- titanic%>%filter(Survived==0)%>%sample_n(425)
df <- rbind(sobrevivente,nsobrevivente)

# Refazer a predicao com os dados balanceados, para verificar se melhora o overfit

aux = sample.int(nrow(df))    #auxiliar para selecao aleatoria de dados
aux = aux[1:floor(0.7*length(aux))]  #70% dos dados para treinamento

treinamento = df[aux,]
validacao = df[-aux,]

# Recriando a Arvore
resultado = rpart(Survived ~., treinamento)
rpart.plot(resultado)

# Importancia das variaveis
summary(resultado)

# Variable importance
# Sex     Fare    SibSp    Class    ParCh      Age Embarked 
# 45       15       11       11        7        7        4

########################################################
# Ajustando as variaveis dependentes Age, SibSp e ParCh
########################################################
df <- titanic
df$AgeGroup = ""
df$AgeGroup[df$Age < 4] = "CriancaColo"
df$AgeGroup[df$Age >= 4.1 & df$Age < 17] = "Crianca"
df$AgeGroup[df$Age >= 17.1 & df$Age < 23] = "Jovem"
df$AgeGroup[df$Age >= 23.1 & df$Age < 65] = "Adulto"
df$AgeGroup[df$Age >= 65.1] = "Idoso"

df$Age <- NULL

# df$Relatives = 0
df$Relatives = (as.numeric(df$SibSp) + as.numeric(df$ParCh)) - 2
summary(df$Relatives)

#df$Relatives <- NULL
df$SibSp <- NULL
df$ParCh <- NULL

set.seed(1234)

aux = sample.int(nrow(df))      
aux = aux[1:floor(0.7*length(aux))]  
df[sample.int(nrow(df)),]  

treinamento = df[aux,]
validacao = df[-aux,]
resultado = rpart(Survived ~., treinamento)
rpart.plot(resultado)

predicao = predict(resultado, newdata = validacao[,-1], type = "class")
matriz_confusao = table(predicao, validacao$Survived)
matriz_confusao

acuracia = sum(diag(matriz_confusao))/sum(matriz_confusao)
acuracia

precisao_sobreviver = diag(matriz_confusao)[1]/sum(matriz_confusao[1,])
precisao_sobreviver

precisao_nsobreviver = diag(matriz_confusao)[2]/sum(matriz_confusao[2,])
precisao_nsobreviver

# import ncia das vari veis
summary(resultado)

# Variable importance
# Sex      Fare Relatives     Class  AgeGroup  Embarked 
#  59        13        12         7         5         4

##########################
# Desempenho do Algoritmo
##########################

library(ROCR)
library(e1071)

tit_pred_prob <- predict(resultado , validacao,  type = "prob")
head(tit_pred_prob)

roc_pred <-
  prediction(
    predictions = tit_pred_prob[, 1],
    labels = validacao$Survived 
  )

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
plot(roc_perf, main = "ROC Curve", col = "green", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)
auc_perf <- performance(roc_pred, measure = "auc")
tit_auc <- unlist(slot(auc_perf,"y.values"))

tit_auc
# [1] 0.7850462
# Resultado esta bom. 
# O nosso modelo preve ~ 79% do perfil de sobrevivente do acidente!
  
roc_data <- data.frame(cbind(y=unlist(slot(roc_perf,"y.values")), x=unlist(slot(roc_perf,"x.values"))))

roc_data %>%
  ggplot() +
  geom_line(mapping = aes(x=x,y=y),color="green",size=1)+
  geom_abline(slope=1, intercept=0, linetype="dashed",size=1)+
  labs(title="ROC Curve",
       x ="False positive rate", y = "True positive Rate")+
  theme_minimal()

#########################################
# Como interpretamos o resultado ??
#########################################

