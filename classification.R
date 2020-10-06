###Classificação

df_train=read.csv("C:/Documents/classificação/classification_train.csv")
df_test=read.csv("C:/Documents/classificação/classification_test.csv")

#verificando os tipos de variáveis
sapply(df_train,	class)

#resumo dos dados
#não há presença de valores faltantes
#x1 e x2 estão em escalas próximas, então não é necessário padronizar
summary(df_train)

#verificando se há desbalanceamento de classes (as classes estão balanceadas)
cbind(freq=table(df_train$target),	
      percentage=prop.table(table(df_train$target))*100)



#de acordo com a média e mediana parece que x1 e x2 possuem distribuições simétricas
#observa-se que X1 é multimodal e x2 está mais próxima de uma dist simétrica
#de acordo com o boxplot não há outliers

install.packages('ggplot2')
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)



#histograma
g1=ggplot(df_train, aes(x1)) + geom_histogram(bins=20)+ xlab('X1')+ ylab('')+
  theme_classic()

g2=ggplot(df_train, aes(x2)) + geom_histogram(bins=20)+ xlab('X2')+ ylab('')+
  theme_classic()

#boxplot
g3=ggplot(df_train, aes(x1)) + geom_boxplot( )+ xlab('x1')+ ylab('')+
  theme_classic()

g4=ggplot(df_train, aes(x2)) + geom_boxplot( )+ xlab('x2')+ ylab('')+
  theme_classic()

grid.arrange(g1,g2,g3,g4, ncol=2, nrow=2) 

#verificando a correlação (observa-se baixa correlação entre as variáveis x1 e x2, não há possibilidade de multicolinearidade)
#x2 tem uma certa correlação com a variável target
install.packages('corrplot')
library(corrplot)

corrmatrix=cor(df_train)
corrplot(corrmatrix, method = 'number')


#######Naive	 Bayes
install.packages("e1071")
library("e1071")

# Encode da classe
df_train$target = factor(df_train$target, levels = c(0,1))
df_test$target = factor(df_test$target, levels = c(0,1))

#criando o modelo
NB = naiveBayes(x = df_train[-3], y = df_train$target)
print(NB)

#fazendo a previsão
prev = predict(NB,newdata=df_test[-3])

#matriz de confusão
matriz_confusao = table(df_test[, 3], prev)
print(matriz_confusao)

#métricas de avaliação

precision_NB = 140/(140+29)#0,83

recall_NB= 140/(140+25)#0,85

F_2 = (1+2^2)*precision_NB*recall_NB/((2^2 * precision_NB)+recall_NB)#0,84




##########Regressão Logística

#verificando a importância de cada variável através do modelo random Forest
#o método mostra que x2 tem maior importância (91,2)
install.packages("randomForest")
library(randomForest)

importancia  = randomForest(target ~ ., df_train)
col = importance(importancia)
col
varImpPlot(importancia)


#treinando o modelo completo
fit	<-	glm(target~.,	data=df_train,	family=binomial(link='logit'))
summary(fit)

#Aplicando	no	modelo	dos	dados	de	treino
prob_train = predict(fit, type = 'response',  df_train[-3])
prev_train = ifelse(prob_train > 0.5, 1, 0)

matriz_confusao_train = table(df_train[, 3], prev_train)
print(matriz_confusao_train)

#métricas
precision_train = 278/(278+53)#0,84

recall_train= 278/(278+57)#0,83

F_2_train = (1+2^2)*precision_train*recall_train/((2^2 * precision_train)+recall_train)#0,83



#Aplicando	no	modelo	dos	dados	de	teste
prob_test = predict(fit, type = 'response', df_test[-3])
prev_test = ifelse(prob_test > 0.5, 1, 0)

matriz_confusao_test = table(df_test[, 3], prev_test)
print(matriz_confusao_test)

#métricas
precision_test = 137/(137+32)#0,81

recall_test= 137/(137+24)#0,85

F_2_test = (1+2^2)*precision_test*recall_test/((2^2 * precision_test)+recall_test)#0,84


#não há evidência de underffiting (resultados ruins na base de treinamento) e overffiting(resultados bons na base de treinamento e ruins na de teste)
#dos métodos ajustados,tanto o Naive Bayes quanto o modelo de regressão logístico, pode ser um bom classificador para os dados



#####Observa-se que treinando o modelo somente com x2, diminuiu o seu desempenho.
#####poderíamos também utilizar outros métodos de classificação para verificar se houve melhora nas métricas de desempenho.




#treinando o modelo com x2
fit	<-	glm(target~x2,	data=df_train,	family=binomial(link='logit'))
summary(fit)

#Aplicando	no	modelo	dos	dados	de	treino
prob_train = predict(fit, type = 'response',  df_train[-3])
prev_train = ifelse(prob_train > 0.5, 1, 0)

matriz_confusao_train = table(df_train[, 3], prev_train)
print(matriz_confusao_train)

#métricas
precision_train = 260/(260+71)#0,78

recall_train= 260/(260+72)#0,78

F_2_train = (1+2^2)*precision_train*recall_train/((2^2 * precision_train)+recall_train)#0,78



#Aplicando	no	modelo	dos	dados	de	teste
prob_test = predict(fit, type = 'response', df_test[-3])
prev_test = ifelse(prob_test > 0.5, 1, 0)

matriz_confusao_test = table(df_test[, 3], prev_test)
print(matriz_confusao_test)

#métricas
precision_test = 120/(120+49)#0,71

recall_test= 120/(120+37)#0,76

F_2_test = (1+2^2)*precision_test*recall_test/((2^2 * precision_test)+recall_test)#0,75


#Poderia fazer as previsões, observando se há valores em x1 e x2 no intervalo fora de 2 desvios, retirá-los (se existir) para verificar se a qualidade das métricas melhorariam.














