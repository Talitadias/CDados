###Classifica��o

df_train=read.csv("C:/Documents/classifica��o/classification_train.csv")
df_test=read.csv("C:/Documents/classifica��o/classification_test.csv")

#verificando os tipos de vari�veis
sapply(df_train,	class)

#resumo dos dados
#n�o h� presen�a de valores faltantes
#x1 e x2 est�o em escalas pr�ximas, ent�o n�o � necess�rio padronizar
summary(df_train)

#verificando se h� desbalanceamento de classes (as classes est�o balanceadas)
cbind(freq=table(df_train$target),	
      percentage=prop.table(table(df_train$target))*100)



#de acordo com a m�dia e mediana parece que x1 e x2 possuem distribui��es sim�tricas
#observa-se que X1 � multimodal e x2 est� mais pr�xima de uma dist sim�trica
#de acordo com o boxplot n�o h� outliers

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

#verificando a correla��o (observa-se baixa correla��o entre as vari�veis x1 e x2, n�o h� possibilidade de multicolinearidade)
#x2 tem uma certa correla��o com a vari�vel target
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

#fazendo a previs�o
prev = predict(NB,newdata=df_test[-3])

#matriz de confus�o
matriz_confusao = table(df_test[, 3], prev)
print(matriz_confusao)

#m�tricas de avalia��o

precision_NB = 140/(140+29)#0,83

recall_NB= 140/(140+25)#0,85

F_2 = (1+2^2)*precision_NB*recall_NB/((2^2 * precision_NB)+recall_NB)#0,84




##########Regress�o Log�stica

#verificando a import�ncia de cada vari�vel atrav�s do modelo random Forest
#o m�todo mostra que x2 tem maior import�ncia (91,2)
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

#m�tricas
precision_train = 278/(278+53)#0,84

recall_train= 278/(278+57)#0,83

F_2_train = (1+2^2)*precision_train*recall_train/((2^2 * precision_train)+recall_train)#0,83



#Aplicando	no	modelo	dos	dados	de	teste
prob_test = predict(fit, type = 'response', df_test[-3])
prev_test = ifelse(prob_test > 0.5, 1, 0)

matriz_confusao_test = table(df_test[, 3], prev_test)
print(matriz_confusao_test)

#m�tricas
precision_test = 137/(137+32)#0,81

recall_test= 137/(137+24)#0,85

F_2_test = (1+2^2)*precision_test*recall_test/((2^2 * precision_test)+recall_test)#0,84


#n�o h� evid�ncia de underffiting (resultados ruins na base de treinamento) e overffiting(resultados bons na base de treinamento e ruins na de teste)
#dos m�todos ajustados,tanto o Naive Bayes quanto o modelo de regress�o log�stico, pode ser um bom classificador para os dados



#####Observa-se que treinando o modelo somente com x2, diminuiu o seu desempenho.
#####poder�amos tamb�m utilizar outros m�todos de classifica��o para verificar se houve melhora nas m�tricas de desempenho.




#treinando o modelo com x2
fit	<-	glm(target~x2,	data=df_train,	family=binomial(link='logit'))
summary(fit)

#Aplicando	no	modelo	dos	dados	de	treino
prob_train = predict(fit, type = 'response',  df_train[-3])
prev_train = ifelse(prob_train > 0.5, 1, 0)

matriz_confusao_train = table(df_train[, 3], prev_train)
print(matriz_confusao_train)

#m�tricas
precision_train = 260/(260+71)#0,78

recall_train= 260/(260+72)#0,78

F_2_train = (1+2^2)*precision_train*recall_train/((2^2 * precision_train)+recall_train)#0,78



#Aplicando	no	modelo	dos	dados	de	teste
prob_test = predict(fit, type = 'response', df_test[-3])
prev_test = ifelse(prob_test > 0.5, 1, 0)

matriz_confusao_test = table(df_test[, 3], prev_test)
print(matriz_confusao_test)

#m�tricas
precision_test = 120/(120+49)#0,71

recall_test= 120/(120+37)#0,76

F_2_test = (1+2^2)*precision_test*recall_test/((2^2 * precision_test)+recall_test)#0,75


#Poderia fazer as previs�es, observando se h� valores em x1 e x2 no intervalo fora de 2 desvios, retir�-los (se existir) para verificar se a qualidade das m�tricas melhorariam.














