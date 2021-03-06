###Regress�o

df_train=read.csv("C:/Documents/regress�o/regression_train.csv")
df_test=read.csv("C:/Documents/regress�o/regression_test.csv")


install.packages("visdat")
library(visdat)

#a vari�vel resposta tem 31% de dados faltantes e x2 tem 1%.
vis_miss(df_train)


#observa-se uma grande variabilidade na vari�vel resposta
summary(df_train)
str(df_train)


#imputa��o dos dados com o pacote mice
#fiz a imputa��o pelo m�todo random forest

install.packages("mice")
library(mice)

miceMod=mice(df_train[, !names(df_train) %in% "medv"], method="rf")  
df_train=complete(miceMod)  
anyNA(df_train)


install.packages('ggplot2')
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)


##Atrav�s do histograma e boxplot percebe-se que "target", apresenta uma distribui��o assim�trica e bastante outliers.
# A maior parte das vari�veis possuem outliers.

#histograma
g0=ggplot(df_train, aes(target)) + geom_histogram(bins=20)+ xlab('target')+ ylab('')+
  theme_classic()

g1=ggplot(df_train, aes(X1)) + geom_histogram(bins=20)+ xlab('X1')+ ylab('')+
  theme_classic()

g2=ggplot(df_train, aes(X2)) + geom_histogram(bins=20)+ xlab('X2')+ ylab('')+
  theme_classic()

g3=ggplot(df_train, aes(X3)) + geom_histogram(bins=20)+ xlab('X3')+ ylab('')+
  theme_classic()

g4=ggplot(df_train, aes(X4)) + geom_histogram(bins=20)+ xlab('X4')+ ylab('')+
  theme_classic()

g5=ggplot(df_train, aes(X5)) + geom_histogram(bins=20)+ xlab('X5')+ ylab('')+
  theme_classic()

g6=ggplot(df_train, aes(X6)) + geom_histogram(bins=20)+ xlab('X6')+ ylab('')+
  theme_classic()

g7=ggplot(df_train, aes(X7)) + geom_histogram(bins=20)+ xlab('X7')+ ylab('')+
  theme_classic()

grid.arrange(g0,g1,g2,g3,g4,g5,g6,g7, ncol=2, nrow=4)



#boxplot
g0=ggplot(df_train, aes(target)) + geom_boxplot()+ xlab('target')+ ylab('')+
  theme_classic()

g1=ggplot(df_train, aes(X1)) + geom_boxplot()+ xlab('X1')+ ylab('')+
  theme_classic()

g2=ggplot(df_train, aes(X2)) + geom_boxplot()+ xlab('X2')+ ylab('')+
  theme_classic()

g3=ggplot(df_train, aes(X3)) + geom_boxplot()+ xlab('X3')+ ylab('')+
  theme_classic()

g4=ggplot(df_train, aes(X4)) + geom_boxplot()+ xlab('X4')+ ylab('')+
  theme_classic()

g5=ggplot(df_train, aes(X5)) + geom_boxplot()+ xlab('X5')+ ylab('')+
  theme_classic()

g6=ggplot(df_train, aes(X6)) + geom_boxplot()+ xlab('X6')+ ylab('')+
  theme_classic()

g7=ggplot(df_train, aes(X7)) + geom_boxplot()+ xlab('X7')+ ylab('')+
  theme_classic()

grid.arrange(g0,g1,g2,g3,g4,g5,g6,g7, ncol=2, nrow=4)


#X2 (0,81) e X7 (0,77) possuem alta corre��o com "target", e s�o altamente correlacionadas entre si (0,93)

install.packages('corrplot')
library(corrplot)

corrmatrix=cor(df_train)
corrplot(corrmatrix, method = 'number')

#Verificando a import�ncia das vari�veis
#observa-se que X1, X2 e X7, tem maior import�ncia para o modelo.
#Como a import�ncia de X2 e X7 s�o muito pr�ximas, irei escolher X2, pois a correla��o mostra que ela influencia mais em target.

install.packages("randomForest")
library(randomForest)

importancia  = randomForest(target ~ ., df_train)
col = importance(importancia)
col
varImpPlot(importancia)


#N�o farei previs�o pelo modelo de regress�o pois um dos seus presupostos � que a vari�vel resposta tenha distribui��o normal,
#e verificamos  atrav�s do histograma e boxplot que "target" possui uma distribui��o assim�trica.
#Sendo assim, necess�rio fazer uma transforma��o. Por�m, transforma��es torna dif�cil a interpreta��o do modelo.
#Al�m de ser muito sens�vel � outliers.

#Antes vou tratar os missings da base teste
#a vari�vel resposta tem 31% de dados faltantes
summary(df_test)

miceMod=mice(df_test[, !names(df_test) %in% "medv"], method="rf")  
df_test = complete(miceMod)  
anyNA(df_test)

###########

#selecionando as vari�veis de interesse no modelo

install.packages("dplyr")
library(dplyr)

df_train=select(df_train,target,X1,X2)

df_test=select(df_test,target,X1,X2)

####Random Forest

#esse modelo n�o � sens�vel a outliers.

fit = randomForest(target~X1+X2, ntree = 500, df_train)

#Aplicando	no	modelo	dos	dados	de	treino
prev_train = predict(fit, df_train[-1])

install.packages("miscTools")
library(miscTools)

#R2=0,99
R2 = rSquared(df_train[['target']], resid = df_train[['target']] - prev_train)

install.packages("Metrics")
library(Metrics)

#rmse=136,54
rmse(df_train$target, prev_train)

#cor=0,99
cor(df_train$target, prev_train)


#prevendo na base teste
prev_test = predict(fit, df_test[-1])


#R2=0,95
R2 = rSquared(df_test[['target']], resid = df_test[['target']] - prev_test)


#rmse=259,55
rmse(df_test$target, prev_test)

#cor=0,97
cor(df_test$target, prev_test)


ggplot() +
  geom_point(mapping = aes(x = df_test$target, y = prev_test))+ xlab('Real')+ 
  ylab('Previs�o') +
  geom_abline(intercept = 0, slope = 1, color = "red")+
  theme_classic()


#No geral encontramos um bom ajuste de modelo atrav�s do Random Forest.

#poder�amos fazer imputa��o por outro m�todo, "rpart", por exemplo e verificar a qualidade do ajuste.


