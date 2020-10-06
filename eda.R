####EDA

##transformei o json em csv através do python


df=read.csv("C:/Documents/EDA/df.csv")

str(df)
summary(df)

#pela descritiva observa-se uma grande dispersão nas variáveis númericas, então, 
#como não conheço do negócio preferi retirar os dados faltantes.
df=df[!is.na(df$fat),]
df=df[!is.na(df$calories),]
df=df[!is.na(df$protein),]
df=df[!is.na(df$rating),]
df=df[!is.na(df$sodium),]

#foi observada linhas replicadas na variável categories
df=df[!duplicated(df$categories), ]


#boxplot para ver a distribuição dos dados
install.packages('ggplot2')
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)


#boxplot
g0=ggplot(df, aes(fat)) + geom_boxplot()+ xlab('fat')+ ylab('')+
  theme_classic()

g1=ggplot(df, aes(calories)) + geom_boxplot()+ xlab('calories')+ ylab('')+
  theme_classic()

g2=ggplot(df, aes(protein)) + geom_boxplot()+ xlab('protein')+ ylab('')+
  theme_classic()

g3=ggplot(df, aes(rating)) + geom_boxplot()+ xlab('rating')+ ylab('')+
  theme_classic()

g4=ggplot(df, aes(sodium)) + geom_boxplot()+ xlab('sodium')+ ylab('')+
  theme_classic()


grid.arrange(g0,g1,g2,g3,g4, ncol=2, nrow=3)

#todas as variáveis numéricas possuem valores considerados outliers, 
#ou seja, tem receitas que se destacam em fat, calories, protein, rating e sodium.
#Ou teve erro de digitação no conjunto de dados.

install.packages("tidyverse")
library(tidyverse)


# selecionar o top 10 de receitas em calorias
df %>% 
   top_n(10, calories) %>% 
  ggplot(aes(x = categories, y = calories, .desc = TRUE))+geom_bar(stat = 'identity')


#outra forma de analisar as receitas mais calóricas seria pelas nuvens de palavras.


#podemos fazer o top 10 de ingredientes.


#separando as variáveis numéricas no conjunto de dados
install.packages("microbenchmark")
library(microbenchmark)

df_n=Filter(is.numeric,df)

install.packages('corrplot')
library(corrplot)

corrmatrix=cor(df_n)
corrplot(corrmatrix, method = 'number')


#podemos observar que existe alta correlação entre fat, calories, protein e sodium.
#ou seja, quanto maior o teor de gordura na receita, cresce também as calorias, proteinas e o sódio. 
#a nota não tem relação com as variáveis acima.


#poderíamos separar o conjunto de dados em receitas com as piores e melhores avaliações
#e fazer o top 10 das receitas mais bem avaliadas e das com piores avaliações.


#Devido ao tempo e a falta de intimidade com algoritmos que trabalham com "strings",
#reconheço que a análise poderia ter sido mais rica. Mas sigo estudando e me reciclando 
#para melhorar a qualidade do meu trabalho. Obrigada por ter me proporcionado esse momento de 
#aprendizagem e me compromento a cada dia aprender mais.



