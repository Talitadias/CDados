###An�lise de Cluster

cluster=read.csv("C:/Documents/cluster/clusterization.csv")

summary(cluster)

install.packages('corrplot')
library(corrplot)

#Observa-se alta correla��o entre X0 e X2, X1 e X7, X0 e X3
corrmatrix=cor(cluster)
corrplot(corrmatrix, method = 'number')

#Algoritmo K-means

#o algoritmo funciona melhor com dados padronizados, n�o foi necess�rio fazer, pois a base j� veio com vari�veis padronizadas

#semente aleat�ria geradora, sempre ter� o mesmo resultado quando rodar o algoritmo
set.seed(1)

############Elbow method (optar pelo n�mero de cluster que mais diminuiu o valor de WCSS)
#escolhi 5 clusters, pois observei que a partir deste valor n�o h� muita diferen�a na queda do WCSS
wcss = vector()
for (i in 1:10) {
  kmeans = kmeans(x = cluster, centers = i, iter.max = 30)
  wcss[i] = sum(kmeans$withinss)
}

plot(1:10, wcss, type = 'b', xlab = 'Clusters', ylab = 'WCSS')

#criando os clusters
set.seed(1)
kmeans = kmeans(x = cluster, centers = 5, iter.max = 30)

#mostra cada elemento a que cluster pertence
previsoes = kmeans$cluster

#j� est� no defult do R, se der erro, instala o pacote primeiro
library(cluster)
plot(cluster, col = previsoes)

#a figura acima nos mostra a rela��o entre as vari�veis e os clusters, sendo �til para verificar padr�es no agrupamento.
#observa-se que as vari�veis altamente correlacionadas parecem variar entre diferentes grupos.

###########M�todo Silhouette score (defino o n�mero de clusters atrav�s do score, quando mais pr�ximo de 1, melhor.)
#De acordo com este m�todo, devemos considerar 2 clusters
silhouette_score = function(k){
  kmeans = kmeans(cluster, centers = k, iter.max = 30)
  ss = silhouette(kmeans$cluster, dist(cluster))
  mean(ss[, 3])
}
k = 2:10
avg_sil = sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Clusters', ylab='Silhouette Scores', frame=FALSE)

#criando os clusters
set.seed(1)
kmeans = kmeans(cluster, centers = 2, iter.max = 30)

prev = kmeans$cluster

plot(cluster, col = prev)


#######Calinski-harabasz (Atrav�s deste �ndice podemos fazer compara��es do n�mero de cluster, o k=n� de cluster, que obter o maior �ndice ser� o ideal.)
#o �ndice nos mostra que o melhor k foi obtido pelo m�todo Silhouette score.

install.packages("clusterSim")
library(clusterSim)

index.G1(cluster,previsoes)#3126.52

index.G1(cluster,prev)#4523.587


############Davies-bouldin(o k=n� de cluster, que obter o menor �ndice ser� o ideal)
#o �ndice nos mostra que o melhor k foi obtido pelo m�todo Elbow 

index.DB(cluster, previsoes)#1,7

index.DB(cluster, prev)#1,81


#Talvez o melhor k, esteja entre 2 e 5. Vou testar considerando K=3.
set.seed(1)
kmeans = kmeans(cluster, centers = 3, iter.max = 30)

prev1 = kmeans$cluster

plot(cluster, col = prev1)

index.G1(cluster,prev1)#4123.732
index.DB(cluster, prev1)#1,67


#De acordo com as an�lises acima, o valor ideal ser� k=2.



