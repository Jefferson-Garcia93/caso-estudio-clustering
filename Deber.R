clientes<- read.csv("C:/Users/poly_/Desktop/Pauly/Maestría/data.csv")
clientes

d=dist(clientes[,2:4],method= "euclidean") #distancia
#c=cor(clientes)
#c
#install.packages("corrplot")
#library(corrplot)
#corrplot(c)


fit = cmdscale(d,eig=TRUE, k=2) # k es el numero de dimensiones, rescalamiento, pasa a dos dimensiones
x = fit$points[,1]
x
y = fit$points[,2]
y
plot(x,y)
text(x, y, labels = row.names(clientes), cex=1)


#cloustering
set.seed(82597)
grupos=kmeans(clientes[,2:4],4) #clasificacion de 2 a 4, en cuatro categorias en funcion de las cuatro variables, kmeans saca centroides (valor medio)
#saca 12 medias de las 3 variables
g1=grupos$cluster
g1 #muestras como clasifico los 4000 datos, ej: datos 10 es categoria 1
g2=grupos$size
g2 #cuantos elementos hay por categoria

plot(x,y,col=c("red","green","blue","black")[g1])                #gráfico en colores en función al g1

grupos$centers   # caracteristicas de cada categoria

clientes$grupo = g1 #agrega la columna a la tabla original con el grupo que pertenece

pairs(clientes)
table(g1)


#jerarquico
#install.packages("dendextend")
library(dendextend)
hc= hclust(d,method = "complete")
hc
clus4=cutree(hc,4)
clus4
dend = as.dendrogram(hc)
dend
dend = color_branches(dend,4)
colors=c("red","green","blue","black")
plot(dend,fill=colors[clus4],main="cliente DHC")
table(clus4)
table

clientes$dendo = clus4

#Elbow
wi = c()
for (i in 1:10) 
{
  g = kmeans(clientes[,2:4],i) 
  wi[i] = g$tot.withinss
}
wi  #valores iterativos, repite del uno al 10
plot((1:length(wi)),wi, xlab="Numero de Clusters", ylab="Suma Cuadrados Internos", pch=19, col="red", type = "b")

#define el número cluster

#validacion interna

#install.packages("cluster")
library(cluster)
#install.packages("clvalid")
library(clValid)

du1=dunn(d,g1)
du1 #0,00053 kmeans
du2=dunn(d,clus4)   #este es el mejorcito
du2 #0,024 jerarquico mejor compactación



#coeficiente de silueta

sil1=silhouette(g1,d) # 0,58 
sil1
plot(sil1,col=1:4, border=NA)

sil2 = silhouette(clus4,d)  #0,86 ESTE ES EL MEJOR
sil2
plot(sil2,col=5:8, border=NA)



which(g1 %in% c(1)) #cone esto saco posiciones



#install.packages("aricode")
#install.packages("plyr")

library(aricode)
library(plyr)

ground=grupos$cluster #"saco los nombre"
ground
ARI=ARI(ground,g1)
ARI
ARI1=ARI(ground,clus4)
ARI1
AMI=AMI(ground,g1)
AMI
AMI1=AMI(ground,clus4)
AMI1
NMI=NMI(ground,g1,variant="joint")
NMI
NMI=NMI(ground,clus4,variant="joint")
NMI