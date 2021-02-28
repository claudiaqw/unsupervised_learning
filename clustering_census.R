library(cluster)
library(vegan)
library(sqldf)
library(fmsb)
library(corrplot)
library(nima)
library(dplyr)
library(leaflet)

census <- read.csv(file = 'data/census2000_conLongitudLatitud.csv')
summary(census)

#Imputacion de missings

census.scale <- na.omit(census)
summary(census.scale)

#transformacion de variables
census.scale$MedHHInc <- as.numeric(gsub('[$,]', '', census.scale$MedHHInc))
census.scale$MeanHHSz <- as.numeric(gsub('[$,]', '', census.scale$MeanHHSz))
census.scale$RegPop <- as.numeric(gsub('[,]', '', census.scale$RegPop))


#estandarizando los datos
#census.scale <- as.data.frame(cbind(census.scale$idClient,scale(census.scale[,c(-1:-3)])))
summary(census.scale)

#census.scale$RegDens <- log(census.scale$RegDens-min(census.scale$RegDens)+1)
#census.scale$RegPop <- log(census.scale$RegPop-min(census.scale$RegPop)+1)
#census.scale$MedHHInc <- log(census.scale$MedHHInc-min(census.scale$MedHHInc)+1)
#census.scale$MeanHHSz <- log(census.scale$MeanHHSz-min(census.scale$MeanHHSz)+1)
#summary(census.scale)summary(census.scale)

census.scale$RegDens <- discrete_by_quantile(census.scale$RegDens)/4
census.scale$RegPop <- discrete_by_quantile(census.scale$RegPop)/4
census.scale$MedHHInc <- discrete_by_quantile(census.scale$MedHHInc)/4
census.scale$MeanHHSz <- discrete_by_quantile(census.scale$MeanHHSz)/4

matrizCorrelacion<-cor(census.scale[c(-1:-3)], method = c("spearman"))
corrplot(matrizCorrelacion, method="number", type="upper")


#seleccion del numero de clusters

#clustering jerarquico
matrizDistancias <- vegdist(census.scale[c(-1:-3)], method = "euclidean", na.rm = TRUE)
clusterJerarquico <- hclust(matrizDistancias, method="ward.D2")
graphics.off()

clusterJerarquico
plot(clusterJerarquico, labels = FALSE, main = "Dendrograma",ylab="Distance between clusters")

rect.hclust(clusterJerarquico, k=2, border="red") 
rect.hclust(clusterJerarquico, k=3, border="blue") 
rect.hclust(clusterJerarquico, k=4, border="green") 
rect.hclust(clusterJerarquico, k=5, border="yellow") 
rect.hclust(clusterJerarquico, k=6, border="purple") 
rect.hclust(clusterJerarquico, k=7, border="gray") 
rect.hclust(clusterJerarquico, k=8, border="black")


#Calinksy
calinsky <- cascadeKM(census.scale[c(-1:-3)], inf.gr = 2, sup.gr = 10, iter = 100, criterion = "calinski")
calinsky$results


asignacionJerarquica<-cbind(census.scale[c(-1:-3)],
                            cutree(clusterJerarquico, k = k))


# Calculo de los centroides asociados a los grupos jer�rquicos
colnames(asignacionJerarquica)[ncol(census.scale[c(-1:-3)])+1] <- "cluster"

centroidesJerarquico<-  sqldf("Select cluster, 
                        count(*) as cluster_size,
                        avg(RegDens) as RegDens_CAT,
                        avg(RegPop) as RegPop_CAT,
                        avg(MedHHInc) as MedHHInc_CAT,
                        avg(MeanHHSz) as MeanHHSz_CAT
                        from asignacionJerarquica
                        group by cluster")

# Distribucion porcentual de los grupos en la muestra (jer�rquico)

centroidesJerarquico[2]
centroidesJerarquico[2]/sum(centroidesJerarquico[2])


###K_MEANS####
# Se ejecuta el kmeans con los centroides obtenidos con el jerarquico


kmeans <- kmeans(census.scale[c(-1:-3)],centers=centroidesJerarquico[,3:ncol(centroidesJerarquico)])
kmeans$centers

# Distribucion porcentual de los grupos en el total de la cartera (optimizacion)
kmeans$size/sum(kmeans$size)


# Calcular SSW (Sum of Squares Wihin) para distintos numero de grupos
set.seed(42)
n <- nrow(census.scale)
p <- ncol(census.scale[c(-1:-3)]) 

SSW <- (n - 1) * sum(apply(census.scale[c(-1:-3)],2,var))

# Se aplica el metodo k-means con 5 inicializaciones distintas de centroides
# para que no sea tan sensible a los centroides de partida

for (i in 2:10) SSW[i] <- 
  sum(kmeans(census.scale[c(-1:-3)],centers=i,nstart=3,iter.max=20)$withinss)


#### Metodo de Elbow ####

plot(1:10, SSW, type="b", xlab="Number of Clusters", ylab="Sum of squares within groups",pch=19, col="steelblue4")

# Elegimos 4 clusters

kmeans <- kmeans(census.scale[c(-1:-3)],k,nstart=3)
asignacionOptimizacion<-cbind(census.scale,kmeans$cluster)
colnames(asignacionOptimizacion)[ncol(asignacionOptimizacion)] <- "cluster"

centroidesOptimizacion<-kmeans$centers

clusters_size<-sqldf("Select cluster, count(*) as cluster_size from asignacionOptimizacion Group by cluster")

# Adjuntamos los limites del grafico de radar (0 y 1)
# Esto es necesario para utilizar la funcion grafica de radar
# Tambien adjuntamos el comportamiento medio de la cartera
# para poder comparar cada centroide con la media total

centroidesOptimizacionParaRadar<-rbind(
  rep(1,nrow(centroidesOptimizacion)) , 
  rep(0,nrow(centroidesOptimizacion)) , 
  apply(centroidesOptimizacion, 2, mean),
  centroidesOptimizacion)

colors_border = c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9))
colors_in = c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4))

for (i in 1:nrow(centroidesOptimizacionParaRadar)-3)
{
  size<-clusters_size[i,2]
  
  radarchart(as.data.frame(centroidesOptimizacionParaRadar[c(1:3,3+i),]), axistype=1,
              pcol=colors_border, pfcol=colors_in, plwd=4 , plty=1,
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,5), cglwd=0.8,
              vlcex=0.8,
              title=paste0("Size:",size)
  )
}


set.seed(43)
indexes = sample(1:nrow(census.scale), size=0.25*nrow(census.scale))
asignacion.sample <- asignacionOptimizacion[indexes,] 



pal <- colorFactor(
  palette = "Set1",
  domain = asignacion.sample$cluster
)



leaflet(data = asignacion.sample) %>% 
  setView(lng = mean(asignacion.sample$LocX), lat = mean(asignacion.sample$LocY), zoom = 5) %>%
  addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
  addCircles(~LocX, ~LocY, 
             popup = ~paste0('<b>Mes:</b> ', as.character(cluster)), 
             radius = 100,
             color = ~pal(cluster),
             opacity = 0.2,
             weight = 0.5) %>%
  addLegend("bottomright", pal = pal, values = ~cluster,
            title = "Clusters",
            opacity = 1)


kmeans.sample <- kmeans$cluster[indexes] 


library(factoextra)
fviz_cluster(kmeans, data = census.scale[, -1], 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

