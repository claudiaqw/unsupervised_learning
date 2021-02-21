library(cluster)
library(vegan)

census <- read.csv(file = 'data/census2000_conLongitudLatitud.csv')
summary(census)

#Imputación de missings

census <- na.omit(census)
summary(census)

#transformacion de variables
census$MedHHInc <- as.numeric(gsub('[$,]', '', census$MedHHInc))
census$MeanHHSz <- as.numeric(gsub('[$,]', '', census$MeanHHSz))
census$RegPop <- as.numeric(gsub('[,]', '', census$RegPop))


#removing location columns
census_data <- census[c(-1, -2, -3)]

#estandarizando los datos
census_data$RegDens <- log(census_data$RegDens-min(census_data$RegDens)+1)
census_data$RegPop <- log(census_data$RegPop-min(census_data$RegPop)+1)
census_data$MedHHInc <- log(census_data$MedHHInc-min(census_data$MedHHInc)+1)
census_data$MeanHHSz <- log(census_data$MeanHHSz-min(census_data$MeanHHSz)+1)
summary(census_data)

#selección del numero de clusters

#clustering jerarquico
matrizDistancias <- vegdist(census_data, method = "euclidean", na.rm = TRUE)
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
calinsky <- cascadeKM(census_data, inf.gr = 2, sup.gr = 10, iter = 100, criterion = "calinski")
calinsky$results


#Gráfico Silhouette
asw <- numeric(nrow(census_data))
for(k in 2:(nrow(census_data) - 1)){
  sil <- silhouette(cutree(clusterJerarquico, k = k), matrizDistancias)
  asw[k] <- summary(sil)$avg.width}
k.best <- which.max(asw)

plot(1: nrow(crime.scale), asw, type="h", 
     main = "Silhouette-optimal number of clusters", 
     xlab = "k (number of groups)", ylab = "Average silhouette width")
axis(1, k.best, paste("optimum", k.best, sep = "\n"), col = "red", font = 2,
     col.axis = "red")
points(k.best, max(asw), pch = 16, col = "red", cex = 1.5)

k <- 5
cutg <- cutree(clusterJerarquico, k = k)
sil <- silhouette(cutg, matrizDistancias)
rownames(sil) <- crime$State

plot(sil, main = "Silhouette plot", 
     cex.names = 0.8, col = 2:(k + 1), nmax = 100)

asignacionJerarquica<-cbind(census_data[,c(1,9:15)],
                            cutree(clusterJerarquico, k = 4))





