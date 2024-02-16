library(ggplot2)
library(jpeg)
library(grDevices)

imagem <- readJPEG("cano.jpeg")
dim(imagem)

x <- rep(1:dim(imagem)[2], each=dim(imagem)[1])
y <- rep(dim(imagem)[1]:1, times=dim(imagem)[2])
R <- as.vector(imagem[,,1])
G <- as.vector(imagem[,,2])
B <- as.vector(imagem[,,3])

imagemRGB <- data.frame(x=x, y=y, R=R, G=G, B=B)

cluster <- kmeans(imagemRGB[,c("R","G","B")], centers=10, nstart = 20, iter.max = 25)
cluster$centers

cores_rgb <- rgb(cluster$centers)
cores <- c('1' = cores_rgb[1],'2' = cores_rgb[2], '3' = cores_rgb[3], '4' = cores_rgb[4], '5' = cores_rgb[5], '6' = cores_rgb[6], '7' = cores_rgb[7], '8' = cores_rgb[8], '9' = cores_rgb[9], '10' = cores_rgb[10])

cluster1 <- as.factor(cluster$cluster)
imagemRGB$cluster <- cluster1

ggplot(imagemRGB, aes(x=x, y=y, color = cluster)) + 
  geom_point() + 
  scale_color_manual(values = cores) +
  theme_bw() + 
  theme(legend.position = "none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())
