matriz <- cbind(c(0,9,3,6,11),c(9,0,7,5,10),c(3,7,0,9,2), c(6,5,9,0,8), c(11,10,2,8,0))
matriz <- as.dist(matriz)

modelo_hierarquico_complete <- hclust(matriz, method = "complete")
plot(modelo_hierarquico_complete, hang = -1)
rect.hclust(modelo_hierarquico_complete, k = 2, border = "red")

modelo_hierarquico_single <- hclust(matriz, method = "single")
plot(modelo_hierarquico_single, hang = -1)
rect.hclust(modelo_hierarquico_single, k = 2, border = "red")


library(datasets)
USArrests
str(USArrests)

modelo_hierarquico <- hclust(dist(USArrests), method = "complete")

plot(modelo_hierarquico, hang = -1)
rect.hclust(modelo_hierarquico, k = 3, border = "red")
