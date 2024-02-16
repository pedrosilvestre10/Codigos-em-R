library(stringr)

dados <- read.table("semente.txt", header = TRUE, sep = ",")
str(dados)
dados$V1 <- str_replace_all(dados$V1, "mm2", "")
dados$V1 <- as.numeric(dados$V1)
str(dados)

dados_padronizados <- scale(dados)
matriz_de_distancias <- dist(dados_padronizados, method = "euclidean")
matriz_de_distancias

modelo_hierarquico <- hclust(matriz_de_distancias, method = "complete")
plot(modelo_hierarquico, hang = -1)
rect.hclust(modelo_hierarquico, k = 5, border = "red")


grupos <- cutree(modelo_hierarquico, k = 5)

aglomerado1 <- dados[grupos == 1, ]
aglomerado2 <- dados[grupos == 2, ]
aglomerado3 <- dados[grupos == 3, ]
aglomerado4 <- dados[grupos == 4, ]
aglomerado5 <- dados[grupos == 5, ]

boxplot(aglomerado1$V1,
        aglomerado2$V1,
        aglomerado3$V1,
        aglomerado4$V1,
        aglomerado5$V1)

plot(modelo_hierarquico, hang = -1)
rect.hclust(modelo_hierarquico, k = 3, border = "red")


grupos2 <- cutree(modelo_hierarquico, k = 3)

aglomerado1.1 <- dados[grupos2 == 1, ]
aglomerado2.1 <- dados[grupos2 == 2, ]
aglomerado3.1 <- dados[grupos2 == 3, ]

boxplot(aglomerado1.1$V1,
        aglomerado2.1$V1,
        aglomerado3.1$V1)

#Inicialmente escolhi 5 aglomerados mais na intuição ,olhando o dendograma,
#mas depois de plotar os boxplots, percebi que se eu escolhesse menos aglomerados
#talvez teria uma melhor distribuição, e de fato, com 3 aglomerados, os boxplots 
#de algumas variaveis como V1 ficaram melhor definidas dentro de cada aglomerado.