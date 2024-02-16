setwd("C:/Users/pedro.silvestre/OneDrive - ValePresente/Área de Trabalho/UFU")
library(readxl)
dados <- read_excel("ANACOR.xlsx", sheet = 1, skip = 1)
require(candisc)

cor(dados)

y <- as.matrix(dados[,1:3])  # Variaveis jejum
x <- as.matrix(dados[,4:6])   # variaveis 1 hora

cc <- cancor(x, y, set.names=c("Sem Jejum","Jejum"))
cc

##No primeiro par a correlação canonica é 0.5415, no segundo par é 0.3182 e no terceiro par é 0.1021


ab <- coef(cc, type="both", standardize=TRUE)

ab

cor(x)%*%ab[[1]][,1]
cor(y)%*%ab[[2]][,1]

## não existe variavel supressora nesse caso as medições x2 e y2 influenciam positivamente no nivel de glicose

summary(dados)

plot(cc)