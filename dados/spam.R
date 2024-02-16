library(stringr)
library(ggplot2)

dados <- read.csv("spam.csv")
str(dados)

# Juntar as colunas que foram divididas na hora de ler o conjunto de dados
for (j in 1:nrow(dados)) {
  dados$mensagem[j] <- paste(dados$v2[j], dados$X[j], dados$X.1[j], dados$X.2[j], sep = " ")
}

# Apagando as colunas que não iremos mais precisar
dados <- dados[,-(2:5)]
colnames(dados) <- c("resposta", "mensagem")

# Analisando a proporção de Spam e Não-Spam
ggplot(data = dados, aes(x = resposta))+
  geom_bar()

# Transformar todas as mensagens para o formato minúsculo
dados$mensagem <- str_to_lower(dados$mensagem)
dados$mensagem[1:10]

# Tratativa dos dados para considerar determinado padrão como um conjunto
## Tranformando números de tamanho maior ou igual a 7 em _numerogrande_
dados$mensagem <- str_replace_all(dados$mensagem, pattern = "\\b(\\d{7,})\\b", replacement = "_numerogrande_")

## Transformando todos os links que começam em HTTP ou WWW em _link_
dados$mensagem <- str_replace_all(dados$mensagem, pattern = "\\b((http|www)[^ ]+)", replacement = "_link_")

## Retirando as pontuações
dados$mensagem <- str_replace_all(dados$mensagem, "[[:punctuation:]+><\\\\]", "")

# Criando o Vocabulario
palavras <- str_split(dados$mensagem, pattern = " ")
palavras

palavras <- unlist(palavras)
vocabulario <- unique(palavras)
length(palavras)
length(vocabulario)
vocabulario <- vocabulario[str_length(vocabulario) > 1]

# Sorteando e criando o conjunto teste e treinamento
dados <- dados[sample(nrow(dados)),]
str(dados)

n <- round(nrow(dados)*0.8)
treinamento <- dados[1:n,]
teste <- dados[-(1:n),]

vocabulario_treinamento <- str_split(dados$mensagem, pattern = " ")
vocabulario_treinamento <- unique(unlist(vocabulario))
vocabulario_treinamento <- vocabulario[str_length(vocabulario)>1]

treino_spam <- treino[treino$resposta == "spam",]
treino_ham <- treino[treino$resposta == "ham",]

contagem_spam <- rep(0, length(vocabulario_treinamento))

for (i in seq_along(vocabulario_treinamento)) {
  for (j in seq_len(nrow(treino_spam))) {
    mensagem <- unique(unlist(str_split(treino_spam$mensagem[j], " ")))
    mensagem <- mensagem[str_length(mensagem) > 1]
    
    # Verificar se a palavra está na mensagem
    if (vocabulario_treinamento[i] %in% mensagem) {
      contagem_spam[i] <- contagem_spam[i] + 1
    }
  }
}

prob_vocab_spam <- (contagem_spam + 1)/(nrow(treino_spam)+2)
prob_vocab_spam
