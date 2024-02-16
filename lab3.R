library(stringr)
library(dplyr)

##1
#a
berry <- str_view(fruit,"berry$")
length(berry)
#existem 14 nomes

#b
vogais <- str_view(fruit,"[aeiou]{2}")

#c
c <- str_view(fruit[str_detect(fruit, "^[^\\s]+$")], "[a-z]{6,}")
c

##2
#a
library(babynames)
head(babynames)
tabela <- data.frame(nome = unique(babynames$name))
tabela$vogais <- str_count(tabela$nome, "[aeiouAEIOU]")
tabela$consoantes <- str_count(tabela$nome, "[a-zA-Z]")-str_count(tabela$nome, "[aeiouAEIOU]")
tabela$total <- str_count(tabela$nome, "[a-zA-Z]")

#b
tabela[which.max(tabela$vogais),1]

#c
tabela$prop_vogal <- tabela$vogais/tabela$total
tabela$prop_cons <- tabela$consoantes/tabela$total
tabela[which.max(tabela$prop_vogal),1]
tabela[which.max(tabela$prop_cons),1]

##3
library(stringr)

minusculas <- function(texto) {
  novo_texto <- str_replace_all(texto, 
                                c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e", 
                                  "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j", 
                                  "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o", 
                                  "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t", 
                                  "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y", 
                                  "Z" = "z"))
  return(novo_texto)
}

texto <- "ECSOODJASasadsASDSDASDs dasDASDLFAfasFsffafaF ASFafasfAFS A."
print(minusculas(texto))

##4
news <- readLines("news.txt")
exemplo <- readLines("exemplo.txt")

emails_news <- str_extract(news, "\\b[^ ]+@[^ ]{2,}\\b")
emails_news <- emails_news[!is.na(emails_news)]

emails_exemplo <- str_extract(exemplo, "\\b[^ ]+@[^ ]{2,}\\b")
emails_exemplo <- emails_exemplo[!is.na(emails_exemplo)]

##5 Nessa questão ajustei o que era string para expresões regulares e rodei para identifica-las
teste <- c("teste", "{teste} djsaid", "2022-01-12", "\\\\\\\\", "d.a.s.adadaaada.d.b.bbb.")
str_view(teste, "^.*$") #captura tudo nas linhas
str_view(teste, "\\{.+\\}") #captura o que esta entre chaves
str_view(teste, "\\d{4}-\\d{2}-\\d{2}")#captura a data no formato aaaa-mm-dd
str_view(teste, "\\\\{4}")#captura 4 barras
str_view(teste, "\\..\\..\\..")#pega um padrão ". char . char . char"
str_view(teste, "(.)\\1\\1")#pega 3 caracteres iguais em sequencia
str_view(teste, "(..)\\1")#pega pares que repetem 2 vezes

##6
telefone <- readLines("telefones.txt")

telefones <- str_extract(telefone, "\\(?(\\d{2,3})\\)?[\\s\\-]?\\d{4,5}[\\s\\-]?\\d{4}")
telefones <- telefones[!is.na(telefones)]
telefones
         