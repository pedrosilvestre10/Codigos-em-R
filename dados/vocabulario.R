# exercícios sobre expressões regulares
library(stringr)

x <- "teste"
x

writeLines(x)

a <- "\""
writeLines(a)

barra <- "\\"
writeLines(barra)

?str_view
teste <- str_view(string = "meu número de telefone é 1234", pattern = "\\d" )
class(teste)

y <- "\\" #string representando uma \
writeLines(y)
str_view(y, "\\\\") #capturando a \ atraves da expressao regular 


str_length(c("a", "ab"))

tamanho <- str_length(words)
table(tamanho)
barplot(table(tamanho))


str_c("abc", "def", sep = " ")
str_c(c("a", "b", "c"), collapse = " ")

str_c(c("a", "b", "c"), c("a", "b", "c"), sep = " ")

str_c(c("a", "b", "c"), "teste", sep = " ")
str_c(c("a", "b", "c"), "teste", collapse = " ")
?str_c


teste <- c("amora", "laranja", "abacaxi")
str_view(teste, "a$")# ancora de fim

str_view(teste, "^a")# ancora de inicio

string <- "\"$^$\""
writeLines(string)
"\$\^\$"
# \"\$\^\$\" como exp regular
str_view(string, "\"\\$\\^\\$\"")
str_view(string, '"\\$\\^\\$"')

#exercicio 5 a

str_view(words,"^y")
str_detect(words, "^y")
words[str_detect(words, "^y")]

#5 b

str_view(words, "x$")
words[str_detect(words, "x$")]

#5 c

str_view(words, "^...$") #palavras de tamanho 3
words[str_detect(words, "^...$")]

str_view(words, "...")
x <- c("ab", "ad", "aef", "abc")
str_view(x, "[ae]") #a ou e

w <- c("abcd", "1254")
str_view(w, "[a-z]")


y <- c("Abc","add", "1254")
str_view(y, "[a-zA-Z]")

str_view(y, "[^a-z]") # ^ = exceto

str_view(words, "^...$")

words[str_detect(words, "^.......")]


a <- c("asgdyasdg", "12 ", "  ")

str_view(a, "\\s")

str_view(words, "[aeiou]")

words[!str_detect(words, "[aeiou]")]

words[str_detect(words, "[^aeiou]+")]

x <- c("gray","grey")
str_view(x, "gr(e|a)y")
a <- "MDCCCLXXXVIII"

str_view(a, "[C]")

str_view(words, "^.{3,}$")

caerio <- readLines("caeiro.txt")

caerio <- caerio[str_length(caerio) > 0]

caerio <- str_to_lower(caerio)

str_view(caerio, "[:punctuation:]")

caerio <- str_replace_all(caerio, "[:punctuation:]", "")

caerio <- str_split(caerio, "\\s")

caerio <- unlist(caerio)

vocabulario <- unique(caerio)

spam <- read.csv("spam.csv")

str_c(spam[96, 2:5], collapse = " ")
