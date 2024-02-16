library(stringr)
x <- "pedro"
str_length(x)
w <- c("abc", "sadhausdhusa")
tamanhos <- str_length(words)
cont <- data.frame(tamanhos)

ggplot(data = cont, mapping = aes(x = tamanhos))+
  geom_bar()

str_c(w,x,sep = " ")

str_sub(x, -3, -1)

p <- "\"amanhã será outro dia.\""
str_view_all(p, "\\.")

teste <- "\"\'\\"
str_view(teste, "\"\'\\\\")

teste2 <- ".a.a.a"
str_view(teste2, "\\..\\..\\..")

str_view(words, "^a")
