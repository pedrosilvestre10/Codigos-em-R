install.packages("tidyverse")
install.packages("rvest")

library(tidyverse)
library(rvest)
library(stringr)

url <- "https://www.dm.unibo.it/~simoncin/Protein.html"
html <- read_html(url)

# extraindo a tabela

dados <- html |>
          html_elements("body") |>
          html_elements("pre") |>
          html_text()

writeLines(dados)
write.table(dados, file = "dados.txt")

dados1 <- read.table("dados.txt", header = TRUE, sep = "\t")

#imdb

url <- "https://www.imdb.com/chart/toptv/"
html <- read_html(url)

# extraindo a tabela

titulos <- html |>
          html_elements("div.sc-c7e5f54-0") |>
          html_elements("h3.ipc-title__text") |>
          html_text2()

titulos <- str_replace_all(titulos, pattern = "^\\d{1,3}\\.\\s", replacement = "")

titulos

# extraindo a tabela

notas <- html |>
          html_elements("div.sc-c7e5f54-0") |>
          html_elements("span.ipc-rating-star__rating") |>
          html_text2()
