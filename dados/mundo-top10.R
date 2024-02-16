mundo <- read.csv("mundo.csv", header = T)
str(mundo)

ggplot(data = mundo, mapping = aes(x = long,y = lat, color = pop)) + geom_point(alpha = 0.5 , show.legend = F) + theme_void()

ordenacao <- order(mundo$pop, decreasing = T)

top10 <- mundo[ordenacao[1:10],]

#antes
ggplot(data = top10, mapping = aes(x = pop, y = reorder(name, pop))) + 
  geom_bar(stat = "identity")

#depois
ggplot(data = top10, mapping = aes(x = pop, y = reorder(name, pop), fill = country.etc)) + 
  geom_bar(stat = "identity") +
  labs(title = "Top 10 cidades mais populosas do mundo", y = "Cidades", x = "População em milhões", fill = "Paises") +
  scale_x_continuous(
    breaks = c(0, 5e6, 10e6, 15e6),
    labels = c("0", "50", "100", "150")
  ) +
  theme_light()

       