# Carregar LIb
library(ggplot2)

# ler dados do  CSV file 
dados <- read.csv("Paises_PIB_ICH.csv")

# filtrar paises  na Europa e Americas
dados_filtrados <- dados[dados$Continent %in% c("Europe", "Americas"), ]

# criar scatter plot com escala logaritmica  para GDP
ggplot(dados_filtrados, aes(x = GDP, y = HCI)) +
  geom_point(aes(color = Continent), size = 2) +  # pintar pontos por continente
  scale_x_log10(breaks = seq(min(dados_filtrados$GDP), max(dados_filtrados$GDP), length.out = 2)) +  # fazer a escala logaritima para eixo xx
  facet_wrap(~Continent) +  # fazer o plot pot continente
  labs(title = "Human Capital Index vs. GDP per capita ", x = "GDP per capita ", y = "Human Capital Index") +
  theme_bw() +
  geom_text(aes(label = Country), data = subset(dados_filtrados, Country %in% c("Lithuania", "Iceland", "United States", "Saint Lucia")), hjust = 1, vjust = 1)  # Nome dos paises 

