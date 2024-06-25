# carregar os pacotes necessários

library(ggplot2)
library(dplyr)

# Ler o arquivo CSV
data <- read.csv("K:\\explicas\\2016\\UA\\cID\\estatistica\\fwdprojetope\\master.csv")

# Filtrar os dados para o ano de 1986 e grupo etário 25-34 anos
taxa_suicidio <- data %>%
  filter(year == 1986 & age == "25-34 years") 


# Criar o gráfico com ggplot2
ggplot(taxa_suicidio, aes(x = sex, y = suicides.100k.pop, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~ country) +
 
  labs(title = "Suicides per 100,000 Population in 1986 (Age Group: 25-34)",
       x = "Sex",
       y = "Suicides per 100,000 Population")
# Salvar o gráfico 
ggsave("graficoP2.png")