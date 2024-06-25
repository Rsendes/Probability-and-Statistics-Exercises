
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Definir o caminho do arquivo Excel
file_path <- "f:\\explicas\\2016\\UA\\cID\\estatistica\\fwdprojetope\\electricity.xlsx"

# Ler os dados 
data <- read_excel(file_path, sheet = "electricity_production")

# Verificar os nomes das colunas
colnames(data)

# Filtrar dados para renováveis, criar a coluna de data, e filtrar para dados desde 2015
data <- data %>%
  filter(PRODUCT == "Renewables") %>%
  mutate(Date = as.Date(paste(YEAR, MONTH, "01", sep = "-"))) %>%
  filter(Date >= as.Date("2015-01-01"))

# Selecionar e renomear colunas relevantes
data <- data %>%
  select(Date, COUNTRY, VALUE) %>%
  spread(key = COUNTRY, value = VALUE) %>%
  rename(IEA_Total = `IEA Total`, Italy = Italy, Latvia = Latvia)

# Converter colunas para numérico
data <- data %>%
  mutate(IEA_Total = as.numeric(IEA_Total),
         Italy = as.numeric(Italy),
         Latvia = as.numeric(Latvia))

# Calcular a proporção de energia renovável
data <- data %>%
  mutate(IEA_Total_Proportion = IEA_Total / sum(IEA_Total, na.rm = TRUE) * 100,
         Italy_Proportion = Italy / sum(Italy, na.rm = TRUE) * 100,
         Latvia_Proportion = Latvia / sum(Latvia, na.rm = TRUE) * 100)

# Transformar dados para  long format
data_long <- data %>%
  select(Date, IEA_Total_Proportion, Italy_Proportion, Latvia_Proportion) %>%
  gather(key = "Country", value = "Proportion", -Date)

# Criar o gráfico com ggplot2
GRF <- ggplot(data_long, aes(x = Date, y = Proportion, color = Country)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Monthly Proportion of Renewable Energy Production",
       y = "Proportion (%)",
       x = "Year",
       color = "Country")
# gravar o grafico
ggsave("graficoP3.png")