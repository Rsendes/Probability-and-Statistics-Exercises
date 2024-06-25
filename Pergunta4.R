#Um sistema é formado por 9 circuitos eléctricos, em que cada um emite sinais codificados no conjunto {1,2,…,10}. Cada circuito emite o sinal i com probabilidade i55, para i∈{1,2,…,10}
#, independentemente dos restantes circuitos. Se pelo menos um dos circuitos emitir o sinal 2 é produzido um aviso sonoro e, caso pelo menos um dos circuitos emita o sinal 1, o sistema é desligado.
#Fixando a semente em 2255
#, simule 150 realizações do estado de um sistema com as características acima descritas e calcule a proporção de vezes em que é produzido um aviso sonoro num sistema que não é desligado. Essa proporção arredondada a 2 casas decimais é igual a:

# Fixando a semente
set.seed(2255)

# Definindo o número de realizações
num_realizacoes <- 150

# Probabilidades associadas a cada sinal
probabilidades <- 1:10 / 55

# Função para simular o estado de um sistema
simular_sistema <- function() {
  sinais <- sample(1:10, 9, replace = TRUE, prob = probabilidades)
  aviso_sonoro <- 2 %in% sinais
  sistema_desligado <- 1 %in% sinais
  return(c(aviso_sonoro, sistema_desligado))
}

# Inicializando contadores
count_aviso_sonoro <- 0
count_sistema_desligado <- 0

# Realizando as simulações
for (i in 1:num_realizacoes) {
  resultado <- simular_sistema()
  if (resultado[2] == FALSE && resultado[1] == TRUE) {
    count_aviso_sonoro <- count_aviso_sonoro + 1
  }
  if (resultado[2] == TRUE) {
    count_sistema_desligado <- count_sistema_desligado + 1
  }
}

# Calculando a proporção
proporcao_aviso_sonoro <- count_aviso_sonoro / (num_realizacoes - count_sistema_desligado)

# Exibindo o resultado
round(proporcao_aviso_sonoro, 3)



