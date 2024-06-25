# Carregar o pacote necessário
if (!require(stats4)) install.packages("stats4")
library(stats4)

# Dados
a <- 4.5
dados <- c(8.54, 4.76, 5.15, 4.96, 6.25, 7.22, 12.9, 6.04, 8.86, 4.88, 6.54, 4.53, 4.7, 5.38, 5.96, 5.17, 5.09, 5.11)

# Definindo a função de verossimilhança
verossimilhanca <- function(theta) {
  if (theta <= 0) return(-Inf)
  n <- length(dados)
  logL <- n * log(theta) - (theta + 1) * sum(log(dados)) + theta * n * log(a)
  return(-logL) # Retorna o negativo porque a função mle minimiza
}

# Encontrar a MLE de theta
mle_result <- mle(minuslogl = verossimilhanca, start = list(theta = 3.4))
theta_hat <- coef(mle_result)
theta_hat

# Calcular o quantil de probabilidade p=0.25 usando a MLE de theta
p <- 0.25
quantil_est <- a / (1 - p)^(1 / theta_hat)
quantil_est

# Verdadeiro valor do quantil quando theta = 3.4
theta_true <- 3.4
quantil_true <- a / (1 - p)^(1 / theta_true)
quantil_true

# Calcular o desvio absoluto
desvio_absoluto <- abs(quantil_est - quantil_true)
round(desvio_absoluto, 4)
