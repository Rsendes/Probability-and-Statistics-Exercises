# Definir parâmetros
n <- 23
r <- 300
m <- 170
set.seed(1950)

# Função para gerar uma amostra de T
generate_T <- function() {
  Z <- rnorm(n + 1)
  T <- sqrt(n) * Z[1] / sqrt(sum(Z[-1]^2))
  return(T)
}

# Gerar r amostras de m valores de T
samples <- replicate(r, replicate(m, generate_T()))

# Calcular a proporção de valores de T <= 1.5 para cada amostra
proportions <- colMeans(samples <= 1.5)

# Calcular a média das proporções
empirical_p <- mean(proportions)

# Calcular p diretamente
direct_p <- pt(1.5, df = n)

# Calcular a diferença
difference <- abs(empirical_p - direct_p)

# Multiplicar por 100 e arredondar
result <- round(difference * 100, 5)

result