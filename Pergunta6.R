# Definir a semente
set.seed(1973)

# Gerar 1000 amostras da distribuição exponencial
samples <- replicate(1000, rexp(40, 1/4))

# Calcular os valores simulados de Y
Y_simulated <- colSums(samples)

# Calcular a proporção de valores simulados de Y que são maiores do que 126
simulated_prob <- sum(Y_simulated > 126) / 1000

# Calcular o valor exato de P(Y>126) usando a distribuição gama
exact_prob <- pgamma(126, shape=40, rate=1/4, lower.tail = FALSE)

# Calcular a diferença entre os resultados
diff <- abs(simulated_prob - exact_prob) * 100

# Arredondar para 4 casas decimais
diff <- round(diff, 4)

# Imprimir os resultados
cat("Probabilidade simulada: ", simulated_prob, "\n")
cat("Probabilidade exata: ", exact_prob, "\n")
cat("Diferença: ", diff, "\n")