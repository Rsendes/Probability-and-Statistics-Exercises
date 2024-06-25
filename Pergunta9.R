set.seed(4588)

# Parâmetros do problema
lambda0 <- 2.40
lambda1 <- 2.65
n <- 100
m <- 5000
k <- 2.623

# Funções auxiliares
generate_sample <- function(lambda, n) {
  rpois(n, lambda)
}

calculate_mean <- function(sample) {
  mean(sample)
}

# Vetores para armazenar os resultados
errors_type_I <- 0
errors_type_II <- 0

# Simulação
for (i in 1:m) {
  # Amostras sob H0
  sample_H0 <- generate_sample(lambda0, n)
  mean_H0 <- calculate_mean(sample_H0)
  
  # Amostras sob H1
  sample_H1 <- generate_sample(lambda1, n)
  mean_H1 <- calculate_mean(sample_H1)
  
  # Teste sob H0
  if (mean_H0 > k) {
    errors_type_I <- errors_type_I + 1
  }
  
  # Teste sob H1
  if (mean_H1 <= k) {
    errors_type_II <- errors_type_II + 1
  }
}

# Frequências relativas dos erros
freq_error_I <- errors_type_I / m
freq_error_II <- errors_type_II / m

# Quociente entre as probabilidades de erro de 2ª espécie e 1ª espécie
quotient <- freq_error_II / freq_error_I

# Resultados
cat("Frequência relativa do erro de 1ª espécie:", freq_error_I, "\n")
cat("Frequência relativa do erro de 2ª espécie:", freq_error_II, "\n")
cat("Quociente entre a probabilidade de erro de 2ª espécie e a probabilidade de erro de 1ª espécie:", quotient, "\n")
