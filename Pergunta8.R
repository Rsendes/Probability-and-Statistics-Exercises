set.seed(1592)
dados <- c(31.8, 31.7, 35.2, 37.1, 31.7, 36.1, 36.3, 33.2, 34.3, 37.5, 30.4, 34.6, 32.4, 31.7, 30.2, 34.3, 35.6, 34.9, 38.9)
amostra <- sample(dados, 12, replace = FALSE)

n <- length(amostra)
gamma <- 0.96
s2 <- var(amostra)
alpha <- 1 - gamma
a <- qchisq((1 - gamma) / 2, df = n - 1)
b <- qchisq((1 + gamma) / 2, df = n - 1)

IC_inf <- (n - 1) * s2 / b
IC_sup <- (n - 1) * s2 / a
IC_original <- c(IC_inf, IC_sup)
amplitude_original <- IC_sup - IC_inf

library(pracma)

equations <- function(x) {
  c <- x[1]
  d <- x[2]
  f1 <- pchisq(d, df = n - 1) - pchisq(c, df = n - 1) - gamma
  f2 <- dchisq(d, df = n + 3) - dchisq(c, df = n + 3)
  return(c(f1, f2))
}

initial_guess <- c(a, b)
solution <- fsolve(equations, initial_guess)
c <- solution$x[1]
d <- solution$x[2]

IC_inf_new <- (n - 1) * s2 / d
IC_sup_new <- (n - 1) * s2 / c
IC_otimizado <- c(IC_inf_new, IC_sup_new)
amplitude_otimizada <- IC_sup_new - IC_inf_new

dif_amplitude <- amplitude_original - amplitude_otimizada
round(dif_amplitude, 4)
