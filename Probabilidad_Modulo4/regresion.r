###########################################
# ComparaciÃ³n de algoritmos de predicciÃ³n #
###########################################
# Caso 1: evaluaciones independientes
theta1 <- 0.6 # 0.3..0.8
theta2 <- 0.7 # 
########################
# H_0: theta1 = theta2 #
########################
n <- 50
m <- 70

x <- rbinom(n, size = 1, theta1)
y <- rbinom(m, size = 1, theta2)

# Estimadores de proporciÃ³n
theta1_hat <- mean(x)
theta2_hat <- mean(y)
delta_hat <- theta1_hat - theta2_hat
##################
# H_0: delta = 0 #
##################


# Error estÃ¡ndar estimado
se_delta_hat <- sqrt(theta1_hat * (1 - theta1_hat) / n + 
                       theta2_hat * (1 - theta2_hat) / m)

# EstadÃ­stico de Wald
W <- delta_hat / se_delta_hat

# p-valor bilateral
p_valor <- 2 * (1 - pnorm(abs(W)))

# Mostrar resultados
cat("Estimador de theta1:", round(theta1_hat, 3), "\n")
cat("Estimador de theta2:", round(theta2_hat, 3), "\n")
cat("Diferencia estimada (delta_hat)):", round(delta_hat, 3), "\n")
cat("Error estÃ¡ndar:", round(se_delta_hat, 3), "\n")
cat("EstadÃ­stico de Wald:", round(W, 3), "\n")
cat("p-valor:", round(p_valor, 4), "\n")

# ConclusiÃ³n al nivel de significancia alpha
alpha <- 0.05
if (p_valor < alpha) {
  cat("ConclusiÃ³n: Se rechaza H0 al nivel de significancia", alpha, "\n")
} else {
  cat("ConclusiÃ³n: No se rechaza H0 al nivel de significancia", alpha, "\n")
}


prop_intervalo <- function(x, alpha){
  n <- length(x)
  theta_hat <- mean(x)
  theta_hat + c(-1, 1)*qnorm(1-alpha/2)*sqrt(theta_hat*(1-theta_hat)/n)
}

alpha <- 0.05
prop_intervalo(x, alpha)
prop_intervalo(y, alpha)

###########################################
# ComparaciÃ³n de algoritmos de predicciÃ³n #
###########################################
# Caso 2: evaluaciones pareadas
n <- 50

# Probabilidades conjuntas
p00 <- 0.10
p01 <- 0.15
p10 <- 0.25
p11 <- 0.50  # total debe ser 1

# Marginales 
theta1 <- p10 + p11
theta2 <- p01 + p11
theta1
theta2

# Correlacion
covXY <- p11 - theta1 * theta2
varX <- theta1 * (1 - theta1)
varY <- theta2 * (1 - theta2)
covXY/sqrt(varX * varY)



probs <- c(p00, p01, p10, p11)
outcomes <- rbind(
  c(0, 0),
  c(0, 1),
  c(1, 0),
  c(1, 1)
)

# Muestreo de pares (X_i, Y_i)
idx <- sample(1:4, size = n, replace = TRUE, prob = probs)
sim <- outcomes[idx, ]
x <- sim[,1]
y <- sim[,2]

D <- x - y
mean(x)
mean(y)


# Estimador de delta
delta_hat <- mean(D)

##################
# H_0: delta = 0 #
##################

# Varianza muestral de D
S_D <- sd(D)
se_delta_hat <- S_D / sqrt(n)

# EstadÃ­stico de Wald
W <- delta_hat / se_delta_hat

# p-valor bilateral
p_valor <- 2 * (1 - pnorm(abs(W)))

# Resultado
cat("Estimador de delta:", round(delta_hat, 3), "\n")
cat("Error estÃ¡ndar:", round(se_delta_hat, 3), "\n")
cat("EstadÃ­stico de Wald:", round(W, 3), "\n")
cat("p-valor:", round(p_valor, 4), "\n")

# ConclusiÃ³n automÃ¡tica
alpha <- 0.05
if (p_valor < alpha) {
  cat("ConclusiÃ³n: Se rechaza H0 al nivel", alpha, "\n")
} else {
  cat("ConclusiÃ³n: No se rechaza H0 al nivel", alpha, "\n")
}