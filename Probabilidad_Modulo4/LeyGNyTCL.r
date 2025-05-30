# Variable aleatoria binomial
# Para reproducibilidad

# ParÃ¡metros
n <- 50
m <- 20
theta <- 0.3

# Generamos variables aleatorias de una binomial
xn <- rbinom(n, size = m, prob = theta)

# Frecuencia relativa (estimaciÃ³n empÃ­rica de la pmf)
tb <- table(factor(xn, levels = 0:m))
pxhat <- tb / sum(tb)

# pmf teÃ³rica
x <- 0:m
px <- dbinom(x, size = m, prob = theta)

# GrÃ¡fica comparativa: fmp empÃ­rica vs teÃ³rica
par(mfrow = c(1, 2))
barplot(pxhat, names.arg = x, main = "fmp empÃ­rica", col = "lightblue", ylim = c(0, max(pxhat, px)))
barplot(px, names.arg = x, main = "fmp teÃ³rica", col = "orange", ylim = c(0, max(pxhat, px)))

# FunciÃ³n de distribuciÃ³n acumulada empÃ­rica
Fxhat <- ecdf(xn)

# CDF teÃ³rica
Fx <- pbinom(x, size = m, prob = theta)

# GrÃ¡fica comparativa: cdf empÃ­rica vs teÃ³rica
par(mfrow = c(1, 1))
plot(Fxhat, verticals = TRUE, do.points = FALSE,
     main = "FunciÃ³n de distribuciÃ³n acumulada", xlab = "x", ylab = "F(x)",
     col = "blue", lwd = 2, xlim = c(0, m))
lines(x, Fx, col = "red", lwd = 2, type = "s")
legend("bottomright", legend = c("EmpÃ­rica", "TeÃ³rica"), col = c("blue", "red"), lwd = 2)


# DistribuciÃ³n normal
n <- 20
mu <- 0
sigma <- 1

# Generamos variables aleatorias normales
xn <- rnorm(n, mean = mu, sd = sigma)

# EvaluaciÃ³n de la densidad teÃ³rica en una malla
x <- seq(min(xn) - 1, max(xn) + 1, length.out = 500)
fx <- dnorm(x, mean = mu, sd = sigma)

# GrÃ¡fica comparativa: fdp empÃ­rica vs teÃ³rica
hist(xn, main = "", col = "lightgray", 
     probability = TRUE, border = "white", 
     xlab = "x", ylab = "f(x)", breaks = seq(min(xn)-1, max(xn)+1, length.out = 20))
lines(x, fx, col = "red", lwd = 2, lty = 2)

# FunciÃ³n de distribuciÃ³n acumulada empÃ­rica
Fxhat <- ecdf(xn)

# CDF teÃ³rica
Fx <- pnorm(x, mean = mu, sd = sigma)

# GrÃ¡fica comparativa: cdf empÃ­rica vs teÃ³rica
plot(Fxhat, verticals = TRUE, do.points = FALSE,
     main = "FunciÃ³n de distribuciÃ³n acumulada", xlab = "x", ylab = "F(x)",
     col = "blue", lwd = 2, xlim = range(x))
lines(x, Fx, col = "red", lwd = 2)
legend("bottomright", legend = c("EmpÃ­rica", "TeÃ³rica"), col = c("blue", "red"), lwd = 2)

####################################
# Ley DÃ©bil de los Grandes NÃºmeros #
####################################

# TamaÃ±o mÃ¡ximo de muestra
n_max <- 1000

# Simula una muestra grande cualquiera de entre varias opciones de v.a.
a <- 6
b <- 5
xn <- rgamma(n_max, a, b)
mu <- a/b

hist(xn)


# Calcula las medias muestrales acumuladas
xbarn <- cumsum(xn) / (1:n_max)

# GrÃ¡fica
plot(1:n_max, xbarn, type = "l", col = "blue", lwd = 2,
     xlab = "TamaÃ±o de la muestra (n)", ylab = "Media muestral",
     main = "Convergencia de la media muestral (LDGN)")
eps <- 0.1
abline(h = mu - eps, col = "black", lty = 2, lwd = 2)  # valor esperado real
abline(h = mu + eps, col = "black", lty = 2, lwd = 2)  # valor esperado real
abline(h = mu, col = "red", lty = 2, lwd = 2)  # valor esperado real
legend("topright", legend = c("Media muestral", "Esperanza teÃ³rica"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

##############################
# Teorema Central del LÃ­mite #
##############################
n <- 100 # tamaÃ±o de muestra
nsim <- 10000 # nÃºmero de simulaciones
theta <- 0.2
theta_hat <- rep(NA, nsim)
# intervalos
inter <- array(NA, c(nsim, 2))
alpha <- 0.05
confianza <- 1- alpha
100*confianza
zq <- qnorm(1 - alpha/2)
for (i in 1:nsim) {
  xn <- rbinom(n, 1, prob = theta) # bernoulli(theta)
  theta_hat[i] <- mean(xn)
  # intervalos de confianza
  se_hat <- sqrt(theta_hat[i]*(1-theta_hat[i])/n)
  inter[i,1] <- max(theta_hat[i] - zq*se_hat, 0)
  inter[i,2] <- min(theta_hat[i] + zq*se_hat, 1)
}

mu <- theta
se <- sqrt(theta*(1-theta)/n)
a <- min(qnorm(0.001, mu, se), min(theta_hat)) - 0.01
b <- max(qnorm(0.999, mu, se), max(theta_hat)) + 0.01
x <- seq(a, b, length.out = 1000)
y <- dnorm(x, mu, se)
hist(theta_hat, xlim = c(a, b), main = "", xlab = "Medias", ylab = "Estimador de densidad",
     breaks = seq(min(theta_hat) - 0.01, max(theta_hat) + 0.01, length.out = 20), probability = TRUE)
abline(v = 0, lwd = 2, lty = 2, col="red")
lines(x, y, col = "blue", lwd = 2)

cobertura <- 100*sum(inter[,1] <= theta & theta <= inter[,2])/nsim
cobertura
