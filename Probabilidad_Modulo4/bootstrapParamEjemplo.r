library(bbmle)
######################################################
# DistribuciÃ³n normal con media mu y varianza sigma2 #
######################################################
n <- 100
mu0 <- 5
sigma0 <- 2
xn <- rnorm(n, mean = mu0, sd = sigma0)
hist(xn)

# DefiniciÃ³n de la funciÃ³n de log-verosimilitud negativa
nll <- function(xn, mu, sigma) {
  -sum(dnorm(xn, mean = mu, sd = sigma, log = TRUE))
}

# EstimaciÃ³n por mÃ¡xima verosimilitud
fit <- mle2(nll,
            start = list(mu = 0, sigma = 1),
            data = list(xn = xn),
            method = "L-BFGS-B",
            lower = c(mu = -Inf, sigma = 0.0001))

# Resumen de los resultados
fit
sfit <- summary(fit)

sfit@coef

fit@fullcoef[1]
fit@fullcoef[2]

# soluciÃ³n exacta
mean(xn)
sqrt(sum((xn - mean(xn))^2)/n)

######################################################
#      DistribuciÃ³n gamma con alpha = 3, beta = 2.   #
######################################################

n <- 70
a <- 3
b <- 2
xn <- rgamma(n, shape = a, rate = b)  # rate = beta

# FunciÃ³n de log-verosimilitud negativa, con datos como argumento explÃ­cito
nll_gamma <- function(xn, alpha, beta) {
  if (alpha <= 0 || beta <= 0) return(Inf)
  -sum(dgamma(xn, shape = alpha, rate = beta, log = TRUE))
}

# EstimaciÃ³n por mÃ¡xima verosimilitud con datos explÃ­citos
fitg <- mle2(nll_gamma,
            start = list(alpha = 1, beta = 1),
            data = list(xn = xn),
            method = "L-BFGS-B",
            lower = c(alpha = 0.001, beta = 0.001))

summary(fitg)

fitg@fullcoef[1]
fitg@fullcoef[2]

a
b

#############################################################
#                 DistribuciÃ³n t de Student                 #
# con media mu, varianza sigma^2 y v grados de libertad     #
#############################################################
mu <- 10
sigma <- 2
v <- 5 # v > 2

n <- 300
xn <- rt(n, df = v)*sigma  + mu

# Log-verosimilitud negativa con sigma^2 explÃ­cito
nll_t <- function(xn, mu, sigma2, v) {
  if (sigma2 <= 0 || v <= 0) return(Inf)
  z <- (xn - mu) / sqrt(sigma2)
  -sum(dt(z, df = v, log = TRUE) - 0.5 * log(sigma2))
}

# Estimar parÃ¡metros
fitt <- mle2(nll_t,
            start = list(mu = 1, sigma2 = 1, v = 10),
            data = list(xn = xn),
            method = "L-BFGS-B",
            lower = c(mu = -Inf, sigma2 = 0.0001, nu = 0.001))


fitt@fullcoef[1]
fitt@fullcoef[2]
fitt@fullcoef[3]

######################
# Modelos de mezclas #
######################
library(MASS)
library(mixtools)

y <- sort(galaxies/1000)

hist(y, main = "", border = "white", xlab = "Velocidades de 82 galaxias en 1000 km/s",
     ylab = "Densidad", breaks = seq(7, 36, length.out = 40), axes = FALSE, 
     probability = TRUE, xlim = c(7, 36), ylim = c(0, 0.25))
#lines(density(y, bw = "sj"), col = "blue", lwd = 2)
axis(1, seq(7, 36, by = 2))
axis(2, seq(0, 0.25, by = 0.05), las = 2)
box(lwd = 2)


k <- 6
mu_ini <- quantile(y, probs = seq(0.2, 0.8, length.out = k))  # medias iniciales espaciadas
sigma_ini <- rep((max(y) - min(y))/k, k)                                   # desviaciones estÃ¡ndar iguales
lambda_ini <- rep(1/k, k)                                    # pesos iguales

# Ajuste automÃ¡tico del modelo de mezcla
modelo <- normalmixEM(y,
                      mu = mu_ini,
                      sigma = sigma_ini,
                      lambda = lambda_ini,
                      k = k,
                      maxit = 1000,
                      epsilon = 1e-8,
                      verb = TRUE)
modelo$posterior
modelo$mu
modelo$lambda
modelo$sigma

# Ver nÃºmero de componentes detectados
norm_mix <- function(xstar, modelo){
  mu <- modelo$mu       # medias
  sig <- modelo$sigma   # desviaciones estÃ¡ndar
  w <- modelo$lambda    #
  k <- length(w)
  ystar <- array(0, c(length(xstar), k))
  for (j in 1:k) {
    ystar[,j] <- w[j]*dnorm(xstar, mu[j], sig[j])
  }
  ystar
}

xstar <- seq(7, 36, length.out = 500)
ystar <- norm_mix(xstar, modelo)
# GrÃ¡fica con densidades ajustadas
hist(y, main = "", border = "white", xlab = "Velocidades de 82 galaxias en 1000 km/s",
     ylab = "EstimaciÃ³n de la densidad", breaks = seq(7, 36, length.out = 40), axes = FALSE, 
     probability = TRUE, xlim = c(7, 36), ylim = c(0, 0.25))
ystar2 <- rowSums(ystar)
lines(xstar, ystar2, col = "blue", lwd = 2)
axis(1, seq(7, 36, by = 2))
axis(2, seq(0, 0.25, by = 0.05), las = 2)
box(lwd = 2)
#
hist(y, main = "", border = "white", xlab = "Velocidades de 82 galaxias en 1000 km/s",
     ylab = "EstimaciÃ³n de la densidad de cada grupo", breaks = seq(7, 36, length.out = 40), axes = FALSE, 
     probability = TRUE, xlim = c(7, 36), ylim = c(-0.022, 0.25))
for (j in 1:k) {
  lines(xstar, ystar[,j], col = j, lwd = 2)
}
u <- runif(82, -0.02, -0.001)
z <- apply(modelo$posterior, 1, which.max)
text(y, u, round(y, 0), col = z, cex = 0.8)
axis(1, seq(7, 36, by = 2))
axis(2, seq(0, 0.25, by = 0.05), las = 2)
box(lwd = 2)


################################
# Cargamos el dataset faithful #
################################
data(faithful)
y <- as.matrix(faithful)

# Histograma base para visualizaciÃ³n
plot(y, pch = 19, col = "grey50", xlab = "DuraciÃ³n", ylab = "Espera",
     main = "Datos del Old Faithful Geyser", cex = 1.2)

# NÃºmero de componentes
k <- 2

# Ajuste del modelo de mezcla normal multivariada
modelo <- mvnormalmixEM(y, k = k, maxit = 1000, epsilon = 1e-8, verb = TRUE)

# ClÃºster asignado a cada punto (el de mayor probabilidad posterior)
z <- apply(modelo$posterior, 1, which.max)

# Visualizamos los datos coloreados por componente
plot(y, col = z, pch = 19, xlab = "DuraciÃ³n", ylab = "Espera",
     main = "ClasificaciÃ³n por mezcla de normales multivariada", cex = 1.2)

# Visualizamos las medias
points(t(simplify2array(modelo$mu)), pch = 4, col = k:1, lwd = 3, cex = 2)

##########################################
# Ejemplo mÃ©todo delta con 2 parÃ¡metros  #
##########################################
n <- 10
mu <- 10
sigma <- 3
tau <- sigma/mu
xn <- rnorm(n, mu, sigma)

mu_hat <- mean(xn)
sigma_hat <- sqrt((1/n)*sum((xn - mu_hat)^2))

tau_hat <- sigma_hat/mu_hat


se_delta <- sqrt((1/n)*(1/mu_hat^4 + (sigma_hat^2/(2*mu_hat^2))))

alpha <- 0.05
z <- qnorm(1-alpha/2)
tau_hat + c(-1, 1)*se_delta*z

tau

#########################
# Bootstrap paramÃ©trico #
#########################
B <- 1000
tau_hat_star <- rep(NA, B)
for (b in 1:B) {
  xn_star <- rnorm(n, mu_hat, sigma_hat)
  mu_hat_star <- mean(xn_star)
  sigma_hat_star <- sqrt((1/n)*sum((xn_star - mu_hat_star)^2))
  tau_hat_star[b] <- sigma_hat_star/mu_hat_star
}

hist(tau_hat_star)
quantile(tau_hat_star, probs = c(0.025, 0.5, 0.975))


tau_hat + c(-1, 1)*se_delta*z



