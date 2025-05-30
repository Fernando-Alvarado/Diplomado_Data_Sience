library(MASS)
my_dir <- "/Users/carloserwin/Library/CloudStorage/GoogleDrive-carloserwin@sigma.iimas.unam.mx/Mi unidad/CURSOS/UNAM/DIPLOMADO/"
y <- sort(galaxies/1000)
summary(y)

pdf(paste0(my_dir, "NOTAS/graf2.pdf"), width = 5, height = 4, pointsize= 9, paper="special")
par(mai=c(0.6,0.6,0.05, 0.05), font=3) # c(bottom, left, top, right)
hist(y, main = "", border = "white", xlab = "Velocidades de 82 galaxias en 1000 km/s",
     ylab = "Densidad", breaks = seq(7, 36, length.out = 40), axes = FALSE, 
     probability = TRUE, xlim = c(7, 36), ylim = c(0, 0.25))
#lines(density(y, bw = "sj"), col = "blue", lwd = 2)
axis(1, seq(7, 36, by = 2))
axis(2, seq(0, 0.25, by = 0.05), las = 2)
box(lwd = 2)
dev.off()

# ClÃºstering jerÃ¡rquico
# Datos ordenados
hc <- hclust(dist(y), method = "complete")

# Crear una paleta para los rectÃ¡ngulos (opcional)
cols <- c("steelblue", "darkorange", "forestgreen")

# Guardar como PDF (opcional)
# Datos
y <- sort(galaxies / 1000)
hc <- hclust(dist(y), method = "complete")
# Guardar como PDF
pdf(paste0(my_dir, "NOTAS/graf3.pdf"), width = 10, height = 5)
par(mai=c(0.8,0.6,0.05, 0.05), font=3) # c(bottom, left, top, right)
plot(hc,
     labels = FALSE,
     hang = -0.1,
     main = "",
     xlab = "Ãndice de la observaciÃ³n",
     ylab = "Distancia", 
     sub = "",
     ylim = c(-2.2, 30))
# Posiciones horizontales de los labels
n <- length(hc$order)
x_pos <- 1:n
y_pos <- rep(0, n)
# Etiquetas redondeadas (valores de velocidad)
etiquetas <- round(y[hc$order], 1)
text(x = x_pos, y = y_pos + runif(n, -2, -0.1),
     labels = x_pos,
     srt = 0,              # RotaciÃ³n 90 grados = vertical
     adj = 1,               # Alinea al centro inferior
     xpd = TRUE,            # Permite escribir fuera del plot
     cex = 0.6)             # TamaÃ±o del texto

# Opcional: rectÃ¡ngulos de grupos
rect.hclust(hc, k = 4,
            border = c("steelblue", "darkorange", "forestgreen", "red"))
box(lwd = 2)
dev.off()


# Instalar si no la tienes
# install.packages("mixtools")

# Cargar la librerÃ­a
library(mixtools)

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
pdf(paste0(my_dir, "NOTAS/graf4.pdf"), width = 11, height = 4, pointsize= 9, paper="special")
par(mfrow = c(1, 2), mai=c(0.6,0.6,0.05, 0.05), font=3) # c(bottom, left, top, right)
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
dev.off()