# Instala quantmod si no lo tienes
library(quantmod)
library(zoo)

# Vector de sÃ­mbolos
symbols <- c("AAPL", "GOOG", "AMZN", "NFLX")
getSymbols(symbols, src = "yahoo", 
           from = "2025-01-01", to = "2025-04-08")

# Extraer precios ajustados
precios <- list(
  AAPL = AAPL[, "AAPL.Adjusted"],
  GOOG = GOOG[, "GOOG.Adjusted"],
  AMZN = AMZN[, "AMZN.Adjusted"],
  NFLX = NFLX[, "NFLX.Adjusted"]
)

# Unir en un solo objeto xts
precios_merged <- do.call(merge, precios)

par(mfrow = c(2, 2))
for (j in 1:4) {
  plot(zoo(precios_merged[,j]), type = "l", col = "blue",
     ylim = range(precios_merged[,j], na.rm = TRUE),
     main = "Precios ajustados (ENERO A ABRIL, 2025)", 
     ylab = "Precio (USD)", xlab = "Fecha")
    # Agregar el sÃ­mbolo en la esquina superior izquierda del grÃ¡fico
  mtext(symbols[j], side = 3, line = -1, adj = 0, cex = 0.8, col = "black")
  serie <- zoo(precios_merged[, j])  # extrae cada serie ajustada
  por_mes <- split(zoo(serie), format(index(serie), "%Y-%m"))
  n <- length(por_mes)
  n_filas <- ceiling(sqrt(n))  # para acomodar los grÃ¡ficos
  for (i in seq_along(por_mes)) {
    serie_mes <- por_mes[[i]]
    fechas <- as.numeric(index(serie_mes))
    modelo <- lm(coredata(serie_mes) ~ fechas)
    #
    x1 <- min(fechas)
    x2 <- max(fechas)
    y1 <- predict(modelo, newdata = data.frame(fechas = x1))
    y2 <- predict(modelo, newdata = data.frame(fechas = x2))
    # Dibujar solo el segmento entre los extremos del mes
    segments(x1, y1, x2, y2, col = "red", lwd = 2)
  }
}




crear_grupos_edad <- function(edad) {
  cortes <- c(0, 20, 40, 60, Inf)  # Definimos los cortes
  etiquetas <- c("[0,20)", "[20,40)", "[40,60)", "[60,)")
  grupo <- cut(edad,
               breaks = cortes,
               labels = etiquetas,
               right = FALSE,   # Intervalos cerrados a la izquierda [a,b)
               include.lowest = TRUE)  # Para incluir el 0
  grupo
}
est_int_nopar_prom <- function(x, alpha){
  n <- length(x)
  mu_hat <- mean(x)
  sig_hat <- sqrt(sum((x - mu_hat)^2)/n)
  z <- qnorm(1 - alpha/2)  # Cuantil de la normal estÃ¡ndar
  lower <- mu_hat - z*sig_hat/sqrt(n)
  upper <- mu_hat + z*sig_hat/sqrt(n)
  c(lower, upper)
}
est_int_nopar_prop <- function(n, theta_hat, alpha){
  z <- qnorm(1 - alpha/2)  # Cuantil de la normal estÃ¡ndar
  lower <- max(0, theta_hat - z*sqrt(theta_hat*(1 - theta_hat)/n))
  upper <- min(1, theta_hat + z*sqrt(theta_hat*(1 - theta_hat)/n))
  c(lower, upper)
}