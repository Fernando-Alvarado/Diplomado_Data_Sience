my_dir <- "/Users/carloserwin/Library/CloudStorage/GoogleDrive-carloserwin@sigma.iimas.unam.mx/Mi unidad/CURSOS/UNAM/DIPLOMADO/"
library(data.table)
source(paste0(my_dir, "R/extras_ejemplo_5.r"))
BD <- data.frame(fread(paste0(my_dir, "DATOS/COVID19MEXICO.csv")))

################################
# CREACIÃ“N DE VARIABLES NUEVAS #
################################

# DICCIONARIO PARA ETIQUETADO RES PCR
RES_PCR <- data.frame(fread(paste0(my_dir, "DATOS/RES_PCR.csv")))
BD$RES_PCR <- factor(BD$RESULTADO_PCR, levels = RES_PCR$CLAVE, 
                     labels = RES_PCR$DESC)
# 1- no, 2- si
BD$HOSP <-  factor(BD$TIPO_PACIENTE, levels = 1:2, labels = c(0, 1))
BD$GEDAD <- crear_grupos_edad(BD$EDAD)
names(BD)

#########################
# FILTRADO Y ETIQUETADO #
#########################
BD1 <- BD[BD$RES_PCR == "SARS-CoV-2", c(12, 44, 6, 45, 21:31)]



names(BD1)
# ETIQUETADO (1- mujer, 2- hombre)
BD1$SEXO <- factor(BD1$SEXO, levels = 1:2, labels = c(1, 0))
# COMORBILIDADES
for (j in 5:15) { # 1- si, 2- no
  BD1[,j] <- factor(BD1[,j], levels = 1:2, labels = c(1, 0))
}

# Eliminamos renglones con valores faltantes
BD2 <- na.omit(BD1)

names(BD2)
# perdÃ­ 
100*(nrow(BD1) -  nrow(BD2))/nrow(BD1)


# CURVA EPIDÃ‰MICA x DÃA
tmp1 <- table(BD2$FECHA_SINTOMAS)
CASOSxDIA <- data.frame(FECHA = as.Date(names(tmp1)), CASOS = as.numeric(tmp1))

pdf(paste0(my_dir, "GRAF/graf5.pdf"), width = 6, height = 4, pointsize= 9, paper="special")
par(mai=c(0.8,0.6,0.05, 0.05), font = 3) # c(bottom, left, top, right)
plot(CASOSxDIA$FECHA, CASOSxDIA$CASOS, 
     type = "l", lwd = 2, col = "red", 
     axes = FALSE, xlab = "", ylab = "Hospitalizaciones diarias")
mtext("Fecha", side = 1, line = 4)  #
axis.Date(1,
          at = seq(min(CASOSxDIA$FECHA), max(CASOSxDIA$FECHA), by = "15 days"),
          format = "%d-%b-%y",  # dÃ­a-mes-aÃ±o
          las = 2,              # rotar verticalmente
          cex.axis = 0.7)
axis(2, seq(0, 160, by = 16), las = 1)
box(lwd = 2)
dev.off()



# CURVA EPIDÃ‰MICA x MES
BD2$YM <- format(BD2$FECHA_SINTOMAS, "%Y-%m")  # Ejemplo: "2020-03"
tmp2 <- table(BD2$YM)
CASOSxMES <- data.frame(FECHA = as.Date(paste0(names(tmp2), "-01")), CASOS = as.numeric(tmp2))

pdf(paste0(my_dir, "GRAF/graf6.pdf"), width = 6, height = 4, pointsize= 9, paper="special")
par(mai=c(0.8,0.6,0.05, 0.05), font = 3)
plot(CASOSxMES$FECHA, CASOSxMES$CASOS, 
     type = "l", lwd = 2, col = "blue",
     axes = FALSE, xlab = "", ylab = "Hospitalizaciones mensuales")
mtext("Fecha", side = 1, line = 4)
axis.Date(1,
          at = seq(min(CASOSxMES$FECHA), max(CASOSxMES$FECHA), by = "1 month"),
          format = "%b-%Y", las = 2, cex.axis = 0.7)
axis(2, las = 1)
box(lwd = 2)
dev.off()


#
u <- sort(unique(BD2$YM)) # ordeno x mes
k <- length(u)
theta1_hat <- rep(NA, k)
theta2_hat <- rep(NA, k)
interv1 <- array(NA, c(k, 2))
interv2 <- array(NA, c(k, 2))
alpha <- 0.05
for (j in 1:k) {
  # filtro x mes
  id <- BD2$YM == u[j]
  BD_tmp <- BD2[id,]
  tmp <- paste0(BD_tmp$HOSP, BD_tmp$SEXO, BD_tmp$DIABETES)
  tb <- table(tmp)
  nx11 <- sum(tb[c("111", "011")])
  theta1_hat[j] <- tb["111"]/nx11
  nx01 <- sum(tb[c("101", "001")])
  theta2_hat[j] <- tb["101"]/nx01
  interv1[j,] <- est_int_nopar_prop(nx11, theta1_hat[j], alpha)
  interv2[j,] <-est_int_nopar_prop(nx01, theta2_hat[j], alpha)
}

B1 <- data.frame(FECHA = as.Date(paste0(u, "-01")), 
           LOWER = interv1[,1], UPPER = interv1[,2])
B2 <- data.frame(FECHA = as.Date(paste0(u, "-01")), 
           LOWER = interv2[,1], UPPER = interv2[,2])

pdf(paste0(my_dir, "GRAF/graf7.pdf"), width = 6, height = 4, pointsize= 9, paper="special")
par(mai=c(0.8,0.6,0.05, 0.05), font = 3)
plot(c(min(B1$FECHA), max(B1$FECHA)), c(0.2, 1), 
     type = "n", lwd = 2, col = "blue",
     axes = FALSE, xlab = "", ylab = "Probabilidad de hospitalizaciÃ³n")
lines(B1$FECHA, B1$LOWER, col = "pink", lwd = 2)
lines(B1$FECHA, B1$UPPER, col = "pink", lwd = 2)
lines(B2$FECHA, B2$LOWER, col = "blue", lwd = 2)
lines(B2$FECHA, B2$UPPER, col = "blue", lwd = 2)
legend("bottomleft",
       legend = c(expression(P(H == 1 ~ "|" ~ D == 1 ~ "," ~ S == 1)),
                  expression(P(H == 1 ~ "|" ~ D == 1 ~ "," ~ S == 0))),
       col = c("pink", "blue"),
       lwd = 2,
       bty = "n")
mtext("Fecha", side = 1, line = 4)
axis.Date(1,
          at = seq(min(B1$FECHA), max(B1$FECHA), by = "1 month"),
          format = "%b-%Y", las = 2, cex.axis = 0.7)
axis(2, las = 1)
box(lwd = 2)
dev.off()
