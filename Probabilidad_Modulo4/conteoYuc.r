my_dir <- "/Users/carloserwin/Library/CloudStorage/GoogleDrive-carloserwin@sigma.iimas.unam.mx/Mi unidad/CURSOS/UNAM/DIPLOMADO/"
library(data.table)
MUESTRA_REC <- data.frame(fread(paste0(my_dir, "DATOS/REMESAS0231012355.txt"), skip = 1)) 
MUESTRA_REC$NO <- 1:nrow(MUESTRA_REC)
names(MUESTRA_REC)
head(MUESTRA_REC)
MR <- MUESTRA_REC[, c("NO", "MVD", "MSR", "JEZC", "JJDM", "TOTAL")]

library(xtable)
xtable(head(MR), caption = "Fragmento de la base de datos del conteo rÃ¡pido (188 casillas)", label = "tab:conteo_rapido")


MR$PMVD <- 100*MR$MVD/MR$TOTAL
MR$PMSR <- 100*MR$MSR/MR$TOTAL


# Obtener histogramas sin graficar
h1 <- hist(MR$PMVD, 
           breaks = seq(0, 100, length.out = 22), plot = FALSE)
h2 <- hist(MR$PMSR, 
           breaks = seq(0, 100, length.out = 22), plot = FALSE)

# Ajustar lÃ­mites
pdf(paste0("/Users/carloserwin/Library/CloudStorage/GoogleDrive-carloserwin@sigma.iimas.unam.mx/Mi unidad/CURSOS/UNAM/DIPLOMADO/NOTAS/", "graf1.pdf"), width = 5, height = 4, pointsize= 9, paper="special")
par(mai=c(0.6,0.6,0.05, 0.05), font=3) # c(bottom, left, top, right)
my_col = c("indianred", "steelblue")
plot(h1, col = my_col[1], border = "white",
     main = "",
     xlab = "Porcentaje de votos por casilla MVD Vs MSR",
     ylab = "Frecuencia", 
     axes = FALSE, xlim = c(0, 100), ylim = c(0, 45))
# Superponer el de MSR
plot(h2, col = my_col[2], border = "white", add = TRUE)
axis(1, seq(0, 100, by = 10))
axis(2, seq(0, 45, by = 5), las = 2)
# Agregar leyenda
legend("topright", legend = c("P_MVD", "P_MSR"),
       fill = my_col,
       bty = "n", border = "white")
box(lwd = 2)
dev.off()

summary(MR[, c("PMVD", "PMSR")])

tot <- colSums(MR[,-c(1, 7, 8)])
round(100*tot[-5]/tot[5], 1)
