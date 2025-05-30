my_dir <- "/Users/carloserwin/Library/CloudStorage/GoogleDrive-carloserwin@sigma.iimas.unam.mx/Mi unidad/CURSOS/UNAM/DIPLOMADO/"
library(data.table)
MUESTRA_REC <- data.frame(fread(paste0(my_dir, "DATOS/REMESAS0231012355.txt"), skip = 1)) 
names(MUESTRA_REC)
head(MUESTRA_REC)
Xn <- MUESTRA_REC[, c("MVD", "MSR", "JEZC", "JJDM", "CNR", "NULOS", "TOTAL")]

n <- nrow(Xn)
k <- ncol(Xn) - 1
B <- 1000

# Bootstrap
pn <- array(NA, c(B, k))
for (l in 1:B) {
  id <- sample.int(n, size = n, replace = TRUE)
  Xn_star <- Xn[id, ]
  Xn_star_bar <- colMeans(Xn_star)
  pn[l,] <- 100*Xn_star_bar[1:k]/Xn_star_bar[k+1]
}
rowSums(pn)

res <- array(NA, c(k, 3))
for (j in 1:k) {
  res[j,] <- quantile(pn[,j], prob = c(0.05, 0.5, 0.975), type = 8)
}
row.names(res) <- colnames(Xn)[1:k]
colnames(res) <- c("INF", "MED", "SUP")
res

# Â¿REALIZAMOS UNA BUENA ESTIMACIÃ“N?
# CARGAMOS LA BASE DE DATOS DE LOS CD
POB <- read.csv(paste0(my_dir, "DATOS/2018_SEE_GOB_YUC_CAS.csv"))
head(POB)
POB$MVD <- POB$PAN + POB$PAN_MC + POB$MC
POB$MSR <- POB$PRI + POB$PVEM + POB$NA. + POB$PRI_PVEM + POB$PRI_NA + POB$PVEM_NA + POB$PRI_PVEM_NA
POB$JEZC <- POB$PRD
POB$JJDM <- POB$PT + POB$MORENA + POB$ES + POB$PT_MORENA + POB$PT_ES + POB$MORENA_ES + POB$PT_MORENA_ES
POB$CNR <- POB$NUM_VOTOS_CAN_NREG
POB$NULOS <- POB$NUM_VOTOS_NULOS
POB$TOTAL <- POB$TOTAL_VOTOS

XN <- POB[, c("MVD", "MSR", "JEZC", "JJDM", "CNR", "NULOS", "TOTAL")]

# CONFIRMANDO
sum(abs(rowSums(XN[-(k+1)]) - XN$TOTAL))

# Porcentajes poblacionales
XN_bar <- colMeans(XN)
pN <- 100*XN_bar[1:k]/XN_bar[k+1]

# ESTIMACION Vs COMPUTOS DISTRITALES
cbind(round(res, 2), pN = round(pN, 2))
