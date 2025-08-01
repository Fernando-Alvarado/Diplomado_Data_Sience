###BrestCancerDiagnosticWisconsin

###Descripción
###El conjunto de datos proporciona información de 569 pacientes sobre 30 características de los núcleos celulares 
###obtenidos de una imagen digitalizada de una aspiración con aguja fina (FNA) de una masa mamaria. 
###Para cada paciente, el cáncer fue diagnosticado como maligno o benigno.

###Format
###A data frame with 569 observations on the following variables:

#ID: ID number

#diagnosis: cancer diagnosis: M = malignant, B = benign

#radius_mean: a numeric vector

#texture_mean: a numeric vector

#perimeter_mean: a numeric vector

#area_mean: a numeric vector

#smoothness_mean: a numeric vector

#compactness_mean: a numeric vector

#concavity_mean: a numeric vector

#nconcave_mean: a numeric vector

#symmetry_mean: a numeric vector

#fractaldim_mean: a numeric vector

#radius_se: a numeric vector

#texture_se: a numeric vector

#perimeter_se: a numeric vector

#area_se: a numeric vector

#smoothness_se: a numeric vector

#compactness_se: a numeric vector

#concavity_se: a numeric vector

#nconcave_se: a numeric vector

#symmetry_se: a numeric vector

#fractaldim_se: a numeric vector

#radius_worst: a numeric vector

#texture_worst: a numeric vector

#perimeter_worst: a numeric vector

#area_worst: a numeric vector

#smoothness_worst: a numeric vector

#compactness_worst: a numeric vector

#concavity_worst: a numeric vector

#nconcave_worst: a numeric vector

#symmetry_worst: a numeric vector

#fractaldim_worst: a numeric vector

###Detalles
###Las características registradas son:

#radius: Media de las distancias desde el centro a los puntos en el perímetro
#texture: Desviación estándar de los valores en escala de grises
#perimeter: Perímetro del núcleo celular
#area: Área del núcleo celular
#smoothness: Variación local en las longitudes de los radios
#compactness: Compacidad del núcleo celular, perímeter^2 / area - 1
#concavity: Severidad de las porciones cóncavas del contorno
#nconcave: Número de porciones cóncavas del contorno
#symmetry: Forma del núcleo celular
#fractaldim: Dimensión fractal, "coastline approximation" - 1

###Para cada característica, los valores registrados se calculan a partir de cada imagen como 
##<caracteristica_name>_media, <caracteristica_name>_se y <caracteristica_name>_worst, para la media, el error estándar y la media de los tres valores más grandes

#df <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", col_names = FALSE)
#df %>% head()

#write.csv(df,"C:/Users/Salvador/Desktop/Cursos2025-II/Diplomado Introducción Analítica a la Ciencia de Datos/wdbc.csv")

###Librerias

library(tidymodels)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
library(psych) # Hace fa
library(ggplot2)
library(broom)
library(Hmisc)#Sirve para poner salida de summary en formato tabla
library(psych)
library(scales)
#library(autoplot)
library(ggplot2)
library(recipes)
library(rgl)
library(plotly)
library(ggforce)
library(tidytext)
library(FactoMineR)  
library(factoextra)
library(ggord)

theme_set(theme_bw(16))

df<-read.csv("C:/Users/ferna/Documents/Diplomado_Data_Sience/Modulo_6/PCA/BreastCancerDiagnosisWisconsin.csv")

df %>% dim()

df %>% glimpse() #Me da una pequeña descricion de los datos por columa y me dice que tipo de datos es y algunos valores

df %>% head()

###Valores faltantes

colSums(is.na(df)) #Checando el numero de NAs

df %>% summary()

###Exploratorio

num.dat = df %>% select_if(is.numeric)

 apply(num.dat,2,function(x) round(summary(x),3)) %>% #Hace una tabla bonita poniendo los cuantiles de los datos
   kbl() %>%
  kable_styling(bootstrap_options = c("striped","hover","bordered")) %>% 
    kable_paper() %>%
  scroll_box(width = "100%", height = "320px")

###Variable de clasificacion: diagnosis

df <- df %>% 
  mutate(diagnosis = relevel(as.factor(diagnosis), "B", "M"))

df  %>% count(diagnosis)

table(df$diagnosis)

df %>% glimpse()

df %>%
  group_by(diagnosis) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n)) %>%
ggplot(aes(df,x = diagnosis, y = n)) +
    geom_col(fill = c("#CC0033", "#e319dc")) +
    geom_text(aes(label = paste0(n, " | ", signif(n / nrow(df) * 100, digits = 4), '%')), nudge_y = 10) + ggtitle("Porcentajes de resultados de biopsia")
    theme_gray()

    
df %>%
  select(where(is.numeric)) %>%
  colMeans() #Selecciono solo las numericas

###

df1 <- df |> dplyr::select(where(is.numeric))

###Histogramas

df1 |> pivot_longer(1:ncol(df1), 
 names_to = "Variable", values_to="Score") |>
   ggplot(aes(x=Score)) + geom_histogram(aes(y = ..density..),bins=20,colour = 3, fill = "darkmagenta") +
     facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal()

# Obs no se analizan las variables predictoras a nivel inferencia, solo nos interesa la respuesta

###box-plot

df1 |> pivot_longer(1:ncol(df1), 
  values_to="Score",names_to = "Variable") |>
    ggplot(aes(y=Score)) + geom_boxplot(aes(fill="darkred"),colour = 3,show.legend = FALSE) +
      facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal()

###Densidad

df1 |> pivot_longer(1:ncol(df1), 
   names_to = "Variable",values_to="Score") |>
     ggplot(aes(x=Score)) + geom_density(aes(fill="darkred"),colour = 3,show.legend = FALSE) +
       facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal()

###Comparacion por la variable de clasificacion o respuesta

df_long <- df %>% 
    pivot_longer(!diagnosis, names_to = "predictores", values_to = "values")

theme_set(theme_light())

df_long %>% 
  ggplot(mapping = aes(x = diagnosis, y = values, fill = predictores)) +
  geom_boxplot() + 
  facet_wrap(~ predictores, scales = "free", ncol = 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  theme(legend.position = "none") +
  labs(title = "Comparación vía box-plot")

df_long |> ggplot(mapping = aes(values, fill = diagnosis)) +
  geom_histogram(color = "white") +
  facet_wrap(~predictores, scales = "free", ncol= 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(title = "Variables Distribution") +
  theme_light()+
  labs(title = "Comparación vía histograma")

df_long |> ggplot(mapping = aes(values, fill = diagnosis)) +
  geom_density(color = "white") +
  facet_wrap(~predictores, scales = "free", ncol= 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(title = "Variables Distribution") +
  theme_light()+
  labs(title = "Comparación a través de densidad")


ggpairs(df, mapping = aes(color = diagnosis),columns = seq(2,11))


###Estructura de correlación

cor_data <- cor(df[, -1])
cor_data

cor1_data <- cor(df[, -1], method = "spearman")
cor1_data

GGally::ggcorr(cor1_data,
               label = TRUE,
               label_alpha = TRUE,
               label_size = 3,
               layout.exp = 1,
               low = "white", mid = "blue", high = "red")


det(cor1_data) # El determinante es muy cercano a cero, 

psych::KMO(cor1_data)

###Prueba de Bartlett
psych::cortest.bartlett(cor1_data, n = dim(df1)[1])# Prueba que la matriz de correlaciones es distinta de la identidad, buscamos rechazarla


library(corrr)
###Todas estas medidas indican que hay una estructura de asociación fuerte
cor.df1 <- df1 %>% cor_mat()
cor.df1

options(scipen = 999)
format(value, scientific = FALSE)
cor.df1 %>% cor_get_pval()

cor.df1 %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

cor.df1 %>% cor_gather() %>% print(n = Inf)

###Confirmando (que es gerundio) que la estructura de correlación es simplemente BESTIAL



#============================================================================================================================================
###PCA
#============================================================================================================================================


pca_fit <- df %>%
  select(where(is.numeric)) %>%
  scale() %>%
  prcomp() # Funcion para hacer pca

str(pca_fit)

pca_fit

pca_fit %>%
  tidy("pcs") %>% print(n=Inf)#Aqui se guardan varias cosas del PCA

pca_fit %>%
  tidy("d")

pca_fit %>%
  tidy(matrix="eigenvalues")

#Los componentes principales, son pesos que le dan nuestras variables


pca_fit %>%
  tidy("pcs") %>%
  ggplot(aes(x=PC, y=percent))+
  geom_col(fill="magenta", alpha=0.7) +
  geom_point(size=2) +
  geom_line(color="darkred", size=1.1)+
  scale_y_continuous(labels=scales::label_percent(),
                     breaks = scales::breaks_pretty(n=6))+
  labs(y= "Varianza explicada", title="Scree plot")

pca_fit %>%
  tidy("pcs") %>%
  ggplot(aes(x=PC, y=cumulative))+
  geom_col(fill="#CC0033", alpha=0.7) +
  geom_point(size=2) +
  geom_line(color="darkviolet", size=1.1)+
  scale_y_continuous(labels=scales::label_percent(),
                     breaks = scales::breaks_pretty(n=6))+
  labs(y= "Varianza explicada acumulada",title="Scree plot")

pca_fit %>%
  augment(df) %>%
  print(n=20)

variance_exp <- pca_fit %>%  
  tidy("pcs") %>% 
  pull(percent)

###Grafica con los dos primeros componentes

pca_fit %>%
  augment(df) %>%
  rename_with(function(x){gsub(".fitted","",x)}) %>%
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp[1]*100), "%"),
       y = paste0("PC2: ",round(variance_exp[2]*100), "%"))+
  labs(title="Gráfica de componentes principales")

pca_fit %>%
  augment(df) %>%
  rename_with(function(x){gsub(".fitted","",x)}) %>%
  ggplot(aes(x = PC1, y = PC2, color=diagnosis))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp[1]*100), "%"),
       y = paste0("PC2: ",round(variance_exp[2]*100), "%"))+
  labs(title="Gráfica de componentes principales")

pca_fit %>%
  augment(df) %>%
  rename_with(function(x){gsub(".fitted","",x)}) %>%
  ggplot(aes(x = diagnosis, y = PC1, color=diagnosis))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitterdodge())+
  labs(y = paste0("PC1: ",round(variance_exp[1]*100), "%"))+
  theme(legend.position = "top")

pca_fit %>%
  augment(df) %>%
  rename_with(function(x){gsub(".fitted","",x)}) %>%
  ggplot(aes(x = diagnosis, y = PC2, color=diagnosis))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitterdodge())+
  labs(y = paste0("PC2: ",round(variance_exp[2]*100), "%"))+
  theme(legend.position = "top")

library(ggfortify)

autoplot(pca_fit, data = df) +
geom_point(alpha = 0.7, size = 2, colour="#e319dc") +
ggtitle("PCA: Biopsias tumores de mama")
theme_minimal()



pca_fit %>%
  augment(df) %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  ) 


###Otra forma


wdbc_recipe <-
  recipe(~., data = df) %>% 
  update_role(diagnosis, new_role = "id") %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>% 
  prep()

wdbc_pca <- 
  wdbc_recipe %>% 
  tidy(id = "pca") 

wdbc_pca

wdbc_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#B53389") + 
  xlim(c(0, 30)) + 
  labs(x="PC", y="% de varianza", title="Scree plot")

wdbc_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "cumulative percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#F25E52") + 
  xlim(c(0, 30)) + 
  labs(x="PC", y="% acumulado de varianza", title="Scree plot")

wdbc_pca %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "¿Positive?"
  ) 


wdbc_pca %>%
  filter(component %in% paste0("PC", 1:6)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

wdbc_pca %>%
  filter(component %in% paste0("PC", 1:6)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "¿Positive?"
  )

juice(wdbc_recipe) %>%
  ggplot(aes(PC1, PC2, label = diagnosis)) +
  geom_point(aes(color = diagnosis), alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)


###3D

# Adicionando los resultados de PCA a la base de datos

df1 <- cbind(df, pca_fit$x)
head(df1)

mycolors <- rainbow(2)
df1$color <- mycolors[as.numeric(df1$diagnosis)]

plot3d( 
  x=df1$PC1, y=df1$PC2, z=df1$PC3, 
  col = df1$color, 
  type = 's', 
  radius = .1,
  xlab="PC1", ylab="PC2", zlab="PC3")

g.df1 <- plot_ly(df1, x = ~PC1, y = ~PC2, z = ~PC3, color = ~diagnosis, colors =c("#0000FF", "#FF00FF") )
g.df1 <- g.df1 %>% add_markers()
g.df1 <- g.df1 %>% layout(scene = list(xaxis = list(title = 'PC1'),
                     yaxis = list(title = 'PC2'),
                     zaxis = list(title = 'PC3')))

g.df1


library(plotly)

df1$diagnosis[which(df1$diagnosis == "B" )] <- "B"
df1$diagnosis[which(df1$diagnosis == "M" )] <- "M"
df1$diagnosis <- as.factor(df1$diagnosis)

fig <- plot_ly(df1, x = ~PC1, y = ~PC2, z = ~PC3, color = ~diagnosis, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                     yaxis = list(title = 'PC2'),
                     zaxis = list(title = 'PC3')))

fig

###EXTRAS

ggpairs(df1, mapping = aes(color = diagnosis),columns = seq(32,36))

# get pca loadings into wider format

pca_wider <- wdbc_pca %>% 
  tidyr::pivot_wider(names_from = component, id_cols = terms)

# define arrow style
arrow_style <- arrow(length = unit(0.001, "inches"),
                     type = "closed")


pca_plot <-
  juice(wdbc_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = diagnosis), 
             alpha = 0.8, 
             size = 1) +
  scale_colour_manual(values = c("darkorange","purple")) 

pca_plot

pca_plot +
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2), 
               x = 0, 
               y = 0, 
               arrow = arrow_style) + 
  geom_text(data = pca_wider,
            aes(x = PC1, y = PC2, label = terms), 
            hjust = 0, 
            vjust = 1,
            size = 3, 
            color = '#0A537D') 

res.pca = PCA(df[,-1],  scale.unit=TRUE) 

fviz_pca_var(res.pca,
             alpha.var = "contrib",
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = 'Influencia de las variables en PCA1 y PCA2',
             repel = TRUE)

fviz_pca_ind(res.pca,
             col.ind = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title='Distribución de los individuos en PCA1 y PCA2',
             repel = TRUE)

fviz_pca_biplot(res.pca, repel = TRUE,
                title='Biplot',
                col.var = "#2E9FDF",
                col.ind = "#696969")

ggord(res.pca, df$diagnosis)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "blue", 
                col.ind = df$diagnosis, 
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                addEllipses = TRUE, ellipse.level = 0.95)
###Prediccion
























