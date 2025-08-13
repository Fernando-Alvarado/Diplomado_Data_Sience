###PCA_WDBCtidymodels1

library(tidyverse)
library(tidymodels)
library(modeldata)
library(embed)
library(tsne)
library(uwot)
library(scattermore)
library(readr)
library(Rtsne)
library(gganimate)
library(data.table)


df<-read.csv("C:/Users/ferna/Documents/Diplomado_Data_Sience/Modulo_6/PCA/BreastCancerDiagnosisWisconsin.csv")

df %>% dim()

df %>% glimpse()

df %>% head()

###t-sne

###Sin proponer hiper-parametros, i.e., con los hiper-parametros de default

set.seed(123)

df_tsne <- df %>% # Asi se deben trabajar las variables discretos 
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  tsne()#Te da las epocas del procesos y su error

str(df_tsne)

head(df_tsne)

# Adicionando los resultados de  t-SNE a la base -------------------------- pegamos la variable de identificacion 

datos<-data.frame(tSNE1=df_tsne[,1],tSNE2=df_tsne[,2],diagnostico=df$diagnosis) #Agarramos los componenetes de tSNE y los pegamos a la base original
head(datos)

ggplot(datos,aes(x=tSNE1,y=tSNE2,color=diagnostico))+
geom_point(size=1.5)+
labs(title="WDBC: t-sne")

###Los argumentos de esta funcion son:

###k: the dimension of the resulting embedding

###initial_dims: The number of dimensions to use in reduction method.

###perplexity: Perplexity parameter. (optimal number of neighbors)

###max_iter: Maximum number of iterations to perform.

###min_cost: The minimum cost value (error) to halt iteration.

###epoch_callback: A callback function used after each epoch (an epoch here means a set number of iterations)
###                (Una función de devolución de llamada utilizada después de cada época (una época aquí significa un número determinado de iteraciones))

###epoch: The number of iterations in between update messages.

###Se puede correr con una selección arbitraria de los parámetros más importantes que son: perplexity, max_iter y min_cost, en ese orden

###Otra funcion para hacerlo

set.seed(123)

df_Rtsne <- df %>% #  Otro algoritmo mas rapido 
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  Rtsne() 

str(df_Rtsne)

datos1<-data.frame(Rtsne1=df_Rtsne$Y[,1],Rtsne2=df_Rtsne$Y[,2],diagnostico=df$diagnosis)

ggplot(datos1,aes(x=Rtsne1,y=Rtsne2,color=diagnostico))+
geom_point(size=1.5)+
labs(title="WDBC: Rtsne")

###Con ambas funciones deberíamos de explorar con varios valores de los hiper-parametros 

###Exploración de parametros tsne

set.seed(123)

tsne_params <-  expand.grid(perplexity=c(10,15,20,25,30,50)) ###Explorando parametro de perplejidad: perplexity. max_iter fijo 
	
tsne_res <- lapply(seq(nrow(tsne_params)), function(i) {
	print(i)
	res <- tsne::tsne(
		X = df[,-1],
            max_iter = 500,
		perplexity = tsne_params[[1]][i]
		
	)
	
	return(res)
})

library(data.table)
d1 <- rbindlist(lapply(seq(nrow(tsne_params)), function(i) {
	data.table(
		x = tsne_res[[i]][,1],
		y = tsne_res[[i]][,2],
		perplexity = tsne_params[[1]][i],
		group = df$diagnosis
	)
}))
												  

p1 <- ggplot(d1) +
	geom_scattermore(
		mapping = aes(x = x, y = y,colour=group),
		pointsize = 2
	) +
	theme(
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		axis.title = element_blank(),
		legend.position = "none"
	) +
	facet_wrap("perplexity" , 
		labeller = label_both,
		scales = "free")
p1 <- p1 + theme_minimal() +
	theme(legend.position = "none")
p1

###Rtsne exploracion de parametros

set.seed(123)


Rtsne_params <-  expand.grid(perplexity=c(10,15,20,25,30,50))# Paquete mas rapdio 
	
Rtsne_res <- lapply(seq(nrow(Rtsne_params)), function(i) {
	print(i)
	Rres <- Rtsne::Rtsne(
		X = df[,-1],
            max_iter = 500,
            verbose=TRUE,
            perplexity = Rtsne_params[[1]][i],
            check_duplicates = FALSE
		
	)
	
	return(Rres)
})


d2 <- rbindlist(lapply(seq(nrow(Rtsne_params)), function(i) {
	data.table(
		x = Rtsne_res[[i]]$Y[,1],
		y = Rtsne_res[[i]]$Y[,2],
		perplexity = Rtsne_params[[1]][i],
		group = df$diagnosis
	)
}))
												  
p2 <- ggplot(d2) +
	geom_scattermore(
		mapping = aes(x = x, y = y,colour=group),
		pointsize = 2
	) +
	theme(
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		axis.title = element_blank(),
		legend.position = "none"
	) +
	facet_wrap("perplexity" , 
		labeller = label_both,
		scales = "free")
p2 <- p2 + theme_minimal() +
	theme(legend.position = "none")
p2

###tsne es muy lento. Para juzgar cómo funciona este algoritmo haciedo una exploracion con dos
###parametros, usaremo Rtsne

set.seed(123)
Rtsne_params2 = expand.grid(perplexity=c(10,15,20,25,30), eta = c(10, 50, 100, 150))  ###eta: tasa de aprendizaje

Rtsne_res2 = lapply(seq(nrow(Rtsne_params2)), function(i) {
  Rres = Rtsne(
    X = df[,-1],
    max_iter = 500,
    verbose=TRUE,
    perplexity = Rtsne_params2$perplexity[i],
    check_duplicates = FALSE,
    eta = Rtsne_params2$eta[i],
    pca = F
  )
  return(Rres)
})

d3 = rbindlist(lapply(seq(nrow(Rtsne_params2)), function(i) {
  data.table(
    x = Rtsne_res2[[i]]$Y[,1],
    y = Rtsne_res2[[i]]$Y[,2],
    perplexity = Rtsne_params2[[1]][i],
    eta = Rtsne_params2[[2]][i],
    group = df$diagnosis
  )
}))


p3<-ggplot(d3) +
  geom_scattermore(
    mapping = aes(x = x, y = y ,colour=group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(eta~perplexity ,
             labeller = label_both,
             scales = "free",
             ncol = 5) +
  theme_bw() 

p3

###

set.seed(123)
Rtsne_params3 = expand.grid(perplexity=c(10,15,20,30), eta = c(10, 50, 100, 150, 200))  ###eta: tasa de aprendizaje

Rtsne_res3 = lapply(seq(nrow(Rtsne_params3)), function(i) {
  Rres = Rtsne(
    X = df[,-1],
    max_iter = 500,
    verbose=TRUE,
    perplexity = Rtsne_params3$perplexity[i],
    check_duplicates = FALSE,
    eta = Rtsne_params3$eta[i],
    pca = F
  )
  return(Rres)
})

d4 = rbindlist(lapply(seq(nrow(Rtsne_params3)), function(i) {
  data.table(
    x = Rtsne_res3[[i]]$Y[,1],
    y = Rtsne_res3[[i]]$Y[,2],
    perplexity = Rtsne_params3[[1]][i],
    eta = Rtsne_params3[[2]][i],
    group = df$diagnosis
  )
}))


p4<-ggplot(d4) +
  geom_scattermore(
    mapping = aes(x = x, y = y ,colour=group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(eta~perplexity ,
             labeller = label_both,
             scales = "free",
             ncol = 5) +
  theme_bw() 

p4


colores = c('#E178C5','#EB5B00')
names(colores) = c("B","M")

anim_plot1<-d4 %>% 
  filter(perplexity == 10 | perplexity == 15 | perplexity == 20 | perplexity == 30) %>% 
  mutate(parametros = factor(paste('perplexity:', perplexity, 'eta:', eta))) %>% ggplot() +
  geom_scattermore(
    mapping = aes(x = x, y = y, col = group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw()+
  labs(title = 'Verificación de los parámetros')+
  scale_color_manual(values = colores) + 
  transition_states(parametros, transition_length = 2, state_length = 3) +
  labs(title = '{closest_state}')

animate(anim_plot1, nframes = 300)



anim_plot2<-d4 %>% 
  filter(eta == 10 | eta == 50 | eta == 100 | eta == 150 | eta == 200) %>% 
  mutate(parametros = factor(paste('perplexity:', perplexity, 'eta:', eta))) %>% ggplot() +
  geom_scattermore(
    mapping = aes(x = x, y = y, col = group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw()+
  labs(title = 'Verificación de los parámetros')+
  scale_color_manual(values = colores) + 
  transition_states(parametros,transition_length = 3, state_length = 1) +
  labs(title = '{closest_state}')

animate(anim_plot2, nframes = 300)

###Despues, se tendria que decidir que seleccion de estos parametros es la que mejor representa
###nuestros datos y correr un modelo con ellos

###Por ejemplo perplexity=30 y eta=200

set.seed(123)

df_RtsneF <- df %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  Rtsne(perplexity=30, eta=200) 


datos2<-data.frame(Rtsne1=df_RtsneF$Y[,1],Rtsne2=df_RtsneF$Y[,2],diagnostico=df$diagnosis)

win.graph()
ggplot(datos2,aes(x=Rtsne1,y=Rtsne2,color=diagnostico))+
geom_point(size=1.5)+
labs(title="WDBC: Rtsne")

###O bien

final_plot<-d4 %>% 
  filter(perplexity==30, eta == 200) %>% 
  mutate(parametros = factor(paste('perplexity:', perplexity, 'eta:', eta))) %>% ggplot() +
  geom_scattermore(
    mapping = aes(x = x, y = y, col = group),
    pointsize = 1.1
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw()+
  labs(title = 'tSNE: Modelo final')

win.graph()
final_plot


final_plot1<-d4 %>% filter(perplexity==30 & eta == 200) %>% 
  ggplot(aes(x = x, y = y, col = group)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = colores) +
  labs(title = 'tSNE: Modelo final',
       subtitle = 'perplexity: 30 and eta: 200')

win.graph()
final_plot1




















