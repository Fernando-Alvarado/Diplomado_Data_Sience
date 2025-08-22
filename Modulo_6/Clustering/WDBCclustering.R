###WDBCclustering

library(tidymodels)
library(tidyverse)
library(tidyclust)
library(factoextra)
library(FactoMineR)
library(cluster)
library(mlr)
library(GGally)
library(ClusterR)
library(vegan)
library(NbClust)
library(gridExtra)
library(grid)
library(lattice)
require(igraph)

###Mi mismo conjunto de datos: Brest Cancer Diagnostic Wisconsin (wdbc)

df<-read.csv("C:/Users/Salvador/Desktop/Cursos2025-II/Diplomado Introducción Analítica a la Ciencia de Datos/BreastCancerDiagnosisWisconsin.csv")

df %>% dim()

df %>% glimpse()

df %>% head()

df %>% summary()

###Cambiando la escala de medicion de la variable de clasificacion,diagnosis, de caracter (chr) a factor (fct) 

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

###Clusters: Metodo jerarquico

###Explorando las ligas

df1<- df %>% dplyr::select(-diagnosis)

head(df1)

###Calculando distancias (distancia euclideana)

dist_df <- dist(scale(df1), method = "euclidean")

hc_ave <- hclust(dist_df, method = "average")
hc_single<-hclust(dist_df, method = "single")
hc_comp<-hclust(dist_df, method = "complete")
hc_ward<-hclust(dist_df, method = "ward.D")
hc_ward2<-hclust(dist_df, method = "ward.D2")

###Distancia cophenetic

d_ave <- cophenetic(hc_ave)
d_single <- cophenetic(hc_single)
d_comp<- cophenetic(hc_comp)
d_ward<- cophenetic(hc_ward)
d_ward2<-cophenetic(hc_ward2)

cbind(cor(dist_df,d_ave),cor(dist_df,d_single),cor(dist_df,d_comp),cor(dist_df,d_ward),cor(dist_df,d_ward2))

plot(hc_ave)

###"OH, MI DIOS"

plot(hc_single)

###Que es esto

###Una que generalmente funciona "mas mejor" es la de ward

plot(hc_ward)

plot(hc_ward2)

###Con la primer liga, si se corta un poco despues de 150 se pueden visualizar tres grupos; en la segunda si se corta alrededpr de 40
###también se observan tres grupos

fviz_dend(hc_ward, cex = 0.65, k = 3, color_labels_by_k = FALSE, rect = TRUE, rect_fill = TRUE, k_colors = "jco", rect_border = "jco", show_labels = F, main="Cluster: Liga ward dendrograma (3 grupos)")

fviz_dend(hc_ward2, cex = 0.65, k = 3, color_labels_by_k = FALSE, rect = TRUE, rect_fill = TRUE, k_colors = "jco", rect_border = "jco", show_labels = F, main="Cluster: Liga ward2 dendrograma (3 grupos)")

fviz_dend(hc_ward, cex = 0.8, k = 3,k_colors =c("#FC4E07","#a8a632", "#00AFBB") ,type = "rectangle")

fviz_dend(hc_ward, cex = 0.8, k = 3,k_colors =c("#FC4E07","#a8a632", "#00AFBB") ,type = "circular")

fviz_dend(hc_ward, cex = 0.8, k = 3,k_colors =c("#FC4E07","#a8a632", "#00AFBB") ,type = "phylogenic")

fviz_dend(hc_ward, cex = 0.8, k = 3,k_colors =c("#FC4E07","#a8a632", "#00AFBB") ,type = "phylogenic", repel=TRUE)


###Con otro metodo aglomerativo: agnes

df1<-as.data.frame(scale(df1))

ac_metric <- list(
  complete_ac = agnes(df1, metric = "euclidean", method = "complete")$ac,
  average_ac = agnes(df1, metric = "euclidean", method = "average")$ac,
  single_ac = agnes(df1, metric = "euclidean", method = "single")$ac,
  ward_ac = agnes(df1, metric = "euclidean", method = "ward")$ac
)

ac_metric

plot(agnes(df1, metric = "euclidean", method = "ward"))

###Aparentemente misma agrupacion que con la liga ward

###Con un metodo divisivo: diana

plot(diana(df1, metric = "euclidean"))

###Nada claro

###Puede observarse que el metodo jerarquico tiene muchos inconvenientes, y es enormemente subjetivo
###Además, si la cantidad de observaciones es grande, el dendrograma dificilmente ayudaria a identificar agrupaciones

###Con tidymodels

hc_spec <- hier_clust(
  linkage_method = "average" )%>%   ###Aquí pueden poner cualquiera de las ligas
  set_engine("stats")

hc_spec

hclust_fit <- tidyclust::fit(hc_spec, ~., df1)

hclust_fit 

hclust_fit$fit %>% plot()

###Sin declarar liga

hclust_spec <- hier_clust() %>%
               set_engine("stats")

hclust_fit1 <- tidyclust::fit(hclust_spec, ~., df1)

hclust_fit1 %>%
  extract_centroids(num_clusters = 3)%>%print(Inf)

hclust_fit1$fit %>% plot()


###Declarando ligas

res_hclust_complete <- hier_clust(linkage_method = "complete") %>%
  tidyclust::fit(~., data = df1)

res_hclust_complete$fit %>% plot()

res_hclust_average <- hier_clust(linkage_method = "average") %>%
  tidyclust::fit(~., data = df1)

res_hclust_average$fit %>% plot()

res_hclust_single <- hier_clust(linkage_method = "single") %>%
  tidyclust::fit(~., data = df1)

res_hclust_single$fit %>% plot()

res_hclust_ward <- hier_clust(linkage_method = "ward.D") %>%
  tidyclust::fit(~., data = df1)

res_hclust_ward$fit %>% plot()

res_hclust_ward2 <- hier_clust(linkage_method = "ward.D2") %>%
  tidyclust::fit(~., data = df1)

res_hclust_ward2$fit %>% plot()


###La alternativa K-means

###Exploracion del numero de grupos (kMedias)

ggpairs(df1[,1:10],
upper = list(continuous = "density"),
lower = list(continuous = wrap("points", size = 0.5)),
diag = list(continuous = "densityDiag")) +
theme_bw()

set.seed(123)

dfTask <- makeClusterTask(data = df1)
listLearners("cluster")$class

kMeans <- makeLearner("cluster.kmeans",par.vals = list(iter.max = 2500, nstart = 25))
kMeans

kMeansParamSpace <- makeParamSet(
makeDiscreteParam("centers", values = 2:6),
makeDiscreteParam("algorithm",
values = c("Hartigan-Wong", "Lloyd", "MacQueen")))
gridSearch <- makeTuneControlGrid()
kFold <- makeResampleDesc("CV", iters = 25)

set.seed(123)

library(clusterSim)

tunedK <- tuneParams(kMeans, task = dfTask,
resampling = kFold,
par.set = kMeansParamSpace,
control = gridSearch,
measures = list(db, G1))

tunedK

set.seed(123)

kMeansTuningData <- generateHyperParsEffectData(tunedK)
kMeansTuningData$data
gatheredTuningData <- gather(kMeansTuningData$data,
key = "Metric",
value = "Value",
c(-centers, -iteration, -algorithm))

ggplot(gatheredTuningData, aes(centers, Value, col = algorithm)) +
facet_wrap(~ Metric, scales = "free_y") +
geom_line() +
geom_point() +
theme_bw()

###El índice db captura la intuicion de que los clusters que estan (1) bien espaciados entre si y 
###(2) son muy densos son probablemente un "buen" agrupamiento. Esto se debe a que la declaracion "max" 
###de la medida selecciona repetidamente los valores donde el punto promedio esta mas alejado de su centroide y 
###donde los centroides estan mas cerca entre si. A medida que el indice db se reduce, el agrupamiento se considera "mejor".

###El indice dunn (exec.time) captura la misma idea que el indice db: mejora cuando los clusters estan bien espaciados y son densos. 
###Pero el indice dunn aumenta a medida que mejora el rendimiento.

###Lo que difiere es la forma en que se aborda este problema. Mientras que el indice db considera la dispersion y 
###separacion de todos los clusteres, el índice dunn solo considera los peores casos en el agrupamiento: los clústeres que 
###estan mas cerca entre si y el cluster mas disperso. Dependiendo de su aplicacion, el cambio en el objetivo puede introducir problemas inesperados.

###Un valor más alto de G1 indica una mejor agrupación, porque significa que los puntos de datos están más dispersos entre los grupos que dentro de ellos.

set.seed(123)

tunedKMeans <- setHyperPars(kMeans, par.vals = tunedK$x)
tunedKMeansModel <- train(tunedKMeans, dfTask)
kMeansModelData <- getLearnerModel(tunedKMeansModel)
kMeansModelData$iter

tunedKMeans

df1 <- mutate(df1,
kMeansCluster = as.factor(kMeansModelData$cluster))

table(df1$kMeansCluster)

ggpairs(df1[,c(1:10,31)], aes(col = kMeansCluster),upper = list(continuous = "density")) +
theme_bw()

newData <- tibble(df1[32:35,])

predict(tunedKMeansModel, newdata = newData)

###3 clusters

###Otra forma de explorar (kMeans)

rec_df <- recipe(~.,data = df) %>%
  update_role(diagnosis, new_role = "id") %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) 

rec_df

kmeans_spec <- k_means(num_clusters = tune())

kmeans_wf <- workflow(rec_df, kmeans_spec)

kmeans_wf <- kmeans_wf %>% 
  update_model(kmeans_spec)

grid <- tibble(num_clusters = 1:10)

set.seed(123)
boots <- bootstraps(df, times = 10)

res <- tune_cluster(
  kmeans_wf,
  resamples = boots,
  grid = grid,
  metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio)
)

res_metrics <- collect_metrics(res)%>% print(n=Inf)

best <- res %>%
  select_best(metric="sse_ratio")
best

res_metrics %>%
  filter(.metric == "sse_ratio") %>%
  ggplot(aes(x = num_clusters, y = mean)) +
  geom_point(col="darkblue",size=2) +
  geom_line(col="red") +
  theme_minimal() +
  ylab("mean WSS/TSS ratio") +
  xlab("Número de clusters") +
  scale_x_continuous(breaks = 1:10)


###Validacion cruzada

df_cv <- vfold_cv(df, v = 10)

clust_num_grid <- grid_regular(num_clusters(),levels = 10)

clust_num_grid

res1 <- tune_cluster(
  kmeans_wf,
  resamples = df_cv,
  grid = clust_num_grid,
  control = control_grid(save_pred = TRUE, extract = identity),
  metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio)
)


res1_metrics <- res1 %>% collect_metrics()%>% print(n=Inf)


best1 <- res1 %>%
  select_best(metric="sse_ratio")
best1


res1_metrics %>%
  filter(.metric == "sse_ratio") %>%
  ggplot(aes(x = num_clusters, y = mean)) +
  geom_point(col="darkblue",size=2) +
  geom_line(col="red") +
  theme_minimal() +
  ylab("mean WSS/TSS ratio cv") +
  xlab("Number of clusters") +
  scale_x_continuous(breaks = 1:10)


###Explorando el numero de clustes K subyacentes a estos datos con otros metodos

df1<-as.data.frame(scale(df1[,-31]))

fviz_nbclust(df1, kmeans, method = "wss")+labs(x ="Número de clusters")+labs(y="Total suma de cuadrados intra clusters")+labs(title = "Número óptimo de clusters")

opt<-Optimal_Clusters_KMeans(df1, max_clusters=10,plot_clusters = TRUE,criterion="WCSSE")

fviz_nbclust(df1, kmeans, method = "silhouette")+labs(x ="Número de clusters")+labs(y="Promedio de silueta")+labs(title = "Número óptimo de clusters")

opt1<-Optimal_Clusters_KMeans(df1, max_clusters=10, plot_clusters = TRUE, criterion="silhouette")

opt2<-Optimal_Clusters_KMeans(df1, max_clusters=10, plot_clusters = TRUE, criterion = "variance_explained",fK_threshold = 0.90)

fviz_nbclust(df1, kmeans, method = "gap_stat")+labs(x ="Número de clusters")+labs(y="GAP")+labs(title = "Número óptimo de clusters")

fit <- cascadeKM(df1, 2, 10, iter = 500)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)

opt_aic<-Optimal_Clusters_KMeans(df1, 10, 'euclidean', plot_clusters=TRUE,criterion="AIC")

nb <- NbClust(df1, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D", index ="all")

names(nb) 

nb$Best.nc


###¿2, 3, o 4, clusters?

k2 <- kmeans(df1, centers = 2, nstart = 25)
k3 <- kmeans(df1, centers = 3, nstart = 25)
k4 <- kmeans(df1, centers = 4, nstart = 25)

###Suma del error cuadrático
###Una métrica sencilla es el error cuadrático de suma dentro de un conglomerado (WSS), 
###que mide la suma de todas las distancias desde las observaciones hasta el centro de su conglomerado. 
###A veces, esto se escala con el error cuadrático de suma total (TSS), la distancia desde todas las observaciones hasta 
###el centroide global; en particular, a menudo se calcula la relación WSS/TSS. 
###En principio, los valores pequeños de WSS o de la relación WSS/TSS sugieren que las observaciones dentro 
###de los conglomerados están más cerca (son más similares) entre sí que con respecto a los otros conglomerados.

k2$tot.withinss/k2$totss; k3$tot.withinss/k3$totss; k4$tot.withinss/k4$totss

p2 <- fviz_cluster(k2, geom = "point", data = df1)+ ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = df1) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = df1) + ggtitle("k = 4")


grid.arrange(p2, p3, p4, ncol = 3)

###Cuncluimos que hay K=2 grupos de pacientes

fviz_cluster(k2, geom = "point",  data = df1) + ggtitle("Número de grupos de pacientes: 2")

fviz_cluster(k2, data = df1,
             palette=c("deeppink3", "magenta3"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())+ ggtitle("Número de grupos de pacientes: 2")

fviz_cluster(k2, df1, ellipse.type = "norm")

fviz_cluster(k2, df1, palette = "Set2", ggtheme = theme_minimal())




require(tibble)

k3 %>%
  extract_centroids()%>% as_tibble() %>% print(width=Inf)

kmeans_clusters <- 
  bind_cols(df1, cluster=k3$cluster)

kmeans_clusters %>%
  pivot_longer(-cluster) %>% 
  ggplot(aes(x = as.factor(cluster), y = value, fill = as.factor(cluster))) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(vars(name), scales = "free") 

kmeans_clusters %>% 
  group_by(cluster) %>% 
  summarise(num_users = n()) %>% 
  mutate(pct_users = num_users / sum(num_users))

table(df$diagnosis)


###Y si hacemos cluster, primero haciendo reduccion de dimension a traves de PCA
###7 componentes proporcionan alrededor del 92% de la varianza explicada

df_pca_rec <- recipe(~ ., data = df) %>%
  update_role(diagnosis, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 7)

df_pca_wf <- workflow() %>%
  add_recipe(df_pca_rec)

df_pca_hier <- df_pca_wf %>%
  add_model(hier_clust(linkage_method = "ward.D")) %>%
  fit(data = df) %>%
  extract_fit_engine() %>%
  plot()

df_pca_hier <- df_pca_wf %>%
  add_model(hier_clust(linkage_method = "ward.D2")) %>%
  fit(data = df) %>%
  extract_fit_engine() %>%
  plot()

df_pca_hier <- df_pca_wf %>%
  add_model(hier_clust(linkage_method = "ward.D")) %>%
  fit(data = df) %>%
  extract_fit_engine() %>%
  fviz_dend(k = 3, main = "Dendograma basado en PCA: Liga Ward")%>%
  plot()
















