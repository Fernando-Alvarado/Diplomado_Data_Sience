###PCAtdyclustWDBC

library(tidymodels)
library(tidyclust)


df<-read.csv("C:/Users/Salvador/Desktop/Cursos2025-II/Diplomado Introducción Analítica a la Ciencia de Datos/BreastCancerDiagnosisWisconsin.csv")

df <- df %>% 
  mutate(diagnosis = relevel(as.factor(diagnosis), "B", "M"))


kmeans_spec <- k_means(num_clusters = 2) %>%
  set_engine("ClusterR")
kmeans_spec

rec_spec <- recipe(~ ., data = df) %>%
   update_role(diagnosis, new_role = "id") %>%
   step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), threshold = 0.90) ###Numero de componentes que explican al menos un 90% de varianza



kmeans_wf <- workflow(rec_spec, kmeans_spec)

kmeans_fit <- fit(kmeans_wf, data = df)
kmeans_fit

names(kmeans_fit)

pca_estimates <- prep(rec_spec)

df_pca <- pca_estimates %>% 
  bake(new_data = NULL)

head(df_pca)

df_pca %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(size = 2, color = "dodgerblue3")

df_pca %>% 
  ggplot(mapping = aes(x = PC1, y = PC2,color = diagnosis)) +
  geom_point(size = 2)


kmeans_spec <- kmeans_spec %>% 
  set_args(num_clusters = tune())

kmeans_wf <- workflow(rec_spec, kmeans_spec)
kmeans_wf

set.seed(1234)
boots <- bootstraps(df, times = 10)

tune_res <- tune_cluster(
  kmeans_wf,
  resamples = boots,
  metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio)
)

res_metrics <- tune_res %>% collect_metrics()%>% print(n=Inf)

best <- tune_res %>%
  select_best(metric="sse_ratio")

res_metrics %>%
  filter(.metric == "sse_ratio") %>%
  ggplot(aes(x = num_clusters, y = mean)) +
  geom_point(col="darkblue",size=2) +
  geom_line(col="red") +
  theme_minimal() +
  ylab("mean WSS/TSS") +
  xlab("Number of clusters") +
  scale_x_continuous(breaks = 1:10)


clust_num_grid <- grid_regular(num_clusters(),levels = 10)

tune_res1<-tune_cluster(
    kmeans_wf,
    resamples = boots,
    grid = clust_num_grid,
    metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio),
    control = tune::control_grid(save_pred = TRUE, extract = identity)
  )

res_metrics1 <- tune_res1 %>% collect_metrics()%>% print(n=Inf)


best1 <- tune_res1 %>%
           select_best(metric="sse_ratio")

best1


extract_cluster_assignment(kmeans_fit)

extract_centroids(kmeans_fit)

predict(kmeans_fit, new_data = slice_sample(df, n = 10))

df_pca %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(size=2, color = diagnosis), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple"))


results <- augment(kmeans_fit,df) 
head(results)

table(results$diagnosis,results$.pred_cluster)

library(plotly)

dat<-data.frame(df_pca,cluster=results$.pred_cluster)
head(dat)

p <- dat %>%
  ggplot(aes(PC1, PC2, color = cluster)) +
  geom_point()

ggplotly(p)

clust_plot <- dat %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = factor(cluster), color = diagnosis), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple"))

ggplotly(clust_plot)


plot3d( 
  x=dat$PC1, y=dat$PC2, z=dat$PC3, 
  col = dat$diagnosis, 
  type = 's', 
  radius = .1,
  xlab="PC1", ylab="PC2", zlab="PC3")

g.dat <- plot_ly(dat, x = ~PC1, y = ~PC2, z = ~PC3, color = ~diagnosis, colors =c("#0000FF", "#FF00FF"),size=2.5 )
g.dat <- g.df1 %>% add_markers()
g.dat <- g.df1 %>% layout(scene = list(xaxis = list(title = 'PC1'),
                     yaxis = list(title = 'PC2'),
                     zaxis = list(title = 'PC3')))

g.dat
















