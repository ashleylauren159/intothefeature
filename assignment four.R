sgemm_product <- read.csv("C:/Users/18326/Desktop/Spring 2020/Machine Learning/sgemm_product_dataset/sgemm_product.csv")
View(sgemm_product)
sgemm_product$avg_run <- (sgemm_product$Run1..ms. + sgemm_product$Run2..ms. + sgemm_product$Run3..ms. + sgemm_product$Run4..ms.) / 4
sgemm_product$Run1..ms. <- NULL
sgemm_product$Run2..ms. <- NULL
sgemm_product$Run3..ms. <- NULL
sgemm_product$Run4..ms. <- NULL
sgemm_product2 <- sgemm_product
View(sgemm_product2)
sgemm_product3 <- sgemm_product2 %>% mutate_at(c("MWG", "NWG", "KWG", "MDIMC", "NDIMC", "MDIMA", "NDIMB", "KWI", "VWM", "VWN", "STRM", "STRN", "SA", "SB", "avg_run"), ~(scale(.) %>% as.vector))
View(sgemm_product3)
sgemm_product2 <- NULL
library(tidyverse)
library(cluster)
library(factoextra)
library(mclust)
library(Metrics)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(dplyr)
set.seed(1234)
size_reduce <- floor(0.06*nrow(sgemm_product))
data_sample <- sgemm_product[sample(1:nrow(sgemm_product), size_reduce, replace = FALSE), ]
fviz_nbclust(data_sample, kmeans, method = "wss")
fviz_nbclust(data_sample, kmeans, method = "silhouette")
k1 <- kmeans(sgemm_product3, 6, nstart = 1)
k2 <- kmeans(sgemm_product3, 6, nstart = 50)
k1$tot.withinss
k2$tot.withinss
k2
data <- cbind(sgemm_product3, clusterNum = k2$cluster)
View(data)
em.fit <- Mclust(sgemm_product3)
summary(em.fit)
plot(em.fit, what = "BIC")
set.seed(101)
data$clusterNum <- as.factor(data$clusterNum)
smp_size = floor(0.7*nrow(data))
set.seed(123)
train_ind = sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
rf.fit <- randomForest(train$clusterNum ~ ., data = train)
rf.pred <- predict(rf.fit, test)
table(rf.pred)
auc(rf.pred, test$clusterNum)
importance(rf.fit)
corrplot(data_corr, method = "number", number.cex = 0.6)
data2 <- data
View(data2)
data2$clusterNum <- NULL
library(h2o)
h2o.init()
pca_data <- as.h2o(data2)
data_split <- h2o.splitFrame(pca_data, ratios = 0.7, seed = 123456)
pca_train <- data_split[[1]]
pca_valid <- data_split[[2]]
data.pca <- h2o.prcomp(training_frame = pca_train, k = 5, use_all_factor_levels = TRUE, pca_method = "GLRM", compute_metrics = TRUE, score_each_iteration = TRUE)
data.pca@model$importance
data.pca@model$eigenvectors %>% as.data.frame() %>% mutate(feature = row.names(.)) %>% ggplot(aes(pc1, pc2, label = feature)) + geom_text()
data.pca2 <- h2o.prcomp(training_frame = pca_train, k = 15, use_all_factor_levels = TRUE, pca_method = "GLRM", compute_metrics = TRUE, score_each_iteration = TRUE)
eigen <- data.pca2@model$importance["Standard deviation", ] %>% as.vector() %>% .^2
sum(eigen)
which(eigen >= 1)
data.pca3 <- h2o.prcomp(training_frame = pca_train, k = 8, use_all_factor_levels = TRUE, pca_method = "GLRM", compute_metrics = TRUE, score_each_iteration = TRUE)
data.pca3@model$importance
data.pca3@model$eigenvectors %>% as.data.frame() %>% mutate(feature = row.names(.)) %>% ggplot(aes(pc1, pc2, label = feature)) + geom_text()
data.pca3@model$eigenvectors
install.packages("ica")
library(ica)
data.ica <- icafast(data2, 10)
data.ica$vafs
data.ica$iter
cor(data2, data.ica$S)
data_clust_rand <- data2
data_clust_rand$NWG <- NULL
data_clust_rand$KWG <- NULL
data_clust_rand$MDIMA <- NULL
data_clust_rand$NDIMB <- NULL
data_clust_rand$KWI <- NULL
data_clust_rand$VWM <- NULL
data_clust_rand$STRM <- NULL
data_clust_rand$STRN <- NULL
data_clust_rand$SA <- NULL
data_clust_rand$SB <- NULL
View(data_clust_rand)
library(tidyverse)
library(cluster)
library(factoextra)
library(mclust)
library(Metrics)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(dplyr)
fviz_nbclust(data_clust_rand, kmeans, method = "wss")
set.seed(4321)
size_reduce2 <- floor(0.06*nrow(data_clust_rand))
data_sample2 <- data_clust_rand[sample(1:nrow(data_clust_rand), size_reduce2, replace = FALSE), ]
fviz_nbclust(data_sample2, kmeans, method = "wss")
fviz_nbclust(data_sample2, kmeans, method = "silhouette")
k3.rf <- kmeans(data_clust_rand, 5, nstart = 50)
data_clust_pca <- data2
View(data_clust_pca)
data_clust_pca$MDIMA <- NULL
data_clust_pca$STRM <- NULL
data_clust_pca$SA <- NULL
data_clust_pca$MWG <- NULL
data_clust_pca$VWN <- NULL
data_clust_pca$avg_run <- NULL
View(data_clust_pca)
set.seed(54321)
size_reduce3 <- floor(0.06*nrow(data_clust_pca))
data_sample3 <- data_clust_pca[sample(1:nrow(data_clust_pca), size_reduce3, replace = FALSE), ]
fviz_nbclust(data_sample3, kmeans, method = "silhouette")
k4.pca <- kmeans(data_clust_pca, 9, nstart = 50)
k4.pca$tot.withinss
data_clust_ica <- data2
View(data_clust_ica)
data_clust_ica$MDIMA <- NULL
data_clust_ica$STRM <- NULL
data_clust_ica$STRN <- NULL
data_clust_ica$MWG <- NULL
data_clust_ica$NWG <- NULL
data_clust_ica$KWG <- NULL
data_clust_ica$NDIMB <- NULL
data_clust_ica$SB <- NULL
view(data_clust_ica)
k5.ica <- kmeans(data_clust_ica, 10, nstart = 50)
k5.ica$tot.withinss
data_clust_rand <- cbind(data_clust_rand, cNum = k3.rf$cluster)
data_clust_pca <- cbind(data_clust_pca, clNum = k4.pca$cluster)
data_clust_ica <- cbind(data_clust_ica, clusNum = k5.ica$cluster)
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "8G")
rf_dl_data <- as.h2o(data_clust_rand)
rf_dl_data$cNum <- as.factor(rf_dl_data$cNum)
h2o.levels(rf_dl_data$cNum)
splits_rf <- h2o.splitFrame(data = rf_dl_data, ratios = c(0.7, 0.15), seed = 1)
train_rf <- splits_rf[[1]]
valid_rf <- splits_rf[[2]]
test_rf <- splits_rf[[3]]
y1 <- "cNum"
x1 <- setdiff(names(train_rf), y1)
rf_dl_fit <- h2o.deeplearning(x = x1, y = y1, training_frame = train_rf, validation_frame = valid_rf, model_id = "dl_fit_rf", epochs = 50, activation = "tanhwithdropout", hidden = c(20, 20, 20), nfolds = 3, seed = 1)
rf_dl_perf <- h2o.performance(model = rf_dl_fit, newdata = test_rf)
h2o.mse(rf_dl_perf)
h2o.confusionMatrix(rf_dl_fit)
pca_dl_data <- as.h2o(data_clust_pca)
pca_dl_data$clNum <- as.factor(pca_dl_data$clNum)
h2o.levels(pca_dl_data$clNum)
splits_pca <- h2o.splitFrame(data = pca_dl_data, ratios = c(0.7, 0.15), seed = 1)
train_pca <- splits_pca[[1]]
valid_pca <- splits_pca[[2]]
test_pca <- splits_pca[[3]]
y2 <- "clNum"
x2 <- setdiff(names(train_pca), y2)
pca_dl_fit <- h2o.deeplearning(x = x2, y = y2, training_frame = train_pca, validation_frame = valid_pca, model_id = "pca_dl_fit", epochs = 50, activation = "tanhwithdropout", hidden = c(20, 20, 20), nfolds = 3, seed = 1)
pca_dl_perf <- h2o.performance(model = pca_dl_fit, newdata = test_pca)
h2o.mse(pca_dl_perf)
h2o.confusionMatrix(pca_dl_fit)
ica_dl_data <- as.h2o(data_clust_ica)
ica_dl_data$clusNum <- as.factor(ica_dl_data$clusNum)
h2o.levels(ica_dl_data$clusNum)
splits_ica <- h2o.splitFrame(data = ica_dl_data, ratios = c(0.7, 0.15), seed = 1)
train_ica <- splits_ica[[1]]
valid_ica <- splits_ica[[2]]
test_ica <- splits_ica[[3]]
y3 <- "clusNum"
x3 <- setdiff(names(train_ica), y3)
ica_dl_fit <- h2o.deeplearning(x = x3, y = y3, training_frame = train_ica, validation_frame = valid_ica, model_id = "ica_dl_fit", epochs = 50, activation = "tanh", hidden = c(20, 20, 20), nfolds = 3, seed = 1)
ica_dl_perf <- h2o.performance(model = ica_dl_fit, newdata = test_ica)
h2o.mse(ica_dl_perf)
h2o.confusionMatrix(ica_dl_fit)
plot(ica_dl_fit, timestep = "epochs", metric = "classification_error")
library(tidyverse)
library(cluster)
library(factoextra)
library(mclust)
library(Metrics)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(dplyr)
install.packages("Rtsne")
library(Rtsne)
gower_dist <- daisy(student.mat, metric = "gower")
gower_mat <- as.matrix(gower_dist)
student.mat[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE) [1, ], ]
student.mat[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE) [1, ], ]
sil_width <- c(NA)
for(i in 2:8) {
pam_fit <- pam(gower_dist, diss = TRUE, k = i)
sil_width[i] <- pam_fit$silinfo$avg.width
}
plot(1:8, sil_width, xlab = "Number of Clusters", ylab = "Silhouette Width")
lines(1:8, sil_width)
k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- student.mat %>% mutate(cluster = pam_fit$clustering) %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_results$the_summary
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
data.frame() %>%
setNames(c("X", "Y")) %>%
mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
geom_point(aes(color = cluster))
k <- 2
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- student.mat %>% mutate(cluster = pam_fit$clustering) %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_results$the_summary
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
data.frame() %>%
setNames(c("X", "Y")) %>%
mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
geom_point(aes(color = cluster))


