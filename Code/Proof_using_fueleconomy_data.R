library(tidyverse)
library(fueleconomy)

##### Option 1 -- Fuel Economy (Dataset that Hadley created ####
test_data <- vehicles %>% #From the fueleconomy package
  filter(year > 2000) %>%
  mutate(trans = ifelse(is.na(trans), "None", trans),
         cyl = ifelse(is.na(cyl), 0, cyl),
         displ = ifelse(is.na(displ), 0, displ),
        make = ifelse(make == "London Taxi", "London Taxi Co", make)
        ) %>%
  mutate(make = factor(make),
         model = factor(model),
         year = factor(year),
         class = factor(class),
         trans = factor(trans),
         drive = factor(drive),
         fuel = factor(fuel))

# I need to split the dataset to get through the examples
small_test_data <- vehicles %>% #From the fueleconomy package
  filter(year >= 2010) %>%
  mutate(trans = ifelse(is.na(trans), "None", trans),
         cyl = ifelse(is.na(cyl), 0, cyl),
         displ = ifelse(is.na(displ), 0, displ),
         make = ifelse(make == "London Taxi", "London Taxi Co", make)
  ) %>%
  mutate(make = factor(make),
         model = factor(model),
         year = factor(year),
         class = factor(class),
         trans = factor(trans),
         drive = factor(drive),
         fuel = factor(fuel))


#### GGPCP Visualizations ####
library(ggpcp)

p1 <- small_test_data %>%
  pcp_select(2:9, 11:12) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  #geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
  geom_pcp(aes(colour = fuel), overplot = "none")


#### Dimension Reduction Techniques #####
# Remove duplicate rows
test_data_unique <- unique( small_test_data[,-1] )
dframe <- test_data_unique[complete.cases(test_data_unique),]

#### t-SNE Dimension Reduction ####
library(Rtsne)


#fuel_matrix <- as.matrix(dframe)

# Perform t-SNE
#tsne_result <- Rtsne(fuel_matrix, pca=FALSE, perplexity=30, theta=0.0)
#tsne_out <- Rtsne(dist(normalize_input(fuel_matrix)), theta=0.0)
tsne_out <- Rtsne(dframe, pca=FALSE, theta=0.0)


# Create a data frame with the t-SNE results and the species information
tsne_data <- data.frame(tsne_out$Y, fuel = dframe$fuel)

# Plot the t-SNE results
ggplot(tsne_data, aes(x = X1, y = X2, color = fuel)) +
  geom_point() +
  ggtitle("t-SNE on Fuel Economy Dataset")


tsne_data %>%
  pcp_select(1:2) %>%
  #pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  #geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
  geom_pcp(aes(colour = fuel), overplot = "none")

#### K-Prototype Dimension Reduction ####
library(clustMixType)
library(ggcorrplot)
library(caret)
library(corrplot)

kprototype_df <- dframe

#correlation plot for mixture data type
#change column names for more better labels visual
for (i in 1:ncol(kprototype_df)){
  colnames(kprototype_df)[i] <- paste0("X", i-1)
}

# model.matrix(~0+., data = kprototype_df) %>%
#   cor(use = "pairwise.complete.obs") %>%
#   ggcorrplot(show.diag = FALSE, type = "full", tl.cex = 7, lab = TRUE,
#              lab_size = 1.5) +
#   labs(title = "Correlation Heat Map of Used Variables *high-correlation deleted")

fuel_clusters <- kproto(kprototype_df, 5)
fuel_clusters

#summary of each variable
summary(fuel_clusters)

fit_df <- factor(fuel_clusters$cluster, order =  TRUE,
                 levels = c(1:5))
fit <- data.frame(dframe, fit_df)

fit %>%
  #arrange(fit_df) %>%
  pcp_select(1:8,10:11) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange(fit_df, method = "from-both") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  #geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
  geom_pcp(aes(colour = fuel), overplot = "none")

result_df <- fuel_clusters$centers
member <- fuel_clusters$size
result <- data.frame(member, result_df)
result

require(gridExtra)

p2 <- fit %>%
  #arrange(fit_df) %>%
  pcp_select(1:11) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  #geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
  geom_pcp(aes(colour = fit_df), overplot = "none")


grid.arrange(p1, p2, ncol=2)


#### Uniform Manifold Approximation and Projection (UMAP) ####
library(umap)
dmatrix <- data.matrix(dframe)

umap_result <- umap(dmatrix)

umap_data <- data.frame(umap_result$layout, fuel = dframe$fuel)
ggplot(umap_data, aes(x = X1, y = X2, color = fuel)) +
  geom_point() +
  ggtitle("UMAP on Fuel Economy Dataset")

umap_data %>%
  pcp_select(1:3) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  #geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
  geom_pcp(aes(colour = fuel), overplot = "none")


#### Factorial Analysis of Mixed Data (FAMD) ####
library("FactoMineR")
library("factoextra")

res.famd <- FAMD(dframe, graph = FALSE)
print(res.famd)

eig.val <- get_eigenvalue(res.famd)
head(eig.val)

# Scree plot
fviz_screeplot(res.famd)

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)

fviz_famd_var(res.famd, "quanti.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

#Qualitive variables
quali.var <- get_famd_var(res.famd, "quali.var")
quali.var

fviz_famd_var(res.famd, "quali.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

fviz_famd_ind(res.famd, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

fviz_mfa_ind(res.famd,
             habillage = "make", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE # Avoid text overlapping
)

fviz_ellipses(res.famd, c("make", "model"), repel = TRUE)


fviz_ellipses(res.famd, 1:2, geom = "point")


#### Gower Distance Function ####
library(cluster)

gower_dist <- daisy(dframe,
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair
dframe[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
dframe[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){

  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)

  sil_width[i] <- pam_fit$silinfo$avg.width

}

# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

##### PAM Clustering Method ####
pam_fit <- pam(gower_dist, diss = TRUE, k = 5)

pam_results <- dframe %>%
  #dplyr::select(-name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

dframe[pam_fit$medoids, ]

#### PCA Mixed Data (PCAmixdata) #####
library(PCAmixdata)

# Split the datasets for analysis
split <- splitmix(dframe)

# Quantitative Dataset
X1 <- split$X.quanti
# Qualiative Dataset
X2 <- split$X.quali

obj <- PCAmix(X.quanti=X1,X.quali=X2,graph=FALSE,ndim=5)
head(obj$quanti.cor)

test<-1:5

