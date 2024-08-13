library(tidyverse)
library(fueleconomy)

##### Option 1 -- Fuel Economy (Dataset that Hadley created ####
test_data <- vehicles %>% #From the fueleconomy package
  filter(year > 2000) %>%
  mutate(trans = ifelse(is.na(trans), "None", trans),
         cyl = ifelse(is.na(cyl), 0, cyl),
         displ = ifelse(is.na(displ), 0, displ)
                        ) %>%
  mutate(make = factor(make),
         model = factor(model),
         year = factor(year),
         class = factor(class),
         trans = factor(trans),
         drive = factor(drive),
         fuel = factor(fuel))

library(ggpcp)

test_data %>%
  pcp_select(2:12) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  #geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
  geom_pcp(aes(colour = fuel), overplot = "none")



#### Dimension Reduction Techniques #####

#### t-SNE Dimension Reduction ####
library(Rtsne)

# Remove duplicate rows
test_data_unique <- unique( test_data[,-1] )
dframe <- test_data_unique[complete.cases(test_data_unique),]
#fuel_matrix <- as.matrix(dframe)

# Perform t-SNE
#tsne_result <- Rtsne(fuel_matrix, pca=FALSE, perplexity=30, theta=0.0)
#tsne_out <- Rtsne(dist(normalize_input(fuel_matrix)), theta=0.0)
tsne_out <- Rtsne(dframe,pca=FALSE, theta=0.0)

# Create a data frame with the t-SNE results and the species information
tsne_data <- data.frame(tsne_out$Y, fuel = dframe$fuel)

# Plot the t-SNE results
ggplot(tsne_data, aes(x = X1, y = X2, color = fuel)) +
  geom_point() +
  ggtitle("t-SNE on Fuel Economy Dataset")

tsne_data %>%
  pcp_select(1:3) %>%
  pcp_scale(method="uniminmax") %>%
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

model.matrix(~0+., data = kprototype_df) %>%
  cor(use = "pairwise.complete.obs") %>%
  ggcorrplot(show.diag = FALSE, type = "full", tl.cex = 7, lab = TRUE,
             lab_size = 1.5) +
  labs(title = "Correlation Heat Map of Used Variables *high-correlation deleted")

fuel_clusters <- kproto(kprototype_df, 5)
fuel_clusters

#summary of each variable
summary(fuel_clusters)

fit_df <- factor(fuel_clusters$cluster, order =  TRUE,
                 levels = c(1:5))
fit <- data.frame(kprototype_df, fit_df)
result_df <- fuel_clusters$centers
member <- fuel_clusters$size
result <- data.frame(member, result_df)
result

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

res.famd <- FAMD(kprototype_df, graph = FALSE)
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
