library(GGally)
library(palmerpenguins)
library(tidyverse)
library(ggpcp)
library(stats)

ggpairs(penguins[,c(1, 3:6)], aes(colour = species))


##### Bundling and Curving in PCPs ####

# Remove rows with missing data from the dataset
penguins_clean <- penguins %>%
  na.omit() %>%
  mutate(species = as.factor(species)
         ) %>%
  select(-island, -sex, -year)


# Create a parallel coordinate plot with bundling and curving
ggparcoord(
  penguins_clean,
  columns = 2:5, # Using the numeric columns
  groupColumn = 1,
  alphaLines = 0.5,
  splineFactor = 5
) +
     theme_minimal() +
     labs(title = "Parallel Coordinate Plot with Bundling and Curving",
                   x = "Variables",
                   y = "Scaled Value")


##### Demostrating Density in PCPs ####
ggparcoord(
  data = penguins_clean,
  columns = 2:5, # Using the numeric columns
  groupColumn = 1,
  scale = "uniminmax", # Scale columns to range between [0, 1]
  alphaLines = 0.3, # Set transparency for lines
  #showPoints = TRUE, # Show points on lines
  mapping = ggplot2::aes(color = species) # Color lines by species
) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Density in Parallel Coordinate Plot of Palmer Penguins",
                y = "Scaled Values",
                x = "Variables")

##### Using PCA in PCPs ######

# Remove rows with missing data from the dataset
penguins_clean <- penguins %>%
  na.omit() %>%
  select(-island, -sex, -year)

# Perform PCA on the numeric columns
penguins_pca <- prcomp(penguins_clean %>%
                         select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
                       center = TRUE, scale. = TRUE)

# Create a data frame with the PCA results
penguins_pca_df <- as.data.frame(penguins_pca$x)
# Add species as a categorical variable to the PCA data frame
penguins_pca_df$species <- penguins_clean$species

# Create a parallel coordinate plot using ggparcoord for the PCA results
ggparcoord(
  data = penguins_pca_df,
  columns = 1:4, # Using the PCA components
  groupColumn = 5,
  scale = "uniminmax", # Scale columns to range between [0, 1]
  alphaLines = 0.3, # Set transparency for lines
  showPoints = TRUE, # Show points on lines
  mapping = ggplot2::aes(color = species) # Color lines by species
) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "PCA in Parallel Coordinate Plot of Palmer Penguins",
                y = "Scaled PCA Values",
                x = "Principal Components")

##### enhanced color encoding and shading in PCPs ####

# Create a parallel coordinate plot with enhanced color encoding and shading
ggparcoord(
  data = penguins_clean,
  columns = 2:5, # Using the numeric columns
  groupColumn = 1,
  scale = "uniminmax", # Scale columns to range between [0, 1]
  alphaLines = 0.5, # Set transparency for lines
  showPoints = TRUE, # Show points on lines
  mapping = ggplot2::aes(color = species), # Color lines by species
  shadeBox = NULL # Use to add shading to enhance visual distinction between variables
) +
  scale_color_manual(values = c("Adelie" = "#1f78b4", "Chinstrap" = "#33a02c", "Gentoo" = "#e31a1c")) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Enhanced Color Encoding and Shading in PCP of Palmer Penguins",
                y = "Scaled Values",
                x = "Variables") +
  theme(legend.position = "bottom")

##### Reordering in PCPs ####

# Reorder columns for better interpretability
penguins_reordered <- penguins_clean %>%
  select(species, flipper_length_mm, body_mass_g, bill_length_mm, bill_depth_mm)

# Create a parallel coordinate plot with reordering and axis flipping using ggparcoord
ggparcoord(
  data = penguins_reordered,
  columns = 2:5, # Using the reordered numeric columns
  groupColumn = 1,
  scale = "uniminmax", # Scale columns to range between [0, 1]
  alphaLines = 0.5, # Set transparency for lines
  showPoints = TRUE, # Show points on lines
  mapping = ggplot2::aes(color = species)
) +
  #scale_color_manual(values = c("Adelie" = "#1f78b4", "Chinstrap" = "#33a02c", "Gentoo" = "#e31a1c")) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Reordering and Axis Flipping in Parallel Coordinate Plot of Palmer Penguins",
                y = "Scaled Values",
                x = "Variables") +
  theme(legend.position = "bottom") +
  ggplot2::scale_y_reverse() # Flip the y-axis to observe the effect

##### Clustering Based PCPs #####

# Select only numeric columns for clustering
penguins_numeric <- penguins_clean %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

# Perform hierarchical clustering
penguins_dist <- dist(penguins_numeric)
penguins_hclust <- hclust(penguins_dist, method = "ward.D2")

# Cut the dendrogram into 3 clusters
penguins_clusters <- cutree(penguins_hclust, k = 3)

# Add cluster information to the dataset
penguins_clean$cluster <- as.factor(penguins_clusters)

# Create a parallel coordinate plot with clusters using ggparcoord
ggparcoord(
  data = penguins_clean,
  columns = 2:5, # Using the numeric columns
  groupColumn = "cluster",
  scale = "uniminmax", # Scale columns to range between [0, 1]
  alphaLines = 0.5, # Set transparency for lines
  #showPoints = TRUE, # Show points on lines
  mapping = ggplot2::aes(color = cluster)
) +
 # scale_color_manual(values = c("1" = "#1f78b4", "2" = "#33a02c", "3" = "#e31a1c")) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Cluster-Based Parallel Coordinate Plot of Palmer Penguins",
                y = "Scaled Values",
                x = "Variables") +
  theme(legend.position = "bottom")

##### Optimized Layout Algorithms PCPs #####

# Calculate the correlation matrix for optimized ordering
cor_matrix <- cor(penguins_clean %>%
                    select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g))

# Find the order of variables that maximizes the interpretability (based on correlation)
order_vars <- order.dendrogram(as.dendrogram(hclust(as.dist(1 - cor_matrix))))
penguins_optimized <- penguins_clean %>%
  select(all_of(order_vars), body_mass_g)

# Create a parallel coordinate plot with optimized layout using ggparcoord
ggparcoord(
  data = penguins_optimized,
  columns = c(1,3:5), # Using the optimized numeric columns
  groupColumn = "species",
  scale = "uniminmax", # Scale columns to range between [0, 1]
  alphaLines = 0.5, # Set transparency for lines
  showPoints = TRUE, # Show points on lines
  mapping = ggplot2::aes(color = species)
) +
  scale_color_manual(values = c("Adelie" = "#1f78b4", "Chinstrap" = "#33a02c", "Gentoo" = "#e31a1c")) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Optimized Layout in Parallel Coordinate Plot of Palmer Penguins",
                y = "Scaled Values",
                x = "Variables") +
  theme(legend.position = "bottom")


##### Categorical and Hybrid PCPs #####
# Remove rows with missing data from the dataset
penguins_clean_cat <- penguins %>%
  na.omit() %>%
  select(-year)

# Convert categorical variables to factors
penguins_clean_cat <- penguins_clean_cat %>%
  mutate(island = as.factor(island),
         sex = as.factor(sex),
         species = as.factor(species))

# Create a parallel coordinate plot using both numeric and categorical variables

penguins_clean_cat %>%
  pcp_select(2:7) %>%
  pcp_scale(method="uniminmax") %>%
  # pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Categorical and Hybrid Parallel Coordinate Plot of Palmer Penguins",
                y = "Scaled Values",
                x = "Variables") +
  theme(legend.position = "bottom") +
  facet_wrap(~sex) # Facet by categorical variable 'island' to create a hybrid plot
