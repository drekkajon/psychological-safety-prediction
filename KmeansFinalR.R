library(readxl)
library(dplyr)
library(cluster)
library(factoextra)

# Load dataset
gss_clean <- read_excel(file.choose())

# Select clustering variables
cluster_data <- gss_clean %>%
  select(trustman, respect, spvtrfair, age) %>%
  drop_na()

# Convert factors to numeric
cluster_data_num <- cluster_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(everything(), as.numeric))

# Standardize variables
cluster_scaled <- scale(cluster_data_num)

# Run K-means
set.seed(123)

kmeans_result <- kmeans(
  cluster_scaled,
  centers = 2,
  nstart = 25
)

# Visualize clusters
fviz_cluster(
  kmeans_result,
  data = cluster_scaled,
  geom = "point",
  ellipse.type = "norm",
  main = "K-Means Clustering of Workplace Perception Variables"
)