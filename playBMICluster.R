library(dplyr)
library(plotly)

# Load Data
df <- read.csv("Clustered_Data_3d.csv")
dffull <- read.csv("bmi.csv")
unique(dffull$BmiClass)
colnames(dffull)
df <- dffull[,-c(4,5)]
head(df)
colnames(df) = c("X","Y","Z")
dfsc=scale(df)
head(dfsc)
df=as.data.frame(dfsc)

# 1. Randomly assign a number, from 1 to K, to each observation
k <- 3  # Number of clusters
df$cluster <- sample(1:k, nrow(df), replace = TRUE)



# Loop until cluster assignments stop changing
isChanged <- TRUE
roundcounter <- 0

while (isChanged) {
  cat("Iteration:", roundcounter + 1, "\n")
  changecounter <- 0
  roundcounter <- roundcounter + 1
  
  # 2. Compute new centroids
  centroids <- df %>%
    group_by(cluster) %>%
    summarise(X = mean(X), Y = mean(Y), Z = mean(Z)) %>%
    ungroup()
  
  # 3D Plot with Plotly
  p <- plot_ly() %>%
    add_trace(data = df, x = ~X, y = ~Y, z = ~Z, 
              type = "scatter3d", mode = "markers",
              color = ~as.factor(cluster),
              marker = list(size = 5)) %>%
    add_trace(data = centroids, x = ~X, y = ~Y, z = ~Z, 
              type = "scatter3d", mode = "markers",
              marker = list(size = 10, symbol = "diamond", color = "black"),
              name = "Centroids") %>%
    
    layout(title = paste("Iteration:", roundcounter),
           scene = list(xaxis = list(title = "X"),
                        yaxis = list(title = "Y"),
                        zaxis = list(title = "Z")))
  
  print(p)
  Sys.sleep(2)  # Pause for visualization
  
  # 3. Assign each observation to the closest centroid
  for (i in 1:nrow(df)) {
    distances <- sapply(1:k, function(j) { sqrt((centroids$X[j] - df$X[i])^2 + (centroids$Y[j] - df$Y[i])^2 + (centroids$Z[j] - df$Z[i])^2) })
    new_cluster <- which.min(distances)
    
    if (df$cluster[i] != new_cluster) {
      df$cluster[i] <- new_cluster
      changecounter <- changecounter + 1
    }
  }
  
  # Check if clusters have changed
  isChanged <- changecounter > 0
  cat("Changes:", changecounter, "\n")
}

cat("Clustering complete!\n")

df$class=dffull$BmiClass
p <- plot_ly() %>%
  add_trace(data = df, x = ~X, y = ~Y, z = ~Z, 
            type = "scatter3d", mode = "markers",
            color = ~as.factor(cluster),
            text = ~paste("Class:", class),
            marker = list(size = 5)) %>%
  add_trace(data = centroids, x = ~X, y = ~Y, z = ~Z, 
            type = "scatter3d", mode = "markers",
            marker = list(size = 10, symbol = "diamond", color = "black"),
            name = "Centroids") %>%
    
    layout(title = paste("Iteration:", roundcounter),
           scene = list(xaxis = list(title = "X"),
                        yaxis = list(title = "Y"),
                        zaxis = list(title = "Z")))
p
