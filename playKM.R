# get two observations

df=read.csv("Clustered_Data.csv")

plot(df)
#1. Randomly assign a number, from 1 to K, to each of the observations.
# These serve as initial cluster assignments for the observations.
#2. Iterate until the cluster assignments stop changing:
#  (a) For each of the K clusters, compute the cluster centroid. The
#    kth cluster centroid is the vector of the p feature means for the
#    observations in the kth cluster.
# (b) Assign each observation to the cluster whose centroid is closest

#1. Randomly assign a number, from 1 to K, to each of the observations.
k=3
df$cluster=round(runif(20,min=1,max = k),0)

#Loop until changecounter stops
isChanged=1
roundcounter=0
while (isChanged == 1) {
  print("GO")
  changecounter=0
  roundcounter=roundcounter+1
  
  #2. Find centroids - find means of the observation in each cluster
  centroids=df %>% group_by(cluster) %>% summarise(X=mean(X),
                                                 Y=mean(Y)) %>% ungroup()
  
  # plot the centroids and cluster
  p <- ggplot(df, aes(x=X,y=Y, colour = as.factor(cluster)))+geom_point()+
    geom_point(data=centroids, aes(x=X,y=Y, size=4,shape = as.factor(cluster)))
  print(p)
  Sys.sleep(2)
  
  
  # assign each obs - uanset cluster - to the closest centercluster
  # closest = min(√[(x2 – x1)2 + (y2 – y1)2]) or R: sqrt((cx-obsx)^2 + (cy-obsy)^2)
  for (i in (1:nrow(df))) {
    # vector to collect the distances for the point to the respective cluster
    mdist=sapply(1:k,function(j) {
      sqrt((centroids$X[j]-df$X[i])^2 + 
             (centroids$Y[j]-df$Y[i])^2+ 
             (centroids$Z[j]-df$Z[i])^2)
    })
    newcluster=which.min(mdist)
    if(df[i,'cluster']!=newcluster) {
      df[i,'cluster']=newcluster
      changecounter=changecounter+1
    }
  }
  
  #plot the new clusterassignment
  p <- ggplot(df, aes(x=X,y=Y, colour = as.factor(cluster)))+geom_point()+
    geom_point(data=centroids, aes(x=X,y=Y, size=4,shape = as.factor(cluster)))+
    ggtitle(paste("Got this",roundcounter))
  print(p)
  Sys.sleep(5)
  
  # Should we continue?
  isChanged = ifelse(changecounter > 0,1,0)
  print(paste("SLEEP",isChanged))
}
