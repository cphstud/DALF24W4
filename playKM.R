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
df$cluster=round(runif(20,min=1,max = 3),0)

ggplot(df, aes(x=X,y=Y, colour = as.factor(cluster)))+geom_point()

#2. Find centroids
# find means of the observation in each cluster
centroids=df %>% group_by(cluster) %>% summarise(X=mean(X),
                                                 Y=mean(Y))
ggplot(df, aes(x=X,y=Y, colour = as.factor(cluster)))+geom_point()+
  geom_point(data=centroids, aes(x=X,y=Y, size=4,shape = as.factor(cluster)))

#assign each obs - uanset cluster - to the closest centercluster
# closest = min(√[(x2 – x1)2 + (y2 – y1)2])
# sqrt((cx-obsx)^2 + (cy-obsy)^2)

for (i in (1:nrow(df))) {
  i=1
  # vector to collect the distances for the point to the respective cluster
  colv=vector()
  for (j in (1:k)) {
    mdist=sqrt(((centroids[[j,2]]-df[[i,1]])^2)+((centroids[[j,3]]-df[[i,2]])^2))
    colv[j]=mdist
  }
  newcluster=which.min(colv)
  df[i,'cluster']=newcluster
}












