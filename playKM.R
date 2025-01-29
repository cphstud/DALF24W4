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
df$cluster=round(runif(20,min=1,max = 3),0)

ggplot(df, aes(x=X,y=Y, colour = as.factor(cluster)))+geom_point()












