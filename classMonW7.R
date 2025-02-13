library(dkstat)
library(mapDK)

strafmeta=dst_meta("straf44")

# BY2
bymeta=dst_meta("by2")
bymeta$variables

my_query <- list(
  KOMK = "*",
  BYST = "*",
  Tid = "2023"
)
dfBy2=(dst_get_data(table = "by2", query = my_query, lang = "da"))
dfBy2=dfBy2 %>% select(-TID)
dfBy2=dfBy2 %>% filter(BYST!="Uden fast bopæl")
dfBy2=dfBy2 %>% group_by(KOMK) %>% mutate(
  total=sum(value),
  urbprct=if_else(BYST=="Landdistrikter",(value/total)*100,0)
) %>% ungroup()

dfBy3 <- dfBy2 %>% filter(BYST=="Landdistrikter") %>% select(KOMK,urbprct)
colnames(dfBy3)=c("OMRÅDE","urbproct")

# Straf44
strafmeta$variables
strafmeta$values$OVERTRÆD
strafmeta$values$Tid

# Straf44
my_query <- list(
  OMRÅDE = "*",
  OVERTRÆD=c("Voldsforbrydelser i alt","Voldtægt mv.","Manddrab"),
  TID = c("2023","2022","2021","2020")
)

dfStraf=(dst_get_data(table = "straf44", query = my_query, lang = "da"))
dfStraf <- dfStraf %>% group_by(OMRÅDE,OVERTRÆD) %>% 
  summarise(nval=sum(value))

dfStrafWide <- dfStraf %>% pivot_wider(names_from = OVERTRÆD,values_from = nval)
dkArrest <- inner_join(dfStrafWide,dfBy3,by="OMRÅDE")
dkArrest$urbproct=round(dkArrest$urbproct,1)
dkArrestSc=as.data.frame(scale(dkArrest[,-1]))
# remove row 52
dkArrestSc=dkArrestSc[-52,]
dkArrest=dkArrest[-52,]


#Data retrieval done

#Explore data for correlations and outliers
# outliers
hist(dkArrestSc$Manddrab)
hist(dkArrestSc$`Voldsforbrydelser i alt`)
hist(dkArrestSc$`Voldtægt mv.`)
#corrlations
dkArrCorr=cor(dkArrestSc)
corrplot::corrplot(dkArrCorr,addCoef.col="black",method="square",type="lower")

#Clustering
colldf=as.data.frame(matrix(nrow=0,ncol=2))
for(i in (1:8)) {
  tmpcl=kmeans(dfArrestSc,centers = i,nstart = 10)
  colldf[i,1]=i
  colldf[i,2]=tmpcl$tot.withinss
}
plot(colldf)

# chose k
dkCl=kmeans(dkArrestSc,center=3,nstart = 10)
# hurtig viz check
fviz_cluster(dkCl,data = dkArrestSc)

# clusterinfo lægges ind
dkArrest$cluster=dkCl$cluster


# ANALYSE
dkArrestAnalyse=dkArrest %>% group_by(cluster) %>% 
  summarise(across(2:5,mean))

dkArrestAnalyseLong=dkArrestAnalyse %>% pivot_longer(c(2,3,4,5),names_to = "crimetype",values_to = "value")
#barplot
ggplot(dkArrestAnalyseLong,aes(x=cluster,y=log(value),fill=as.factor(crimetype)))+
  geom_bar(stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 80))

# PCA
dkpca=princomp(dkArrestSc)
dkpca$loadings

library(plotly)
# 3d plot
p <- plot_ly(
  data=dkArrests,
  x = ~Manddrab,y = ~`Voldtægt mv.`,z = ~urbproct,
  type="scatter3d",
  mode="markers",
  color=~as.factor(cluster),
  text= ~paste(
    "Kommune:",OMRÅDE,
    "<br>Drab:",Manddrab,
    "<br>Pop:",urbproct,
    "<br>Voldtægt:",`Voldtægt mv.`
    ),
  hoverinfo="text"
)

p
# plot data on map
library(mapDK)
dkArrestMap=dkArrests %>% select(OMRÅDE,cluster) %>% mutate(cluster=cluster*3000)
colnames(dkArrestMap)=c("kommune","cluster")
mapDK(values = "cluster", id = "kommune", data = dkArrestMap)



