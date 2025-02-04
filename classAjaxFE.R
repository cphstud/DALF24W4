library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)

#connect to mongo
cong=mongo(
  collection = "games",
  db = "wyscoutdutch",
  url= "mongodb://localhost"
)

conm=mongo(
  collection = "matches",
  db = "wyscoutdutch",
  url= "mongodb://localhost"
)
conp=mongo(
  collection = "players",
  db = "wyscoutdutch",
  url= "mongodb://localhost"
)

# hent kampene
allmatches = conm$find(query = '{}',fields = '{}')
allDutch=allmatches %>% filter(competitionId==635)
allAjax=allDutch %>% filter(str_detect(label,"Ajax"))
#allAjax$res=gsub(".*,","",allAjax$label)
#allAjax$teams=gsub(",.*","",allAjax$label)

#AJAX STATS
# hent events for Ajax' kampe
# find _id
idv=allAjax[,'_id']
# testkampe
idvt=idv[1:3]
idvt=idv
query = jsonlite::toJSON(list(`_id` = list(`$in` = idvt)), auto_unbox = TRUE)
result=cong$find(query=query, fields = '{}')
testl=result$events
testdf=bind_rows(testl)
resdf=fromJSON(toJSON(testdf),flatten=T)

ajaxEvents=resdf %>% filter(team.name=="Ajax")
# only passes
ajaxEventsPasses=ajaxEvents %>% filter(type.primary=="pass")
# remove columns
av=colnames(ajaxEventsPasses)
av
avsub=av[1:30]
avsub2=avsub[-c(3:9)]
ajaxEventsPassesSub=ajaxEventsPasses[,avsub2]
ajaxEventsPassesSub$cat=unlist(lapply(ajaxEventsPassesSub$type.secondary, function(x) x[1]))


# find relevant pass-types
ddf=as.data.frame(table(ajaxEventsPassesSub$cat)) 
ddf=ddf %>% filter(Freq>410)
nv=unique(as.character(ddf$Var1))

pstat=ajaxEventsPassesSub %>% group_by(matchId, cat) %>% select(matchId,cat) %>% 
  filter(cat %in% nv) %>% 
  summarise(value=n(),.groups = "drop") %>% 
ungroup()


# collect stats
ajstat=ajaxEventsPassesSub %>% group_by(matchId) %>% mutate(
  passl=mean(pass.length),
  passvar=sd(pass.length),
  totpasses=n(),
  accratio=round((totpasses - sum(pass.accurate))/totpasses,3),
) %>% select(accratio,totpasses,matchId,passl,passvar) %>% unique() %>% ungroup()
colnames(ajstat)
ajstatLong=ajstat %>% pivot_longer(c(totpasses,passl,passvar,accratio),names_to = "cat",values_to = "value")

totstat=bind_rows(pstat,ajstatLong)

# now wide
totstat_wide=totstat %>% pivot_wider(names_from = "cat", values_from = "value")
totstat_wide_scaled=as.data.frame(scale(totstat_wide))

kmod=kmeans(totstat_wide_scaled, nstart = 10, centers =4)
fviz_cluster(kmod, data = totstat_wide_scaled)

dftwss=data.frame(k=1:20,twss=0)
for (i in (1:20)) {
  tmod=kmeans(totstat_wide_scaled,centers = i,nstart = 10)
  dftwss[i,'twss']=tmod$tot.withinss
}

plot(dftwss)

kmod=kmeans(totstat_wide_scaled, nstart = 10, centers =3)
kmod=kmeans(totstat_wide, nstart = 10, centers =3)
fviz_cluster(kmod, data = totstat_wide)
  
# pca
data.pca <- princomp(totstat_wide_scaled)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_pca_var(data.pca, col.var = "black")

# hcl
distm=dist(totstat_wide_scaled)
hi=hclust(distm,method = "complete")
plot(hi)
cut_avg <- cutree(hi, k = 3)
rect.hclust(hi , k = 3, border = 2:6)
