library(sf)
library(tidyverse)

Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")

CofMAShehia<-c("Changaweni","Fundo","Gando","Kambini","Kangani","Kifundi","Kisiwa Panza",
               "Mgogoni","Michenzani","Mjimbini", "Mjini Wingwi","Msuka Magharibi","Mtambwe Kaskazini",
               "Mtambwe Kusini","Shumba Mjini","Tondooni","Tumbe Magharibi","Makangale")


EndogCofma<-c("Pujini","Shamiani","Wambaa","Chonga","Msuka Mashariki","Shengejuu",
              "Tumbe Mashariki","Ukunjwi","Vitongoji","Kojani","Ng'ambwa","Makoongwe")


Pemba$Cofma<-ifelse(Pemba$NAME_3 %in% CofMAShehia, "Original",
                    ifelse(Pemba$NAME_3 %in% EndogCofma,"Endogenous","None"))

ggplot(data=Pemba)+
  geom_sf(aes(fill=Cofma))

leakage<-read_csv("~/Pemba_Project/ReportedLeakage2019ENDOG_CofmasOnly.csv")
graph<-igraph::graph_from_edgelist(as.matrix(leakage), directed = TRUE)
plot(graph)

centroids <- st_centroid(Pemba)
coords<-as.data.frame(st_coordinates(centroids))
coords$Shehia<-Pemba$NAME_3

ggplot(data=Pemba)+
  geom_sf(aes(fill=Cofma))+
  geom_point(data=coords,aes(x=X,y=Y),color="red")

V(graph)$lon <- sapply(V(graph)$name, function(x) coords$X[coords$Shehia == x])
V(graph)$lat <- sapply(V(graph)$name, function(x) coords$Y[coords$Shehia == x])

 


library(GGally)
cols<-c("Endogenous"="#b2df8a","Original"="#a6cee3","None"="White")
gg<-ggplot(data=Pemba)+
  geom_sf(aes(fill=Cofma))+
            scale_fill_manual(values=c(cols))+theme_bw()

ggnetworkmap(gg,graph,
             arrow.size = 0.2,
             size=0,segment.color = "black"
             )

