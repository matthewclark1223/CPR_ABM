devtools::install_github("dkahle/ggmap")
library(ggmap)
library(maps)
library(ggnetwork)
library(geomnet)
library(GGally)

sites<-data.frame(Site=c("Makangale","Before Ningeze Forest","Msituu Kuu","bottom just for mapping"),
                  lon=c(39.690753,39.710927,39.837124,39.651140),
                  lat=c(-4.906206,-4.940105,-4.997158,-5.477887))

pbbox <- make_bbox(lon = sites$lon, lat = sites$lat, f = .1)
pbbox

#this might not be necessary
sq_map <- get_map(location = pbbox, maptype = "satellite", source = "google")
##
ll_means <- sapply(sites[2:3], mean)
pb_map2 <- get_map(location = ll_means,  maptype =  "satellite", source = "google", zoom = 10)
ggmap(pb_map2) + 
  geom_point(data = sites[1:3,], color = "red", size = 4) +
  geom_text(data = sites[1:3,], aes(label = paste("  ", as.character(Site), sep="")), size=6, hjust = 0, color = "white")


