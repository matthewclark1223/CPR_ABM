library(raster)
library(RColorBrewer)
prec<-getData('worldclim',var = 'prec',res = 0.5, lon=39.74,lat=-5.313)
tmin<-getData('worldclim',var = 'tmin',res = 0.5, lon=39.74,lat=-5.313)
tmax<-getData('worldclim',var = 'tmax',res = 0.5, lon=39.74,lat=-5.313)
bio<-getData('worldclim',var = 'bio',res = 0.5, lon=39.74,lat=-5.313)
save(bio,file="bio.rda")

writeRaster(bio,filename="bio.tif")

bio = stack("bio.tif")

scale.parameter = 0.3  # scaling paramter. less than 1 is zooming in, more than 1 zooming out. 
xshift = -7 # Shift to right in map units. 
yshift = 10  # Shift to left in map units. 
original.bbox = bbox(prec) # Pass bbox of your Spatial* Object. 
# Just copy-paste the following
edges = original.bbox

edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1, 
                                                                             ]) + xshift
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2, 
                                                                             ]) + yshift

#Monthly average plots
months<-c( "January", "February", "March", "April", 
                                     "May", "June", "July", "August", "September", "October", 
                                     "November", "December")

names(tmin)<-months
names(tmax)<-months
names(prec)<-months

tmin<-tmin/10
tmax<-tmax/10

prec.pal <- brewer.pal(n = 7, name = "Blues")
spplot(prec$January, 
       col.regions = prec.pal, 
       cuts = 6,main="Eastern Tanzania & Zanzibar Precipitation",
       sub="Average Precipitation from 1950 - 2000", xlim = edges[1, ], ylim = edges[2, ])


t.pal <- rev(brewer.pal(n = 7, name = "RdYlBu"))
spplot(tmin, 
       col.regions = t.pal, 
       cuts = 6,main="Eastern Tanzania & Zanzibar Minimum Temperature",
       sub="Average Minimum Temperature from 1950 - 2000", xlim = edges[1, ], ylim = edges[2, ])


spplot(tmax, 
       col.regions = t.pal, 
       cuts = 6,main="Eastern Tanzania & Zanzibar Maximum Temperature",
       sub="Average Maximum Temperature from 1950 - 2000", xlim = edges[1, ], ylim = edges[2, ])



#Overall average plots
#BIO1 = Annual Mean Temperature
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp – min temp))
#BIO3 = Isothermality (BIO2/BIO7) (* 100)
#BIO4 = Temperature Seasonality (standard deviation *100)
#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month
#BIO7 = Temperature Annual Range (BIO5-BIO6)
#BIO8 = Mean Temperature of Wettest Quarter
#BIO9 = Mean Temperature of Driest Quarter
#BIO10 = Mean Temperature of Warmest Quarter
#BIO11 = Mean Temperature of Coldest Quarter
#BIO12 = Annual Precipitation
#BIO13 = Precipitation of Wettest Month
#BIO14 = Precipitation of Driest Month
#BIO15 = Precipitation Seasonality (Coefficient of Variation)
#BIO16 = Precipitation of Wettest Quarter
#BIO17 = Precipitation of Driest Quarter
#BIO18 = Precipitation of Warmest Quarter
#BIO19 = Precipitation of Coldest Quarter


##NOTWORKINGAFTER HERE

spplot((bio$bio1_37)/10, 
       col.regions = t.pal, 
       cuts = 6,main="Eastern Tanzania & Zanzibar Maximum Temperature",
       sub="Average Maximum Temperature from 1950 - 2000", xlim = edges[1, ], ylim = edges[2, ])
