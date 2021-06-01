pp<-raster("00N_030E_tCO2_pixel_AGB_masked_by_loss.tif")
pp<-crop(pp,z)
plot(pp)

Biomass<-raster("AG_Biomass_year2000.tif")
Biomass<-crop(Biomass,z)
plot(Biomass)



pp[is.na(pp[])] <- 0

BM_model <- function(r, Time) { #this is the very simple model 
  outputs <- vector("list", Time) #list of length 'time'
  outputs[[1]]<-r #the first object in the list is the initialization raster
  
  for(t in 2:19) { #2001 - 2018
    r_old<-outputs[[t-1]] #raster from last time step is now the thing getting things done to it
    new_bm<-r_old-((pp/18)*2)
    p<-new_bm<0
    new_bm[p]<-0 #dont let biomass go below 0
  outputs[[t]] <-new_bm}
  return(outputs) #return rasters as list
}

Mod <- BM_model(r = Biomass, Time = 19) #run

da<-stack(Mod)
names(da)<-paste0("Year.",as.character(2000:2018))
#levelplot(da)
animate(da)

animation::saveGIF(animate(da))

View(da[[2]])
