library(raster)
Pem2018LC<-raster::raster("Pem_2018_LC_20m.tif")
stackRS<-raster::stack("PembaFiresAndPredictors.tif")
#Make function to reproject LC to the stack DF
preprocess_raster<-function(raster,method){
  if(method=="ngb"){ x<-raster::projectRaster(raster,crs=crs(stackRS),method="ngb")}
  if(method=="bilinear"){ x<-raster::projectRaster(raster,crs=crs(stackRS))}
  return(resample(x,stackRS,method=method))
}
Pem2018LC<-preprocess_raster(Pem2018LC,method="ngb")
Pem2018LC<-mask(Pem2018LC,stackRS$PembaFiresAndPredictors.1)
stackRS<-stack(stackRS,Pem2018LC)
names(stackRS)<-c("roads_proximity","slope","soil_cat","fires2019","LandCover2018")
StackDF <- raster::as.data.frame(stackRS,xy=TRUE)
StackDF<-na.omit(StackDF)
StackDF$soil_cat<-as.character(StackDF$soil_cat)
StackDF$LandCover2018<-as.character(StackDF$LandCover2018)

#ggplot(v,aes(x=roads_proximity,y=layer))+geom_jitter(alpha=0.05)+geom_smooth(method="gam")

#stdize function as per Gelman reccomendation
stdize<-function(x){
  (x-mean(x))/(2*sd(x))}
StackDF<-StackDF%>%filter(LandCover2018%in%c("5","1"))%>% #Only include burnable pixels
  mutate(slope_std=stdize(slope),road_proximity_std=stdize(roads_proximity))

########THIS NEEDS TO BE EDITED SO WE ONLY PREDICT FIRES ON BURNABLE LAND! #####################
# Filter out all non-burnable land before running the regression!!!!!!!!!!!!!

fit<-glm(fires2019~slope_std+road_proximity_std+soil_cat+LandCover2018,family="binomial",data=StackDF)
#fit<-rstanarm::stan_glm(layer~stdize(slope)+stdize(roads_proximity),family="binomial",data=v,chains=1)
summary(fit)

StackDF$predFire<-predict(fit,newdata=StackDF,type="response")

predFireRas<-rasterFromXYZ(StackDF[c(1,2,9)],crs = crs(stackRS) )
predFireRas<-resample(predFireRas,stackRS)
stackRS<-stack(stackRS,predFireRas)
raster::writeRaster(predFireRas,"PredFire2019.tif",overwrite=T)

