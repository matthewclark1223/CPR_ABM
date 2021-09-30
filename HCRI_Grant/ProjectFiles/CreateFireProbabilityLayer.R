stackRS<-raster::stack("PembaFiresAndPredictors.tif")
names(stackRS)<-c("roads_proximity","slope","soil_cat","fires2019")
StackDF <- raster::as.data.frame(stackRS,xy=TRUE)
StackDF<-na.omit(StackDF)
StackDF$soil_cat<-as.character(StackDF$soil_cat)
#ggplot(v,aes(x=roads_proximity,y=layer))+geom_jitter(alpha=0.05)+geom_smooth(method="gam")

#stdize function as per Gelman reccomendation
stdize<-function(x){
  (x-mean(x))/(2*sd(x))}
StackDF<-StackDF%>%mutate(slope_std=stdize(slope),road_proximity_std=stdize(roads_proximity))

########THIS NEEDS TO BE EDITED SO WE ONLY PREDICT FIRES ON BURNABLE LAND! #####################
# Filter out all non-burnable land before running the regression!!!!!!!!!!!!!

fit<-glm(fires2019~slope_std+road_proximity_std+soil_cat,family="binomial",data=StackDF)
#fit<-rstanarm::stan_glm(layer~stdize(slope)+stdize(roads_proximity),family="binomial",data=v,chains=1)
summary(fit)

StackDF$predFire<-predict(fit,newdata=StackDF,type="response")

predFireRas<-rasterFromXYZ(StackDF[c(1,2,9)],crs = crs(stackRS) )
predFireRas<-resample(predFireRas,stackRS)
stackRS<-stack(stackRS,predFireRas)
raster::writeRaster(predFireRas,"PredFire2019.tif")

