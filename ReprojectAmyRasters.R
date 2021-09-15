library(raster)
raster::plot(r_merged) #this is the aggregated fire data
raster::plot(RoadProx)
crs(r_merged)
crs(RoadProx)

#This works!!!
x<-projectRaster(RoadProx,crs=crs(r_merged))
plot(x)
y<-resample(x,r_merged,method="bilinear")
plot(y)


z<-resample(slope,r_merged,method="bilinear")
plot(z)

plot(slope)

s <- raster::stack(y, z,r_merged)
v <- data.frame(na.omit(values(s)))

ggplot(v,aes(x=roads_proximity,y=layer))+geom_jitter(alpha=0.05)+geom_smooth(method="gam")

#stdize function as per Gelman reccomendation
stdize<-function(x){
  (x-mean(x))/(2*sd(x))}
fit<-rstanarm::stan_glm(layer~stdize(slope)+stdize(roads_proximity),family="binomial",data=v,chains=1)
summary(fit)

posterior <- as.matrix(fit)

bayesplot:: mcmc_intervals (posterior,pars = c("stdize(slope)","stdize(roads_proximity)"),
                            prob = 0.99) 