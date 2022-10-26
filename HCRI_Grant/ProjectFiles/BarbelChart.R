library(raster)
library(tidyverse)
library(sf)
#ObsData2<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
 #                             "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
  #                            "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
   #                  PixelsConverted2018_2021=c(614,1385,941,1321,1240,1182,471,726,1124,696,
    #                                            762,1758,1194,1078,624,988,897,1473,441))

ObsData2<-df

Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")

#Load 2018 observed landcover
LC2018<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2018.tif")


ObsData2$Pixels2018<-rep(NA,nrow(ObsData2))


for(i in 1:nrow(ObsData2)){
  shehia<-ObsData2[i,]$Shehia
  Outline<-Pemba%>%filter(NAME_3 == shehia)
  
  LC2018_clipped<- crop(LC2018, extent(Outline)) #clip the area
  LC2018_clipped <- mask(LC2018_clipped, Outline) #This deals with NAs
  
  #correct number of conversions
  ObsData2[i,]$Pixels2018<-length(which(LC2018_clipped[] ==5))
}
  



#ObsData2$ExtrinsicRates<-c(0.647,2.06,2.01,9.23,9.23,8.67,5.06,4.20,
 #                        5.65,7.69,11.3,9.02,10.0,6.18,15.5,11.0,9.73,6.27,2.98) 


#ObsData3$ExtrinsicRates<-c(0.0355,0.0805,1.21,8.43,8.03,5.57,3.57,2.70,
 #                          2.57,3.01,8.54,4.80,5.93,2.73,11.3,9.57,7.79,3.25,1.12) #obsdat1

ExtrinsicRates<-dataFiltered%>%group_by(Shehia)%>%summarise(medy=median(prior))%>%arrange(desc(medy) )
ObsData2<-base::merge(x=ObsData2,y=ExtrinsicRates,by="Shehia", all.x=TRUE )
ObsData2[is.na(ObsData2$medy),]$medy<-0
ObsData2$ExtrinsicRates<-ObsData2$medy

ObsData<-ObsData2%>%mutate(ConvertedPercentTotal=PixelsConverted2018_2021/Pixels2018)%>%
  mutate(ConvertedPercentYearly = ConvertedPercentTotal/3)%>%
  mutate(ExtrinsicRates=ExtrinsicRates*0.01)%>%
  mutate(EndogenousRate = ConvertedPercentYearly-ExtrinsicRates)

ObsData%>%
  mutate(Shehia = factor(Shehia, levels = rev(yax)))%>%
ggplot(., aes(y=Shehia, x=EndogenousRate, xend=ConvertedPercentYearly)) +
  ggalt::geom_dumbbell(size=3.5, color="#e3e2e1",
                colour_x = "black", colour_xend = "#0570b0",
                dot_guide=TRUE, dot_guide_size=0.25)+theme_classic()+
  annotate("text", x = 0.069, y = "Mvumoni", label = "Driven by soil quality",vjust = -1,fontface="bold",color="black",size=4.2)+
  annotate("text", x = 0.0895, y = "Mvumoni", label = "Total observed loss",vjust = -1,fontface="bold",color="#0570b0",size=4.2)+
   xlab("Yearly Loss of Coral Rag Forest Cover")+scale_x_continuous(labels=scales::percent)+
  theme(axis.title = element_text(color="black",size=18),
         axis.text = element_text(color="black",size=14),
         axis.title.y = element_text(face = "italic"))


mean(ObsData$ConvertedPercentYearly)
range(ObsData$EndogenousRate)
mean(ObsData$ExtrinsicRates)



