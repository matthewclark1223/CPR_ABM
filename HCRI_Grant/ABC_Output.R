Output<-readRDS("~/Pemba_Project/HCRI_Grant/ProjectFiles/Output.RDS")
S<-readRDS("~/Pemba_Project/HCRI_Grant/ProjectFiles/S.RDS")
data<-as.data.frame(S)
names(data)[1:2]<-c("Shehia","prior")



data$estimate<-unlist(Output)

#This is not good above


###########

ObsData1<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                             "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                             "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
                    PixelsConverted2018_2021=c(961,2223,1139,3448,3579,1503,568,951,1491,910,1203,
                                               2122,1391,1352,943,1484,1302,1650,780))


ObsData2<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                         "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                         "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
                PixelsConverted2018_2021=c(614,1385,941,1321,1240,1182,471,726,1124,696,
                                           762,1758,1194,1078,624,988,897,1473,441))


ObsData3<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                    "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                    "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
           PixelsConverted2018_2021=c(1431,3717,1535,3663,3899,1914,654,1169,1993,1209,1367,3019,
                                      1839,1859,1102,1602,1480,2336,1051))

ObsData2<-df

data2<-merge(data,ObsData2,all.x = TRUE,by="Shehia" )



ggplot(data=data2,aes(y=Shehia))+
  ggridges::geom_density_ridges(aes(x=estimate))+
  geom_point(aes(x=PixelsConverted2018_2021),color="red",shape="|",size=4,
             position = position_nudge(y = 0.5) )+
  xlim(0,5000)+theme_bw()



dataFiltered<-data2%>%
  filter(estimate <= (PixelsConverted2018_2021+0.10*PixelsConverted2018_2021) &
           estimate >= (PixelsConverted2018_2021-0.10*PixelsConverted2018_2021))


x<-dataFiltered%>%group_by(Shehia)%>%summarise(medy=median(estimate))%>%arrange(desc(medy) )

yax<-c("Maziwa Ng'ombe","Mjini Ole","Kangagani","Uwandani","Ole","Pujini","Kojani","Vitongoji",         
       "Shumba Mjini","Mjini Wingwi","Mvumoni","Dodo",
       "Chambani","Kiwani","Kibokoni","Fundo","Muambe","Jombwe","Shamiani")

data2%>%
  mutate(Shehia = factor(Shehia, levels = rev(yax)))%>%
  #mutate(Shehia = fct_relevel(Shehia, levels = as.character(x$Shehia)))%>%
ggplot(data=.,aes(y=Shehia))+
  ggridges::geom_density_ridges(aes(x=estimate,height=stat(density)),
                                stat = "binline", bins = 40, scale = 0.98, draw_baseline = FALSE,
                                fill="#525252")+
  geom_point(aes(x=PixelsConverted2018_2021),color="red",shape="|",size=8,
             position = position_nudge(y = 0.5) )+
  annotate("text", x = 5000, y = "Fundo", label = "Estimates from prior",vjust = -0.85,color="#525252",size=5)+
  annotate("text", x = 460, y = "Uwandani", label = "Observed pixels converted",vjust = -1,color="red",size=5)+
  scale_x_log10(label=scales::comma)+
  xlab("Pixels Converted")+
  scale_fill_gradient(low = "#efedf5",
    high = "#3f007d",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill",name="Tail probability "
  )+theme_bw()+theme(axis.title = element_text(color="black",size=18),
                                axis.text = element_text(color="black",size=14),
                                panel.grid.major.x   = element_blank(),
                                panel.grid.minor.x   = element_blank(),
                                axis.title.y = element_text(face = "italic"))







###########plot with rates


x<-dataFiltered%>%group_by(Shehia)%>%summarise(medy=median(prior))%>%arrange(desc(medy) )
data2%>%
  #mutate(Shehia = fct_relevel(Shehia, levels = as.character(x$Shehia)))%>%
  mutate(Shehia = factor(Shehia, levels = rev(yax)))%>%
  ggplot(data=.,aes(y=Shehia))+
  #ggridges::geom_density_ridges(aes(x=prior), scale = 1)+
  ggridges::geom_density_ridges(aes(x=prior,height=stat(density)),
                                stat = "binline", bins = 30, scale = 0.98, draw_baseline = FALSE,
                                fill="#525252")+
  ggridges:: stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                                 data=dataFiltered,
                                 aes(x=prior,fill = 0.5 - abs(0.5 - stat(ecdf))),
                                 quantile_lines = TRUE, quantiles = 2,size=0.5)+
  xlab("Externally Driven Forest Conversion Rate")+
  scale_fill_gradient(low = "#efedf5",
                      high = "#3f007d",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",name="Estimate\ntail probability "
  )+
   annotate("text", x = 2.3, y = "Maziwa Ng'ombe", label = "Prior",vjust = -0.4,color="white",size=5)+
  scale_x_continuous(breaks=c(0,5,10,15),labels = c("0%","5%","10%","15%"),limits=c(0,15))+
  #scale_y_discrete(breaks = rev(yax))+
  theme_bw()+theme(axis.title = element_text(color="black",size=18),
                                axis.text = element_text(color="black",size=14),
                                panel.grid.major.x   = element_blank(),
                                panel.grid.minor.x   = element_blank(),
                                axis.title.y = element_text(face = "italic"))



###






