library(vistime)
library(ggplot2)

data <- read.csv(text="event,group,start,end,color
                       Data Collection,Phase,2021-07-01,2021-08-15,#c7e9c0
                       Model Building,Phase,2021-08-15,2021-10-15,#a1d99b
                       Dashboard Building, Phase, 2021-10-15,2021-12-15, #74c476
                       Workshop,Phase,2021-12-15,2021-12-31,#41ab5d
                       Matt Clark, Matt Clark,2021-07-01,2021-12-31,#bdbdbd
                       Community Forests Pemba, Community Forests Pemba,2021-07-01,2021-08-15,#bdbdbd
                         , Community Forests Pemba,2021-12-15,2021-12-31,#bdbdbd
                       Jeff Andrews, Jeff Andrews,2021-07-01,2021-10-15,#bdbdbd
                         , Jeff Andrews,2021-12-15,2021-12-31,#bdbdbd
                       Monique Borgerhoff-Mulder, Monique Borgerhoff-Mulder,2021-08-15,2021-10-15,#bdbdbd
                       Vicken Hillis, Vicken Hillis ,2021-08-15,2021-10-15,#bdbdbd"
                 
                       )



#vistime(data) 
#p<-gg_vistime(data, title = "Table 2: Timeline of proposed research activities and team members involved")
p <- gg_vistime(data, optimize_y = T)


p +theme_bw()+scale_x_datetime(date_breaks = "1 month")+ ggplot2::theme(
  plot.title = element_text(hjust = 0, size=12, color="black"),
  axis.text.x = element_text(size = 15, color = "black"),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





x<-lubridate::ymd(unique(data$start))
x<-as.POSIXct(x,format='%Y-%M-%D')
x<-format(as.POSIXct(x,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
p +theme_bw()+scale_x_datetime(breaks =x )+ ggplot2::theme(
  plot.title = element_text(hjust = 0, size=12, color="black"),
  axis.text.x = element_text(size = 15, color = "black"),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
