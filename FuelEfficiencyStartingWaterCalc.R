#formula for calculating starting mass of water for water boiling test.
#formula from Clean Cooking Alliance

WaterStart<-function(temp_C){
  Water_kg<-(0.001*temp_C^2)+(0.0381*temp_C)+4.2018
  return(round(Water_kg,digits=2))
}


WaterStart(29)


calc_table<-data.frame(StartingTemperature=4:30,
                       WaterMassNeeded=rep(NA,length(temps)))

for(i in 1:nrow(calc_table)){
  calc_table[i,]$WaterMassNeeded<-WaterStart(calc_table[i,]$StartingTemperature)
}


library(formattable)
View(calc_table)
