#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(gganimate)

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("title"),
    
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("years",
                      "Years to predict",
                      min = 1,
                      max = 5,
                      value = 1),
          sliderInput("fallowtime",
                        "Years plots must be left fallow",
                        min = 1,
                        max = 5,
                        value = 3),
            sliderInput("aglimit",
                        "Years ag plots can be productive for",
                        min = 1,
                        max = 5,
                        value = 2),
            
            selectInput("wards",
                        "Shehia",
                        c("Kojani","Kangagani","Fundo"),selected ="Kojani",multiple=FALSE),
          actionButton("goButton", "Go!"),
          p("Click the button to update the value displayed in the main panel.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          #withSpinner(imageOutput("Map"), type = 2),
           
          #new
          tabsetPanel(type = "tabs",
                      tabPanel("Gif", withSpinner(imageOutput("Map"), type = 2)),
                      tabPanel("Summary"),
                      tabPanel("Table"))
          
          #end new
          
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    output$Map <- renderImage({
      
      LC2021<-raster::raster("pemmyLC2021.tif")
      
      
      
      replacefun<-function(r){
        r[]<-ifelse(LC2021[] == 0,0,r[]) #Mangrove
        r[]<-ifelse(LC2021[] == 1,10,r[]) #High Forest            
        r[]<-ifelse(LC2021[] == 3,30,r[]) #Urban
        r[]<-ifelse(LC2021[] == 4,40,r[]) #Bare
        r[]<-ifelse(LC2021[] == 6,60,r[]) #OWV/agroforestry
        r[]<-ifelse(LC2021[] == 7,70,r[])#H20
        r[]<-ifelse(r[]==3,1,r[]) #Make only one Ag class
        return(r[])} 
      
      #Extrinsic growth rates
      rates<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                                 "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                                 "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
                        Rate=c(1.6,2.6,4.6,1.3,2.5,3.9,3.6,3.2,
                                                   1.7,1.9,2.9,2.6,3.8,3.3,2.2,1.1,2.6,3.1,4.1))
      
      source("Full_LandUse_ABM_FORAPP.R")
      LU_AMB(YearsPast2021 = input$years, #years (timesteps) to run model
                                            Wards = input$wards,  #character vector or wards to model. Default is full model
                                            FallowTime = input$fallowtime, #time (in years) it takes for fallow land to recharge
                                            AgLimit = input$aglimit,
             IntrinsicExp=rates[rates$Shehia==input$wards,]$Rate)
        
        YearsPast2021=input$years

      LndCvr<-raster::stack(rstack[[c(2,(length(names(rstack))-YearsPast2021+1):length(names(rstack)))]])
      
      LC2021<- crop(LC2021, extent(LndCvr$LndCvr2021))
      LC2021 <- mask(LC2021, LndCvr$LndCvr2021)
      
      
      

      
      
      
      for(i in 1:length(names(LndCvr))){
        LndCvr[[i]]<-replacefun(LndCvr[[i]])# do it for all layers in Raster stack (all years)
      }
      
      
      
      z<-list()
      for(i in 1:length(names(LndCvr))){
        d<-raster::as.data.frame(LndCvr[[i]],xy=TRUE)
        names(d)[3]<-"layer"
        d$year<-names(LndCvr[[i]])
        d<-na.omit(d)
        z[[i]]<-d
        
      }
      
      z<-do.call(rbind, z)
      
      
      cols <- c("0" = "#c7e9c0", "10" = "#00441b", "2" = "#e7298a", "30" = "#4d4d4d",
                "40"= "#ffffbf","1"="#fdbf6f","60"="#41ab5d","70"="#4575b4")
     
    plot<- ggplot(data = z)+
        geom_tile(aes(x = x, y = y,fill=as.character(layer))) +
      scale_fill_manual(values = cols,labels = c("Mangrove","High Forest","Coral rag forest","Urban","Bare",
                                                 "Agriculture","Agroforestry & \nother woody veg", "Water"),
                        name="Landcover")+
        theme_bw()+ labs(title = 'Year: {closest_state}')
    
      anim<-plot + 
        transition_states(as.factor(year),transition_length = 0,state_length=1)
      
      anim_save("outfile.gif", gganimate::animate(anim)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif", contentType = "image/gif")
      
    },deleteFile = FALSE )
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
