#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#setwd("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandUsePredictionsApp")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Pemba Island Coral Rag Landcover Predictions Application"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      h4("Welcome!"),
      h5("This application allows users to use the model presented in Clark et al. (in review)
         to predict coral rag forest cover for 19 wards (Shehia) in Pemba Island, Tanzania for the years
         2021 through 2025."),
     
      h4("Instructions"),
      h5("The sliders below represent different alternative scenarios for rotational agriculture possible through
         investment in conservation agricutlure. Use the sliders to predict how the landscape is 
         expected to change based on small changes in how cycles of rotational agriculture are scheduled"),
      
      sliderInput("fallowtime",
                  "Years plots are left fallow",
                  min = 2,
                  max = 4,
                  value = 3),
      sliderInput("aglimit",
                  "Years plots are planted and harvested",
                  min = 1,
                  max = 3,
                  value = 2),
      
      selectInput("wards",
                  "Ward (Shehia)",
                  c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                      "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                      "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),selected ="Chambani",multiple=FALSE)
      
     
    ),
    
    mainPanel(
    
    #new
    tabsetPanel(type = "tabs",
                tabPanel("Expected land conversions 2021 - 2025", imageOutput("Plot")),
                tabPanel("Animation",plotOutput("Gif")),
                tabPanel("Further information",
                         h3("Team members"),
                         p("This application was produced as a collaboration between researchers from Boise State University, 
                           The Max Planck Institute for Evolutionary Anthropology, and Community Forests Pemba, a Pemban nonprofit 
                           focused on community-based conservation. "),
                         br(),
                         
                         p("This aplication relies on the model shown in 'Causal attribution of
agricultural expansion in a small island system using approximate Bayesian computation' currently in review at Land Use Policy. Preprint
found here: https://www.biorxiv.org/content/10.1101/2023.01.20.524853v1."),
                         br(),
                         p("Full citation is:  Clark, M., Andrews, J., Kolarik, N., Omar, M., Hillis, V. (In review). Causal attribution of
agricultural expansion in a small island system using approximate Bayesian computation. Land
Use Policy. https://www.biorxiv.org/content/10.1101/2023.01.20.524853v1.
")))
    )
   
      

      
      #end new
      
      
    
  )
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 

  
  output$Plot <- renderImage({
    fallow<-input$fallowtime
    fertile<-input$aglimit
    ward<-input$wards
    PATH=paste0("Plots/",ward,"_",fallow,"_",fertile,".png")
    
    list(src=PATH,height='500px',width='600px')
    #PATH<-paste0("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandUsePredictionsApp/Gifs/",
     #      input$wards,"_",input$fallowtime,"_",input$aglimit,".gif")
    #img(src="Gifs/Fundo_2_1.gif", align = "left",height='250px',width='500px')
    
     } ,deleteFile = FALSE )
  
  ####################################
  output$Gif <- renderImage({
    
  
    fallow<-2
    fertile<-1
    ward<-input$wards
    PATH=paste0("Gifs/",ward,"_",fallow,"_",fertile,".gif")
    
    list(src=PATH,height='500px',width='600px')
    #PATH<-paste0("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandUsePredictionsApp/Gifs/",
    #      input$wards,"_",input$fallowtime,"_",input$aglimit,".gif")
    #img(src="Gifs/Fundo_2_1.gif", align = "left",height='250px',width='500px')
    
  } ,deleteFile = FALSE )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)