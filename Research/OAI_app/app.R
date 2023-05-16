library(shiny)
library(tidyverse)

# Load Data
DATAPATH <- Sys.getenv("OAI_DATA")
if (DATAPATH == "" ) stop( "Please add datapath to OAI_DATA" )

source("../OAI_LoadData.R", chdir = T)

bsln <- getBaselineData(DATAPATH)
evnts <- getEvents(DATAPATH)

setBslnClstr <- function(clstr = "") {
  data_full <- getCompleteData(DATAPATH, bsln_data = bsln, evnt_data = evnts, cluster = clstr)
  bsln$EVNT <- data_full$EVNT
  print(paste("Baseline Clusters Updated, Levels:", length(levels(bsln$EVNT))))
  return(bsln)
}

col_all <- c("AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP", "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "CESD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", "EDCV", "P01OAGRD", "P02JBMPCV_NEW", "DPRSD", "CEMPLOY_NW", "RACE_O")

col_num <-  c("ID", "AGE", "PASE", "WOMADL", "WOMKP", "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "CESD")

col_fac <-  c("SEX", "MEDINS", "DPRSD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", "CEMPLOY_NW", "EDCV", "P01OAGRD", "P02JBMPCV_NEW", "RACE_O")

clusters <- c("None", "K.5.Clusters", "K.4.Clusters")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("OAI Data Analysis"),

    sidebarLayout(
        sidebarPanel(
            radioButtons("c", "Cluster", c("None" = "n", "K.5.Clusters" = "k5", "K.4.Clusters" = "k4"), selected = "n"),
            hr(),
            selectInput('p', 'Predictor', choices = col_all)
        ),

        # Plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  d <- reactiveVal()
  
  observeEvent(input$c, {
    if(input$c == "k5") {
      d(setBslnClstr(clstr = "K.5.Clusters"))
    }
    else if(input$c == "k4") {
      d(setBslnClstr(clstr = "K.4.Clusters"))
    }
    else {
      d(setBslnClstr())
    }
    
  })
  
  output$distPlot <- renderPlot({ 
    if( input$p %in% col_num ) {
      hist(d()[,input$p], main = paste("Distribution of", input$p), xlab = input$p)
    }
    if( input$p %in% col_fac ) {
      ggplot(d(), aes(input$p, fill = EVNT)) + geom_bar()
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
