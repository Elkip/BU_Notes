library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

# Load Data
DATAPATH <- Sys.getenv("OAI_DATA")
if (DATAPATH == "" ) stop( "Please add datapath to OAI_DATA" )

source("/home/elkip/Workspace/BU_Notes/Research/OAI_LoadData.R", chdir = T)

bsln <- getBaselineData(DATAPATH)
evnts <- getEvents(DATAPATH)

setBslnClstr <- function(clstr = "") {
  data_full <- getCompleteData(DATAPATH, bsln_data = bsln, evnt_data = evnts, cluster = clstr)
  bsln$EVNT <- data_full$EVNT
  print(paste("Baseline Clusters Updated, Levels:", length(levels(bsln$EVNT))))
  return(bsln)
}

setBstData <- function() {
  tmp <- bsln %>% select(c(predictors_best, "EVNT"))
  return(na.omit(tmp))
}

col_all <- c("AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP", "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "CESD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", "EDCV", "P01OAGRD", "P02JBMPCV_NEW", "DPRSD", "CEMPLOY_NW", "RACE_O")

col_num <-  c("ID", "AGE", "PASE", "WOMADL", "WOMKP", "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "CESD")

col_fac <-  c("SEX", "MEDINS", "DPRSD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", "CEMPLOY_NW", "EDCV", "P01OAGRD", "P02JBMPCV_NEW", "RACE_O")

col_bst <- c("AGE", "SEX", "RACE_O", "PASE", "WOMKP", "WOMSTF", "HEIGHT", "WEIGHT", "V00WTMAXKG", "NSAID", "P01OAGRD_Severe", "P01OAGRD_Moderate", "P01OAGRD_Mild", "P01OAGRD_Possible", "EDCV_HSDeg", "EDCV_GradDeg", "EDCV_UGDeg", "CESD", "Surg_Inj_Hist")

clusters <- c("None", "K.5.Clusters", "K.4.Clusters")

ui <- dashboardPage(
  dashboardHeader(title = "OAI Data Analysis"),
  
  dashboardSidebar(
    radioButtons(
      "c",
      "Cluster",
      c("None" = "n", "K.5.Clusters" = "k5", "K.4.Clusters" = "k4"),
      selected = "n"
    ),
    sidebarMenu(
      menuItem("Data Analysis", tabName = "data"),
      menuItem("Model Analysis", tabName = "model")
    )
  ),
  
  dashboardBody(tabItems(
    tabItem(tabName = "data",
            fluidRow(
              selectInput('p', 'Predictor', choices = col_all, selected = "AGE")
            ),
            fluidRow(box(
              plotOutput("distPlot")
            ),
            box(
              tabsetPanel(tabPanel("Summary", dataTableOutput("sumData")))
            ))),
    tabItem(tabName = "model",
            fluidRow(
              numericInput("age", "AGE", 60),
              selectInput('sex', 'SEX', c("Male" = 1, "Female" = 2)),
              selectInput('race', 'RACE', c("White" = 0, "Non-White" = 1)),
              sliderInput('cesd', 'CESD', min = 0, max = 60, value = 0),
              sliderInput('womkp', 'WOMKP', min = 0, max = 20, value = 0),
              sliderInput('womstf', 'WOMSTF', min = 0, max = 8, value = 0),
              numericInput("pase", "PASE", 0),
              numericInput('height', 'HEIGHT', 100),
              numericInput('weight', 'WEIGHT', 100),
              numericInput('max', 'V00WTMAXKG', 100),
              selectInput('nsaid', 'NSAID', c("No" = 0, "Yes" = 1)),       
              selectInput('grd', 'OAI GRADE', c("None" = 0, "Possible" = 1, "Mild" = 2,
                                                "Moderate" = 3, "Severe" = 4)),   
              selectInput('edu', 'Education', c("None" = 0, "High School" = 1,
                                                "Undergrad" = 2, "Grad School" = 3)),
              selectInput('surj', 'Surg_Inj_Hist', c("No" = 0, "Yes" = 1)),
              actionButton("sbmt", "Submit")
            ),
            fluidRow(box(
              textAreaInput("Hi", "hi")
            )))
  ))
)

server <- function(input, output) {
 
  d <- reactiveVal()
  bst <- reactiveVal()
  
  observeEvent(input$c, {
    if(input$c == "k5") {
      d(setBslnClstr(clstr = "K.5.Clusters"))
      bst(setBstData)
    }
    else if(input$c == "k4") {
      d(setBslnClstr(clstr = "K.4.Clusters"))
      bst(setBstData)
    }
    else {
      d(setBslnClstr())
      bst(setBstData)
    }
  })
  
  ### Data Analysis
  output$distPlot <- renderPlot({ 
    if( input$p %in% col_num ) {
      hist(d()[,input$p], main = paste("Distribution of", input$p), xlab = input$p)
      
      output$sumData <- renderDataTable({
        datatable( d() %>%
                     filter(!is.na(get(input$p))) %>%
                     group_by(EVNT) %>%
                     summarise(avg = mean(get(input$p)), sd = sd(get(input$p)), min = min(get(input$p)), max = max(get(input$p))), options = list(dom = 't')
                   ) 
      })
    }
    if( input$p %in% col_fac ) {
        output$sumData <- renderDataTable({
        datatable( d() %>%
                     filter(!is.na(get(input$p))) %>%
                     group_by(EVNT) %>%
                     summarize(freq = n(), SUM = sum(get(input$p) == 1), perc = SUM/freq*100), options = list(dom = 't')
                   )
      })
      ggplot(d(), aes(get(input$p), fill = EVNT)) + labs(title = input$p, x = input$p) + geom_bar()
    }
  })
  
  ### Model Analysis
  new_data =  data.frame(AGE=50, SEX="1", RACE_O="0", CESD=0, CEMPLOY_NW="0", PASE=0, WOMKP=0,WOMSTF=0, HEIGHT=0, WEIGHT=0, NSAID="0", P01OAGRD_Severe="0", P01OAGRD_Moderate="0", P01OAGRD_Mild="0", P01OAGRD_Possible="0", EDCV_HSDeg="0", EDCV_GradDeg="0", EDCV_UGDeg="0", V00WTMAXKG=0, Surg_Inj_Hist="0")
  # predict(mod_best, type="probs", newdata = new_data)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
