library(shiny)
library(datasets)
library(ggplot2)
library(gplots)
#source("~//Paudel et al. 2017 Paper 1//SourceCode//summarySE.R") # Summarize function to get the summary statistics;
cells = c("SKMEL5", "A375", "SKMEL19", "SKMEL28", "WM88", "WM793", "WM164", "A2058", "SC01", "SC07", "SC10")
concs = c(0, 2, 4, 8, 16, 32)

# Define UI for application that plots the data frame.
ui <- shinyUI(fluidPage(
  titlePanel("Welcome to Data Analysis using R Shiny created by Bishal Paudel"),
  tabsetPanel(
    tabPanel("Upload and subset data of interest",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose a CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                 selectInput('conc', label = "Choose a concentration", choices = c(unique(concs))),
                 selectInput('cell', label = "Choose a cellline to plot", choices = c(as.character(cells)))),
               mainPanel(
                 tableOutput('contents')
                 )
        )),
    tabPanel("Plots",
             pageWithSidebar(
               headerPanel('Your plot goes here'),
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 textInput('title', 'Title', ""),
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")),
               mainPanel(
                 plotOutput('MyPlot'))
      )
    )
  )
)
)

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    inFile <- input$file1 
    df <- read.csv(inFile$datapath)
    updateSelectInput(session, inputId = 'xcol',  label = 'X Variable',        choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol',  label = 'Y Variable',        choices = names(df), selected = names(df)[2])
    df_subset <- reactive({
      newD <- subset(df, CellLine == input$cell & conc == input$conc)
    return(newD)})
    return(df_subset())
  })
  output$contents <- renderTable({data()})
  output$MyPlot <- renderPlot({
    x <- data()[, c(input$xcol, input$ycol)]
    p1 = ggplot(data = x, aes(x=x[,1], y=x[,2]))
    p1 + theme_bw()+geom_smooth(span=.45, aes(group=1), method = "loess", size=.5, alpha=0.6, col="blue") + 
      labs(x=paste0(input$xcol), y=paste0(input$ycol))+ ylim((min(x[,2])-1), (max(x[,2])+1)) + ggtitle(paste0(input$title))
    #plot(x[,1], x[,2])
  })
})

shinyApp(ui, server)



