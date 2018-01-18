library(shiny)
library(datasets)
library(ggplot2)
library(gplots)
source("summarySE.R") # Summarize function to get the summary statistics;
cells = c("SKMEL5", "A375", "SKMEL28", "WM88", "WM793", "WM164", "A2058", "WM115", "WM983B", "WM1799", "WM2664")
treatment = c("PLX4720",    "Oligomycin", "Oxamic") 
dates = c("20150114", "20150314", "20150212", "20141130", "20150505", "20140519")

# Define UI for application that plots the data frame.
ui <- shinyUI(fluidPage(
  titlePanel("Welcome to Data Analysis using R Shiny created by Bishal Paudel"),
  tabsetPanel(
    tabPanel("Welcome to data analysis platform",
             titlePanel("Upload your data and visualize them."),
             mainPanel(
                  tags$p("In this application, you will upload your normalized cell count data from high-throughput image analysis. 
                  Example set provided with this application is obtained from CellaVista (https://synentec.com/products/cellavista-4th), 
                  but could be obtained from any source. The raw cell counts should be pre-processed to normalize cell counts at each time
                  point to cell count at time 0 on log2 scale. Your csv file should contain the following headings for easier manipulation of the data."),
                  tags$br(),
                  tags$p("In this example, 'l2' refers to log2(cell count), 'nl2' refers to log2(cell count at time = t) normalized to log2(cell count at time = 0). 
                  In addition, the application will give you summary statistics for each condition at each time point. Since proliferation is dynamic in nature, 
                  it is best quantified in terms of rate as a metric. Our group recently described Drug-Induced Proliferation (DIP) rates (Harris et al. 2016) 
                  as metric of anti-proliferative response in cancer cells under perturbations. Since most drugs exhibit delay in realizing their effects, you 
                  could exclude an initial window of drug equilibration to calculate proliferation rates for each conditions."), 
                  tags$br(), 
                  tags$p("Output for summary statistics and proliferation rates is in data table format. N-number of replicates, sd-standard deviation, se-standard
                         error mean, ci-95% confidence interval"),
               img(src = "data_image.png", height = 150, width = 750)
             )
    ),
    tabPanel("Upload and subset data of interest",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose a data to upload',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                 selectInput('drug', label = "Choose a drug", choices = c(unique(treatment))),
                 selectInput('date', label = "Choose a date", choices = c(unique(dates))),
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
                 textInput('title', 'Enter Title for the plot', ""),
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")),
               mainPanel(
                 plotOutput('MyPlot', width = 600, height = 800)))), 
    tabPanel("Data Summary", 
               mainPanel(
               dataTableOutput(outputId = "summary")
             )), 
    tabPanel("Proliferation Rates", 
             pageWithSidebar(
               headerPanel('Estimate DIP Rates'),
               sidebarPanel(
                 numericInput(inputId = 'time', 'Starting time', value = 25)),
             mainPanel(
               dataTableOutput(outputId = "dip"),
               plotOutput(outputId = "box", width = 400, height = 400)
             )
             ))
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
    newD <- subset(df, Cell.Line == input$cell & Treatment == input$drug & Date == input$date)
  })
  data1 <- reactive({
    summ <- summarySE(data(), measurevar = "nl2", groupvars = c("Date", "Cell.Line", "Treatment", "Conc", "Time"))
  })
  data2 <- reactive({
    temp <- subset(data(), Time >= input$time)
    dn = data.frame()
    for(w in unique(temp$Well)){
      s = subset(temp, Well == w)
      df = data.frame(Date = unique(s$Date), Cell.Line = unique(s$Cell.Line), Treatment = unique(s$Treatment), Conc = unique(s$Conc), Well = unique(s$Well), rates = coef(lm(nl2~Time, data=s))[2])
      dn = rbind(dn, df)
    }
    dn = summarySE(dn, measurevar = "rates", groupvars = c("Date", "Cell.Line", "Treatment", "Conc"))
    dn
  })
  output$contents <- renderTable({data()}, digits = 10)
  output$MyPlot <- renderPlot({
    x <- data()[, c(input$xcol, input$ycol, "Conc", "Cell.Line", "Treatment")]
    plot(x[,1], x[,2], ylim = c((min(x[,2])-1), max(x[,2])+1), xlab = paste0(input$xcol), type="l", ylab = paste0(input$ycol), main=paste0(input$title))
  })
  output$summary <- renderDataTable({data1()})
  output$dip <- renderDataTable({data2()})
  output$box <- renderPlot({plot(data2()$rates~log10(data2()$Conc), ylab = "rates", xlab = "Log10(Conc)", type="b")})
})

shinyApp(ui, server)



