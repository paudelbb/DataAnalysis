library(wbstats)
library(data.table)
library(googleVis)
library(ggplot2)
library(gplots)
library(reshape2)
indicators = wbindicators()
countries <- data.table(wbcountries())


# Define UI for application that plots the data frame.
ui <- shinyUI(fluidPage(
  titlePanel("Analyze data from around the globe"),
  tabsetPanel(
    tabPanel("Welcome Data Analysis Platform",
             titlePanel("Welcome to data portal."),
             mainPanel(
               tags$p("Random"),
               tags$br(),
               tags$p("What goes here?"), 
               tags$br(), 
               tags$p("This is very random."),
               img(src = "data_image.png", height = 550, width = 450)
               )
    ),
    tabPanel("Upload and subset data of interest",
             titlePanel("Select variables for comparison"),
             sidebarLayout(
               sidebarPanel(
                 selectInput('indicator', label = "Choose an indicator", choices = c(unique(indicators$indicator)), multiple = T),
                 selectInput('country', label = "Choose a country", choices = c(unique(countries$country)), multiple = T)),
               mainPanel(
                 dataTableOutput('contents1')
               )
             )),
    tabPanel("Data Visualization",
             pageWithSidebar(
               headerPanel('Your plot goes here'),
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('identifier', 'identifer', ""),
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")),
               mainPanel(
                 plotOutput('MyPlot')))), 
    tabPanel("Data Summary", 
             verticalLayout(
               plotOutput('summaryPlot'),
               dataTableOutput(outputId = "summary"))), 
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
  options(shiny.maxRequestSize=30*1024^2)
  # added "session" because updateSelectInput requires it
  data1 <- reactive({ 
    req(input$indicator) ## ?req #  require that the input is available
    req(input$country) ## ?req #  require that the input is available
    indId = match(input$indicator, indicators$indicator)
    #indicatorid = indicators[indId,]$indicatorID
    myDT = data.table(wb(indicator = c(indicators[indId,]$indicatorID), mrv = 60, return_wide = TRUE))
    updateSelectInput(session, inputId = 'identifier',  label = 'identifer',   choices = names(myDT), selected = names(myDT))
    updateSelectInput(session, inputId = 'xcol',  label = 'X Variable',        choices = names(myDT), selected = names(myDT))
    updateSelectInput(session, inputId = 'ycol',  label = 'Y Variable',        choices = names(myDT), selected = names(myDT)[2])
    dN = myDT[(myDT$country %in% input$country),]
    dN = data.frame(dN)
  })
  data2 <- reactive({
    data = subset(data1(), data1()$date == max(data1()$date))
  })
  
  output$contents1 <- renderDataTable({data1()})
  output$MyPlot <- renderPlot({
      x <- data1()[ , c(input$identifier,input$xcol, input$ycol)]
      colnames(x) <- c("country", "b", "c")
      sp = ggplot(x, aes(x=b, y=c, col=country)) + geom_point() + labs(x=paste0(input$xcol), y=paste0(input$indicator), title=paste0(input$indicator))
      sp + theme_bw() + theme(axis.text.x = element_text(angle=45)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  })
 
  output$summaryPlot <- renderPlot({
    x <- data2()[ , c(input$identifier,input$xcol, input$ycol)]
    colnames(x) <- c("country", "b", "c")
    ggplot(data=x, aes(x=country, y=c, fill=country)) + geom_bar(stat="identity") + labs(y=paste0(input$ycol), title=paste0(input$indicator))
  })
  output$summary <- renderDataTable({data2()})
  output$dip <- renderDataTable({data3()})
  output$box <- renderPlot({boxplot(data3()$rates~log10(data3()$Conc), ylab = "rates", xlab = "Log10(Conc)", type="b", ylim=c(-0.01, 0.06))})
})

shinyApp(ui, server)



