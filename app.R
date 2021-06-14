#
# This is a Shiny web application based on starja
# https://stackoverflow.com/questions/65415824/linear-regression-r-shiny-application-with-multiple-independent-variable-selecti
#

library(shiny) 
library(DT)
library(shinyWidgets) 

ui <- fluidPage(
    titlePanel("Linear Model: multiple regression"),
    h3("Regressão linear múltipla em R:"),
    h4("Adaptado por Adriano Marcos Rodrigues Figueiredo a partir de `starja` comment on stackoverflow"),
    h5("E-mail: adriano.figueiredo@ufms.br"),
    helpText("Entrar com seu arquivo em extensão .csv, contendo rótulos das variáveis na primeira linha."),
    helpText("O menu drowdown surgirá após especificar o modelo, sugerindo a primeira coluna como variável dependente e a segunda coluna em diante como independente."),
    sidebarPanel(
        
        fileInput(
            inputId = "filedata",
            label = "Upload data .csv - Selecione seu arquivo .csv",
            multiple = FALSE,
            accept = c(".csv"),
            buttonLabel = "Choosing ...",
            placeholder = "No files selected yet"
        ),
        uiOutput("yvariable"),
        uiOutput("xvariable")
    ), #sidebarpanel
    
    mainPanel( #DTOutput("tb1"), 
        fluidRow(column(6, verbatimTextOutput('lmSummary')) , column(6, plotOutput('diagnosticPlot')))
    )
) #fluidpage


server <- function(input, output) {
    
    data <- reactive({
        req(input$filedata)
        inData <- input$filedata
        if (is.null(inData)){ return(NULL) }
        mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
    })
    output$tb1 <- renderDT(data())
    
    output$xvariable <- renderUI({
        req(data())
        xa<-colnames(data())
        pickerInput(inputId = 'xvar',
                    label = 'Select x-axis variable',
                    choices = c(xa[1:length(xa)]), selected=xa[2],
                    options = list(`style` = "btn-info"),
                    multiple = TRUE)
        
    })
    output$yvariable <- renderUI({
        req(data())
        ya<-colnames(data()) 
        pickerInput(inputId = 'yvar',
                    label = 'Select y-axis variable',
                    choices = c(ya[1:length(ya)]), selected=ya[1],
                    options = list(`style` = "btn-info"),
                    multiple = FALSE)
        
    })
    
    lmModel <- reactive({
        req(data(),input$xvar,input$yvar)
        x <- as.numeric(data()[[as.name(input$xvar)]])
        y <- as.numeric(data()[[as.name(input$yvar)]])
        current_formula <- paste0(input$yvar, " ~ ", paste0(input$xvar, collapse = " + "))
        current_formula <- as.formula(current_formula)
        model <- lm(current_formula, data = data(), na.action=na.exclude)
        return(model)
    })
    
    
    
    
    
    
    
    output$lmSummary <- renderPrint({
        req(lmModel())
        summary(lmModel())
    })
    
    output$diagnosticPlot <- renderPlot({
        req(lmModel())
        par(mfrow = c(2,2))
        plot(lmModel())
    })
}

shinyApp(ui = ui, server = server)