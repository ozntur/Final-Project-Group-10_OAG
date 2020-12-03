library(shiny)
library(linreg10)

library(MASS)

mydata = Boston

# Define UI for app that draws a histogram ----
ui <- fluidPage(

    # App title ----
    titlePanel("linreg10 Package Application!"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            selectInput(inputId = "myresponse", "Pick a Response:",
                        choices = names(mydata),
                        selected = names(mydata[1]),
                        multiple = FALSE),

            selectInput(inputId ="mypredictors", "Pick Predictors:",
                        names(mydata),
                        selected = names(mydata[2]),
                        multiple = TRUE),

            radioButtons(inputId = "pltype", "Plot Type",
                        choices = c("res_fit", "qq", "hist", "all"), selected = "hist"),

        ),

        # Main panel for displaying outputs ----
        mainPanel(

            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("distPlot")),
                        tabPanel("Summary of Response", dataTableOutput("respvar")),
                        tabPanel("Summary of Predictor", dataTableOutput("predvar")),
                        tabPanel("Confidence Intervals", verbatimTextOutput("ci")),
                        tabPanel("Coefficients", renderPrint("coeff")),
                        tabPanel("MSPE", dataTableOutput("mspe")),
                        tabPanel("p-value", dataTableOutput("pval")))


        )
    )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {

    # plotting
    output$distPlot <- renderPlot({
        plots(as.matrix(mydata[input$myresponse]),
              as.matrix(mydata[input$mypredictors]),
              pl_type = input$pltype)
    })

    # data info summaries
    output$respvar <- renderDataTable(summary(mydata[input$myresponse]))
    output$predvar <- renderDataTable(summary(mydata[input$mypredictors]))

    # confidence interval
    output$ci <- renderDataTable(ci(as.matrix(mydata[input$myresponse]), as.matrix(mydata[input$mypredictors]) ))

    # coeff
    output$coeff <- renderTable(coeff(as.matrix(mydata[input$myresponse]), as.matrix(mydata[input$mypredictors]) ))

    # mspe
    output$mspe <- renderTable(mspe(as.matrix(mydata[input$myresponse]), as.matrix(mydata[input$mypredictors]) ))

    # pval
    output$pval <- renderDataTable(pval(as.matrix(mydata[input$myresponse]), as.matrix(mydata[input$mypredictors]) ))


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
