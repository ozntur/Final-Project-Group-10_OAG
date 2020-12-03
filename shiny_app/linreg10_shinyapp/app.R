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

            selectInput(inputId = "pltype", "Plot Type",
                        choices = c("res_fit", "qq", "hist", "all"), selected = "hist",
                        multiple = FALSE),

        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Histogram ----
            plotOutput(outputId = "distPlot")

        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- renderPlot({

        plots(as.matrix(mydata[input$myresponse]),
              as.matrix(mydata[input$mypredictors]),
              pl_type = input$pltype)

    })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
