#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(randomForest)
rf <- readRDS("rf.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Prediction of Y"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "var_2",
                         "var_2", value = 0, min = -7000, max = 13000), 
            numericInput(inputId = "var_3",
                         "var_3", value = 0, min = -450000, max = 86000),
            numericInput(inputId = "var_9",
                         "var_9", value = 0), 
            selectInput(inputId = "var_10",
                         "var_10", choices = c('1'= 1, '2'= 2, '3' = 3, '4' = 4, '5' = 5)),
            actionButton(inputId = 'go',
                         label = 'Predict')            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = 'distPlot'), 
           verbatimTextOutput(outputId = 'predictedOutput')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        newData = eventReactive(input$go, {data.frame(var_2 = input$var_2, var_3 = input$var_3, var_9 = input$var_9, var_10 = input$var_10)})
        predicted = reactive({as.vector(predict(rf, newData(), predict.all = TRUE)$individual)})
        
    output$distPlot = renderPlot({
        # generate bins based on input$bins from ui.R

        
        # draw the histogram with the specified number of bins
        hist(predicted(), main = 'Distribution of Predicted Value', xlab = 'predicted value', prob = TRUE)
    })
    
    output$predictedOutput = renderPrint({summary(predicted())})
}

# Run the application 
shinyApp(ui = ui, server = server)
