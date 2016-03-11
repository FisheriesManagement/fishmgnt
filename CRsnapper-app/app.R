#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

#load in data
AppDATA <- readRDS("data/AppDATA.rds")


# Define UI for application that plots our data
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Red Snapper on Costa Rica's Pacfic Coast"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
        selectInput(inputId = "var", 
                    label = "Choose a variable to display",
                    choices = list("Total Catch", "Mean Naitonal Price", "Trends"),
                    selected = "Total Catch"),
        
        
        
        sliderInput(inputId = "range", 
                    label = "Range of interest:",
                    min = 1990, max = 2013, value = c(1990, 2013),
                    sep="")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("OutPlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

  output$OutPlot <- renderPlot({ 
    data <- switch(input$var, 
                   "Total Catch" = AppDATA$TotalCatch,
                   "Mean Naitonal Price" = AppDATA$corrected,
                   "Trends" = AppDATA$chgP
                   )
    
    data2 <- switch(input$var, 
                   "Total Catch" = AppDATA$TotalCatch,
                   "Mean Naitonal Price" = AppDATA$corrected,
                   "Trends" = AppDATA$chgC
    )
      
    years <- switch(input$var, 
                    "Total Catch" = AppDATA$Year,
                    "Mean Naitonal Price" = AppDATA$Year,
                    "Trends" = AppDATA$Year
    )
    
    colors <- switch(input$var, 
                    "Total Catch" = "green",
                    "Mean Naitonal Price" = "blue",
                    "Trends" = "green"
                    )
    ylabels <- switch(input$var, 
                      "Total Catch" = "Total Catch",
                      "Mean Naitonal Price" = "Mean National Price (colones)",
                      "Trends"= "Percent Change"
                     )
    
    
    ggplot(AppDATA, aes(x=years, y=data))+
            geom_line(color="blue", aes(size=1)) +
            geom_line(data=AppDATA, aes(x=years, y=data2, size=1),color=colors)+
            theme_bw()+
            labs(x="Year", y= ylabels) +
            # theme(legend.position="none")+
            scale_x_continuous(limits = c(input$range[1],input$range[2]), expand = c(0,0))
            
      
      

  })
})

# Run the application 
shinyApp(ui = ui, server = server)

