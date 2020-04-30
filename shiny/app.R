#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)

# import functions from external files

dat <- read_csv('../data/merged_data_percent_change.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("What's a good title?"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput('category', 'Select a category:',
                        c('Positive Increase'='positiveIncrease',
                          'Positive % Change'='positive_percent_change',
                          '% Positive'='percent_positive',
                          'Hospitalized Increase'='hospitalizedIncrease',
                          'Hospitalized % Change'='hospitalized_percent_change',
                          'Death Increase'='deathIncrease',
                          'Death % Change'='death_percent_change'))
        ),
        mainPanel(
            plotlyOutput("heatmap")
        )
    )
)

# Define server logic
server <- function(input, output) {
    heatmapMatrix <- reactive({
        # obtain 30 timestamps after school closure (including school closure date)
        heatmap.width <- 30
        cat <- input$category
        states <- unique(dat$state)
        mat <- matrix(rep(NA, length(states)*heatmap.width), nrow=length(states))
        for (i in 1:length(states)) {
            state <- states[i]
            # filter by state
            df <- dat[dat$state == state & dat$name == cat,]
            closure_date <- df$StateClosureStartDate[1] - 10
            df <- df %>%
                filter(date >= closure_date) %>%
                arrange(date)
            mat[i,] <- df$value[1:heatmap.width]
        }
        rownames(mat) <- states

        return(mat)
    })
    
    output$heatmap <- renderPlotly({
        mat <- heatmapMatrix()
        plot_ly(
            y=rownames(mat),
            x=seq(1,30),
            z=mat, type='heatmap',
            height=800,
        ) %>%
            layout(
                xaxis=list(
                    title='Days',
                    dtick=1,
                    zeroline=F,
                    showline=F,
                    showticklabels=T,
                    showgrid=T
                ),
                yaxis=list(
                    title='States',
                    dtick=1,
                    zeroline=F,
                    showline=F,
                    showticklabels=T,
                    showgrid=T
                )) %>%
            add_segments(x=10, xend=10, y='AK', yend='WY')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
