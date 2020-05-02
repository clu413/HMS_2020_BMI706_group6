library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(lme4)

#---load data---
dat.change <- read_csv(url('https://raw.githubusercontent.com/luchenyue95/HMS_2020_BMI706_group6/master/data/fixed_data_percent_change.csv'))
dat.filt   <- read_csv(url('https://raw.githubusercontent.com/luchenyue95/HMS_2020_BMI706_group6/master/data/filtered_data.csv'))

#---preprocess data---
preproc <- function(df) {
    df$date <- as.Date(df$date)
    df$state <- factor(df$state)
    df$Governor.Political.Affiliation <- factor(df$Governor.Political.Affiliation)
    return(df)
}
dat.change <- preproc(dat.change)
dat.filt   <- preproc(dat.filt)

#---UI---
ui <- fluidPage(
  titlePanel('When did US states close their schools when the COVID pandemic hit?'),
  
  fluidRow(
    column(4, sidebarPanel(
      selectInput('name', 'Select a value to display:',
                  c('Positive Increase'='positiveIncrease'
                  )),
      selectInput('category', 'Color by:',
                  c('None' = 'state',
                    'Governor Political Affiliation'='Governor.Political.Affiliation',
                    'Region'='Region',
                    'Time of Closure'='ClosureDateCat'
                  )),
      pickerInput('state', 'Select states:', 
                  choices = unique(levels(dat.change$state)), 
                  options = list(`actions-box` = TRUE), 
                  selected = unique(dat.change$state), multiple = T),
      sliderInput('innoculation',
                  'Innoculation Time (days):',
                  min = 1,
                  max = 10,
                  value = 5),
      materialSwitch('normalize', label = 'Normalize by state population?', status='primary'),
      width = 12
    )),
    column(8, plotlyOutput('pcp'))),
  
  fluidRow(
    # column(4, plotlyOutput('lineplot')),
    column(6, plotlyOutput('map')),
    column(6, plotlyOutput('lineplot'))
    )
)


# Define server logic
server <- function(input, output) {
    #--Chen--
    output$map <- renderPlotly({
        df <- dat.filt[which(dat.filt$date==max(dat.filt$date)),]
        df$hover <- with(df, paste(state, '<br>',positive))
        plot_geo(df, locationmode = 'USA-states') %>%
            add_trace(text = ~hover, locations = ~state,
                      color = ~Governor.Political.Affiliation
            ) %>%
            layout(
                title = 'lallaa',
                geo = g <- list(
                    scope = 'usa',
                    projection = list(type = 'albers usa'),
                    showlakes = TRUE,
                    lakecolor = toRGB('white')
                )
            )
    })
    
    #--Dany--
    output$pcp <- renderPlotly({
      
        df <- dat.filt[which(dat.filt$date==max(dat.filt$date)),] %>% as.data.frame() %>% filter(state %in% input$state)
        party <- unique(df$Governor.Political.Affiliation)
        pcdat <- df %>%
            select(state, total, StateClosureStartDate, Governor.Political.Affiliation) %>%
            subset(!is.na(Governor.Political.Affiliation)) %>% filter(state %in% input$state)
        pcdat$StateClosureStartDate <- as.factor(pcdat$StateClosureStartDate)
        pcdat$state <- as.factor(pcdat$state)
        factor_cols <- sapply(pcdat, is.factor)
        pcdat[, factor_cols] <- sapply(pcdat[, factor_cols], unclass)
        pcdat$state <- as.factor(pcdat$state) %>% unclass()
        
        pcdat %>%
            plot_ly() %>%
            add_trace(type = 'parcoords', line = list(color = ~Governor.Political.Affiliation, colorscale = list(c(0,'blue'),c(1,'red'))),
                      dimensions = list(
                          list(range = c(1,length(df$state)),
                               tickvals = c(1:length(df$state)),
                               label = 'state',
                               ticktext = c(paste(df$state)),
                               #multiselect = TRUE,
                               #constraintrange = list(c(1,1), c(25,25), c(49,50)),
                               #constraintrange = c(state_restraint),
                               values = ~state ),
                          
                          list(range = c(~min(total),~max(total)),
                               label = 'total tests',
                               tickvals = c(~min(total),~max(total)),
                               values= ~total),
                          
                          list(range=c(1,2),
                               tickvals = c(1, 2),
                               label = 'party',
                               ticktext = c(paste(party)),
                               values = ~Governor.Political.Affiliation ),
                          
                          list(range = c(1,7),
                               tickvals = c(1,7),
                               label = 'school closure date',
                               ticktext = c(paste(dat.filt$StateClosureStartDate)),
                               values = ~StateClosureStartDate
                          )
                      )
                      
                      
            )
        
    })
    
    #--Kath--
    heatmapMatrix <- reactive({
        # obtain 30 timestamps after school closure (including school closure date)
        heatmap.width <- 30
        cat <- input$name
        states <- unique(dat.change$state)
        mat <- matrix(rep(NA, length(states)*heatmap.width), nrow=length(states))
        for (i in 1:length(states)) {
            state <- states[i]
            # filter by state
            df <- dat.change[dat.change$state == state & dat.change$name == cat,]
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
    
    #--Jon--
    output$lineplot <- renderPlotly({
        #subset by states selected
        dat_subset <- subset(dat.change, state %in% input$state & name %in% input$name)
        
        # normalize
        if (input$normalize == TRUE) {
          dat_subset$value <- dat_subset$value / (dat_subset$POPESTIMATE2019/1e5)
          y.label <- paste(input$name, "per 100K")
        }
        else {
          y.label <- input$name
        }
      
        #linear models
        dat_subset_before <- subset(dat_subset, value < Inf & !is.na(value) & date_diff <= input$innoculation)
        dat_subset_after <- subset(dat_subset, value < Inf & !is.na(value) & date_diff > input$innoculation)
        if (nrow(dat_subset > 0)) {
          if(input$category != 'state') {
              dat_subset_before$fv <- dat_subset_before %>% lm(formula(paste("value ~ date_diff * ", input$category)), ., na.action = na.exclude) %>% fitted.values()
              dat_subset_after$fv <- dat_subset_after %>% lm(formula(paste("value ~ date_diff * ", input$category)), ., na.action = na.exclude) %>% fitted.values()
              colorby <- formula(paste0("~",input$category))
              if(input$category == 'Governor.Political.Affiliation') {
                colorlist = c("blue", "red")
              }
              else {
                colorlist = c("darkgreen","gold", "darkred", "purple")
              }
              
          }
          else {
              dat_subset_before$fv <- dat_subset_before %>% lm(value ~ date_diff, ., na.action = na.exclude) %>% fitted.values()
              dat_subset_after$fv <- dat_subset_after %>% lm(value ~ date_diff, ., na.action = na.exclude) %>% fitted.values()
              colorby <- 1
              colorlist = "Dark2"
          }
        }
        
        # draw plot
        dat_subset %>% group_by(state) %>% plot_ly() %>%
            add_trace(x = ~date_diff, 
                      y= ~value, 
                      type = 'scatter', mode = 'lines', color=formula(paste0("~",input$category)), colors=colorlist,
                      line=(list(width = 1, opacity = 0.6))) %>%
            add_trace(data = dat_subset_before,
                      x = dat_subset_before$date_diff,
                      y = ~fv,
                      type = 'scatter', mode = 'lines', color=colorby,
                      line=list(width = 4, dash = 'dash'),
                      name = "Trend Until Schools Closed", showlegend = FALSE) %>%
            add_trace(data = dat_subset_after,
                      x = dat_subset_after$date_diff,
                      y = ~fv,
                      type = 'scatter', mode = 'lines', color=colorby,
                      line=list(width = 4, dash = 'dash'),
                      name = "Trend After Schools Closed", showlegend = FALSE) %>%
            layout(shapes = list(type = "rect", fillcolor = "blue", line=list(color="blue"), opacity = 0.2,
                                 x0=0, x1=input$innoculation, xref = 'x', 
                                 y0=0, y1= 1, yref='paper'),
                   yaxis = list(title = y.label),
                   margin = list(b=90),
                   annotations = list(x=1, xref='paper', xanchor='right',
                                      y=-0.3, yref='paper', 
                                      text = "Dashed rine represents linear models before and after policy implementation",
                                      showarrow = FALSE)) %>%
            hide_colorbar()
        
    })
}

#--Run the application--
shinyApp(ui = ui, server = server)

