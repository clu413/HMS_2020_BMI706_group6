library(shiny)
library(htmlwidgets)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(lme4)
library(RColorBrewer)
library(htmlwidgets)

#---load data---
dat.change <- read_csv('../data/fixed_data_percent_change.csv')
dat.change[is.infinite(dat.change$value), 'value'] <- NA
dat.filt   <- read_csv('../data/filtered_data.csv')

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
  tags$head(tags$script("
    $(document).on('shiny:updateInput', function(event) {
      alert(event.name);
      console.log(event.name, event.value);
    });
  ")),
  titlePanel('When did US states close their schools when the COVID pandemic hit?'),

  fluidRow(
    column(4, sidebarPanel(
      selectInput('name', 'Select a value to display:',
                  c('Positive Increase'='positiveIncrease',
                    'Positive % Change'='positive_percent_change',
                    '% Positive of Total'='percent_positive',
                    'Total New Tests' = 'totalTestResultsIncrease',
                    'Total Tests % change' = 'total_tests_percent_change',
                    'Death Increase'='deathIncrease',
                    'Death % Change'='death_percent_change'
                  )),
      selectInput('category', 'Color by:',
                  c('None' = 'state',
                    'Governor Political Affiliation'='Governor.Political.Affiliation',
                    'Region'='Region',
                    'Time of Closure'='ClosureDateCat'
                  )),
      # pickerInput('state', 'Select states:',
      #             choices = unique(levels(dat.change$state)),
      #             options = list(`actions-box` = TRUE),
      #             selected = unique(dat.change$state), multiple = T),
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
        column(5, plotlyOutput('map')),
        column(7, tabsetPanel(
           tabPanel('Line Chart', plotlyOutput('lineplot')),
           tabPanel('Heatmap', plotlyOutput('heatmap'))
        ))
    )
)


# Define server logic
server <- function(input, output, session) {
  reactive.states.input <- reactiveValues()
  reactive.pcp.dims <- reactiveValues()

  # add list of states
  reactive.states.input$states <- unique(dat.change$state)

    #--Chen--
output$map <- renderPlotly({
    df <- dat.filt[which(dat.filt$date==max(dat.filt$date)),]
    df$hover <- with(df, paste(state, '<br>',positive))
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    if (input$category == 'state'){
      plot_geo(df, locationmode = 'USA-states') %>%
        add_trace(text = ~hover, locations = ~state,
                  color = ~as.factor(StateClosureStartDate),
                  colors = "Blues"
        )%>%
        layout(
          title = 'US states',
          geo = g
        )}
    else if(input$category == 'Governor.Political.Affiliation'){
      plot_geo(df, locationmode = 'USA-states') %>%
        add_trace(text = ~hover, locations = ~state,
                  color = ~Governor.Political.Affiliation,
                  colors = c("blue", "red"))%>%
        layout(
          title = 'Governor.Political.Affiliation',
          geo = g)
    }
    else if(input$category == 'Region'){
      plot_geo(df, locationmode = 'USA-states') %>%
        add_trace(text = ~hover, locations = ~state,
                  color = ~Region,
                  colors = c("blue", "red"))%>%
        layout(
          title = 'Governor.Political.Affiliation',
          geo = g)
    }
    else if(input$category == 'ClosureDateCat') {
      plot_geo(df, locationmode = 'USA-states') %>%
        add_trace(text = ~hover, locations = ~state,
                  color = ~ClosureDateCat,
                  colors = "Reds")%>%
        layout(
          title = 'School Closure Date',
          geo = g)
    }
      })

    # a hack for linking to state selection - update accordingly!
    pcp.dimensions <- c('state', 'Region', 'Governor.Political.Affiliation', 'ClosureDatCat', 'StateClosureStartDate')
    pcp.states <- unique(levels(dat.change$state))
    output$pcp <- renderPlotly({

        df <- dat.filt[which(dat.filt$date==max(dat.filt$date)),] %>% as.data.frame()
        if (input$normalize == TRUE) {
          df$total <- df$total / (df$POPESTIMATE2019/1e5)
          total.label <- "Total Tests per 100K"
        }
        else {
          total.label <- "Total Tests"
        }
        df <- df[sort(df$state, decreasing = T),] %>% filter(state %in% input$state)
        party <- unique(as.factor(dat.filt$Governor.Political.Affiliation)) %>% levels()
        state <- unique(as.factor(dat.filt$state)) %>% levels() %>% sort(decreasing = T)
        region <- unique(as.factor(dat.filt$Region)) %>% levels() %>% sort(decreasing = T)
        closure <- unique(as.factor(dat.filt$StateClosureStartDate)) %>% levels() %>% sort(decreasing = T)
        closure.cat <-  c('Early', 'Middle', 'Late')

        pcdat <- df %>%
            select(state, total, StateClosureStartDate, Governor.Political.Affiliation, Region, ClosureDateCat) %>%
            subset(!is.na(Governor.Political.Affiliation))
        pcdat$StateClosureStartDate <- factor(pcdat$StateClosureStartDate, levels = closure)
        pcdat$state <- factor(pcdat$state, levels = state)
        pcdat$Region <- factor(pcdat$Region, levels = region)
        pcdat$ClosureDateCat <- factor(pcdat$ClosureDateCat, levels = c('Early', 'Middle', 'Late'), ordered = T)
        factor_cols <- sapply(pcdat, is.factor)
        pcdat[, factor_cols] <- sapply(pcdat[, factor_cols], unclass)
        pcdat <- pcdat[sort(pcdat$state, decreasing = T),]

        #DTH: magma or  viridis
        #DTH can color = 'grey'
        dimensions = list(
          list(range = c(1,length(df$state)),
               tickvals = c(1:length(df$state)),
               label = 'States',
               ticktext = c(paste(state)),
               values = ~state),
          list(range = c(1,4),
               tickvals = c(1:4),
               label = 'Regions',
               ticktext = c(paste(region)),
               values = ~Region,
               ticks = 'outside'),
          list(range=c(1,2),
               tickvals = c(1:2),
               label = 'Governor Political \nAffiliation',
               ticktext = c(paste(party)),
               values = ~Governor.Political.Affiliation),
          list(range = c(~min(total),~max(total)),
               label = total.label,

               values= ~total),
          list(range = c(~min(ClosureDateCat),~max(ClosureDateCat)),
               tickvals = c(1:3),
               label = 'Time of Closure',
               ticktext = c(paste(closure.cat)),
               values = ~ClosureDateCat),
          list(range = c(1,7),
               label = 'School Closure Dates',
               tickvals = c(1:7),
               ticktext = c(paste(closure)),
               values = ~StateClosureStartDate,
               ticks = 'outside'
          )
        )
        out <- pcdat %>% plot_ly(source='pcoords')
        if (input$category == 'state'){
          out <- out %>%
            add_trace(type = 'parcoords', line = list(color = ~state, colorscale = 'Viridis'),
                      domain = list(x=c(0,2893), y=c(0,3)),
                      dimensions = dimensions
            ) %>% layout(autosize = F, height = 500, margin = list(l = 30, r = 150, b = 10, t = 10, pad = 4), title = "By State")
        }
        else if(input$category == 'Region'){
          out <- out %>%
            add_trace(type = 'parcoords', line = list(color = ~Region, colorscale = 'Viridis'),
                      dimensions = dimensions
            )%>% layout(autosize = F, height = 500, margin = list(l = 30, r = 150, b = 10, t = 10, pad = 4), title = "By Region")
          }
        else if(input$category == 'ClosureDateCat') {
          out <- out %>%
              add_trace(type = 'parcoords', line = list(color = ~ClosureDateCat, colorscale = 'Viridis'),
                        dimensions = dimensions
              )%>% layout(autosize = F, height = 500, margin = list(l = 30, r = 150, b = 10, t = 10, pad = 4), title = "By Closure Date")
        }
        else if(input$category == 'Governor.Political.Affiliation'){
          out <- out %>%
              add_trace(type = 'parcoords', line = list(color = ~Governor.Political.Affiliation, colorscale = list(c(0,'red'), c(1,'blue'))),
                        dimensions = dimensions
              )%>% layout(autosize = F, height = 500, margin = list(l = 30, r = 150, b = 10, t = 10, pad = 4), title = "By Governor Political Affiliation")
        }
        out %>% onRender("
          function(el) {
            console.log(el);
              var dict = {
                      'States': null,
                      'Governor Political Affiliations': 'Governor.Political.Affiliation',
                      'Regions': 'Region',
                      'School Closure Dates': 'ClosureDateCat',
                      'Total tests': 'total',
                  };
              // change hover css
              $('.axis-title').hover(function() {
                  self = $(this)
                  if (dict[self.text()]) {
                      self.css('cursor', 'pointer');
                  }
              });
              $('.axis-title').click(function() {
                  var text = $(this).text();
                  if (dict[text]) {
                      Shiny.setInputValue('category', dict[text]);
                      $('.axis-title').css('fill', 'black');
                      $(this).css('fill', 'red');
                  }
              });
          }
        ") %>%
        event_register('plotly_restyle')
    })

    observeEvent(event_data('plotly_restyle', source = 'pcoords'), {
      d <- event_data("plotly_restyle", source = 'pcoords')
      # what is the relevant dimension (i.e. variable)?
      dimension <- as.numeric(stringr::str_extract(names(d[[1]]), "[0-9]+"))
      # If the restyle isn't related to a dimension, exit early.
      if (!length(dimension)) return()
      # careful of the indexing in JS (0) versus R (1)!
      dimension_name <- pcp.dimensions[[dimension + 1]]
      # a given dimension can have multiple selected ranges
      # these will come in as 3D arrays, but a list of vectors
      # is nicer to work with
      info <- d[[1]][[1]]
      if (typeof(info) == "NULL") {
        reactive.pcp.dims[[dimension_name]] <- NA
      } else {
        reactive.pcp.dims[[dimension_name]] <- if (length(dim(info)) == 3) {
          lapply(seq_len(dim(info)[2]), function(i) info[,i,])
        } else {
          list(as.numeric(info))
        }
      }
      party <- unique(as.factor(dat.filt$Governor.Political.Affiliation)) %>% levels()
      state <- unique(as.factor(dat.filt$state)) %>% levels() %>% sort(decreasing = T)
      region <- unique(as.factor(dat.filt$Region)) %>% levels() %>% sort(decreasing = T)
      closure <- unique(as.factor(dat.filt$StateClosureStartDate)) %>% levels() %>% sort(decreasing = T)
      closure.cat <-  c('Early', 'Middle', 'Late')
      dat.init <- dat.filt
      for (field in names(reactive.pcp.dims)) {
        ranges <- reactive.pcp.dims[[field]]
        if (is.na(ranges)) {
          # somehow case_when doesn't work
          if (field == 'state') { all_values <- state }
          else if (field == 'Region') { all_values <- region}
          else if (field == 'Governor.Political.Affiliation') { all_values <- party }
          else if (field == 'ClosureDatCat') { all_values <- closure.cat }
          else { all_values <- closure }
        } else {
          all_values <- c()
          for (range_i in seq_along(ranges)) {
            range <- seq(ceiling(ranges[[range_i]][1]), floor(ranges[[range_i]][2]))
            values <- case_when(
              field == 'state' ~ state[range],
              field == 'Region' ~ region[range],
              field == 'Governor.Political.Affiliation' ~ party[range],
              field == 'ClosureDatCat' ~ closure.cat[range],
              TRUE ~ closure[range] # state closure start date
            )
            all_values <- c(all_values, values)
          }
        }
        dat.init <- dat.init[dat.init[[field]] %in% all_values,]
      }
      reactive.states.input$states <- unique(dat.init$state)
    })


    #--Kath--
    heatmapMatrix <- reactive({
        # obtain 30 timestamps after school closure (including school closure date)
        heatmap.height <- 30
        cat <- input$name
        orderby <- input$category
        states <- unique(dat.filt[order(dat.filt[[orderby]]),]$state)
        mat <- matrix(rep(NA, length(states)*heatmap.height), nrow=heatmap.height)
        for (i in 1:length(states)) {
            state <- states[i]
            # filter by state
            df <- dat.change[dat.change$state == state & dat.change$name == cat,]
            closure_date <- df$StateClosureStartDate[1] - 10
            df <- df %>%
                filter(date >= closure_date) %>%
                arrange(date)
            if (input$normalize == TRUE & input$name %in% c('positiveIncrease', 'totalTestResultsIncrease', 'hospitalizedIncrease', 'deathIncrease')) {
              df$value <- df$value / (df$POPESTIMATE2019/1e5)
            }
            mat[,i] <- df$value[1:heatmap.height]
        }
        colnames(mat) <- states
        rownames(mat) <- seq(1, heatmap.height)
        return(mat)
    })

    output$heatmap <- renderPlotly({
        mat <- heatmapMatrix()
        states <- colnames(mat)
        rows <- rownames(mat)
        if (input$normalize == TRUE & input$name %in% c('positiveIncrease', 'totalTestResultsIncrease', 'hospitalizedIncrease', 'deathIncrease')) {
          hm.label <- paste(input$name, "\nper 100K")
        }
        else {
          hm.label <- input$name
        }
        plot_ly(
            y=rows,
            x=states,
            colorbar = list(title = hm.label),
            z=mat, type='heatmap',
            width=700,
        ) %>%
            layout(
                xaxis=list(
                    title='States',
                    dtick=1,
                    zeroline=F,
                    showline=F,
                    showticklabels=T,
                    showgrid=T
                ),
                yaxis=list(
                    autorange='reversed',
                    title='Days',
                    dtick=1,
                    zeroline=F,
                    showline=F,
                    showticklabels=T,
                    showgrid=T
                )) %>%
            add_segments(x=states[1], xend=states[length(states)], y=which(rows == 10), yend=which(rows == 10))
    })

    #--Jon--
    output$lineplot <- renderPlotly({
        #subset by states selected
        dat_subset <- subset(dat.change, state %in% reactive.states.input$states & name %in% input$name & positive > 100)

        # normalize
        if (input$normalize == TRUE & input$name %in% c('positiveIncrease', 'totalTestResultsIncrease', 'hospitalizedIncrease', 'deathIncrease')) {
          dat.change$value <- dat.change$value / (dat_subset$POPESTIMATE2019/1e5)
          dat_subset$value <- dat_subset$value / (dat_subset$POPESTIMATE2019/1e5)
          y.label <- paste(input$name, "per 100K")
        }
        else {
          y.label <- input$name
        }

        #linear models
        dat_subset_before <- subset(dat_subset, value < Inf & !is.na(value) & date_diff <= input$innoculation)
        dat_subset_after <- subset(dat_subset, value < Inf & !is.na(value) & date_diff > input$innoculation)
        if (nrow(dat_subset) > 0) {
          if(input$category != 'state') {
              colorby <- input$category
              if(input$category == 'Governor.Political.Affiliation') {
                colorlist = c("blue", "red")
              }
              else {
                colorlist = c("darkgreen","gold", "darkred", "purple")
              }

          }
          else {
              colorby <- 1
              colorlist = colorRampPalette(brewer.pal(8, "Dark2"))(length(reactive.states.input$states))
          }
        }

        # draw plot
        g <- ggplot(subset(dat.change, name %in% input$name & positive > 100)) +
          geom_line(aes(x = date_diff, y=value, group = state), alpha = 0.1, size = 0.5, color = 'lightgrey') + theme_minimal() +
          geom_rect(xmin = 0, xmax=input$innoculation, ymin=-12000, ymax = 12000, size=0, fill = "lightblue", alpha = 0.5) +
          scale_color_manual(values = colorlist) + scale_y_log10() +
          labs(title = "Activity over time", x="Days from school closure", y = y.label)
        if (nrow(dat_subset)>0) {
          g <- g+ geom_line(data = dat_subset, alpha = 0.2, size=0.5, aes_string(x = 'date_diff', y='value', group = 'state', color = input$category))
          if (colorby == 1) {
            g <- g + geom_smooth(data = dat_subset_before, size = 1, aes(x = date_diff, y=value, group = 1), color = 'blue', method = lm) +
                     geom_smooth(data = dat_subset_after, size = 1, aes(x = date_diff, y=value, group = 1), color = 'purple', method = lm)
          }
          else {
            g <- g+ geom_smooth(data = dat_subset_before, size = 1, aes_string(x = 'date_diff', y='value', color = colorby), method = lm) +
                    geom_smooth(data = dat_subset_after, size = 1, aes_string(x = 'date_diff', y='value', color = colorby), method = lm)
          }
        }
        ggplotly(g) %>% layout(margin = list(b=90)) %>%
          add_annotations(x=0.5, xref='paper', xanchor='center',
                          y=-0.3, yref='paper', font=list(size = 10),
                          text = "Dashed rine represents linear models before and after policy implementation",
                          showarrow = FALSE) %>%
          add_annotations(x=input$innoculation/2, xref='x',y=1, yref='paper', xanchor='center', font=list(size=10), text="Innoculation \nperiod", showarrow=FALSE)
    })
}

#--Run the application--
shinyApp(ui = ui, server = server)
