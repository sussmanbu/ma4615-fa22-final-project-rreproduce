#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(leaflet)
library(sf)
library(tidyverse)
library(htmltools)
library(plotly)



# Build UI
ui <- fluidPage(
    tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"), 
    # Application title
    span(style = "font-weight: 600; font-size: 25px; width: 100%;
         color: #022DB7;", "State Migration Pattern, Income, Crime Rate"),
    # Line Breaks
    br(),br(),
    # Map
    fluidRow(
      column(8, leafletOutput("map")
      ),
      column(4, 
             span(style = "font-weight: 600; font-size:18px","Select "), span(style="font-weight: 600; font-size:18px; color:green", "State"), 
             span(style = "font-weight: 600; font-size:18px"," from the map:"),
             br(),br(),
             htmlOutput("name"),
             br(),
             htmlOutput("Linfo"),
             htmlOutput("Minfo"),
             br(),
             actionButton("leave","Leave"),
             actionButton("movein","Move-in"),
             br(),
             hr()
      )),
    br(),br(),
    hr(),
    fluidRow(
      column(4, plotlyOutput("income_plot", width = "100%", height = "250px")),
      column(4, plotlyOutput("robplot00", width = "100%", height = "250px")),
      column(4, plotlyOutput("burplot00", width = "100%", height = "250px"))
    ),
    fluidRow(
      column(4),
      column(4, plotlyOutput("robplot10", width = "100%", height = "250px")),
      column(4, plotlyOutput("burplot10", width = "100%", height = "250px"))
    )
  )


# Define server logic required
server <- function(input, output, session) {
    
  ####### GLOBAL VARIABLE #########
  click_count <- 0 ###COUNTING CLICKs
  sn <- " "
  mode <- ""
  ###############
  ####INPUT DATASET#####
  movein <- read_csv("movein.csv") %>%
    rename(NAME=d_state_name)
  leave <- read_csv("leave.csv") %>%
    rename(NAME=o_state_name)
  
  state <- st_read("cb_2019_us_state_20m/cb_2019_us_state_20m.shp")
  movein_state <- state %>% inner_join(movein,by='NAME')
  leave_state <- state %>% inner_join(leave,by='NAME')
  
  
  state_inc <- read_csv("state_inc.csv")
  ##MAP ELEMENTS###
  bins <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
  pal <- colorBin("YlOrRd", domain = state$proportion, bins = bins)
  labels <- sprintf(
    "<strong>%s</strong>",
    state$NAME
  ) %>% 
    lapply(htmltools::HTML)
  
  
## 
  
  ########MAP###########
  output$map <- renderLeaflet({
    leaflet(state) %>%
    setView(-96, 37.8, 4) %>%
    addTiles() %>%
    addPolygons(layerId = ~NAME,
                weight=2,
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                         padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto"))}) 
  observeEvent(input$movein, {
    labels <- sprintf(
      "<strong>%s</strong><br/>%g percent",
      movein_state$NAME,movein_state$proportion*100
    ) %>% 
      lapply(htmltools::HTML)
    
    mode <<- "M"
    
    leafletProxy("map", data = movein_state) %>%
      clearShapes() %>%
      clearTiles() %>%
      clearControls() %>%
      addTiles() %>%
      addPolygons(layerId = ~NAME,
                  fillColor = ~pal(proportion),
                  opacity=0.5,
                  weight=2,
                  fillOpacity=1,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, 
                title = NULL, position = "bottomright")
  })
  
  observeEvent(input$leave, {
    labels <- sprintf(
      "<strong>%s</strong><br/>%g percent",
      leave_state$NAME,leave_state$proportion*100
    ) %>% 
      lapply(htmltools::HTML)
    
    mode <<- "L"
    
    leafletProxy("map", data = leave_state) %>%
      clearShapes() %>%
      clearTiles() %>%
      clearControls() %>%
      addTiles() %>%
      addPolygons(layerId = ~NAME,
                  fillColor = ~pal(proportion),
                  opacity=0.5,
                  weight=2,
                  fillOpacity=1,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, 
                title = NULL, position = "bottomright")
      })
  
########################################  
  output$Linfo <- renderText({
    click <- input$map_shape_click$id
    selection <- leave_state %>% filter(NAME == click)
    name <- selection$NAME
    percentage <- selection$proportion %>% round(digits=2)*100
    paste(percentage,"% population left this state at 16")
    })

  output$Minfo <- renderText({
    click <- input$map_shape_click$id
    selection <- movein_state %>% filter(NAME == click)
    name <- selection$NAME
    percentage <- selection$proportion %>% round(digits=2)*100
    paste(percentage,"% population moved in this state at 26")
  })
  
  observeEvent(input$map_shape_click, {
    click_count <<- click_count+1
  })
  
  output$name <- renderText({
    click <- input$map_shape_click$id
    selection <- state %>% filter(NAME == click)
    sn <<- selection$NAME
    paste("<strong> <span style = \'font-weight: 700;\'> State:            </span> </strong> 
        <strong> <span style = \'font-weight: 500;\'> ",sn, "</span> </strong>")
  })
  
  output$income_plot <- renderPlotly({
    click <- input$map_shape_click$id
    selection <- state_inc %>% filter(o_state_name==toupper(click))
    selection %>%
      plot_ly(labels = ~income, values = ~ inc_n, type = 'pie',sort=F,
              width = 350, height = 300) %>%
      layout(title="State Income Quantile Proportion",
             font = list(family='Arial', size = 11),
             legend = list(orientation = 'h', x=0, font = list(family = 'Arial', size = 10)),
             paper_bgcolor='transparent',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  crime00<-read_csv("00-07Crime.csv")
  crime10<-read_csv("10-17Crime.csv")
  output$robplot00 <- renderPlotly({
    click <- input$map_shape_click$id 
    click <- sub(' ','',click)
    selection <- crime00 %>% filter(Area == toupper(click))
    selection %>%
      plot_ly(x=~year) %>%
      add_trace(y=~Robbery,name='Robbery',mode='lines+markers',color='red') %>%
      layout(title="State Robbery Cases From 2000-2007",
             font = list(family='Arial', size = 11))
  })
  output$robplot10 <- renderPlotly({
    click <- input$map_shape_click$id 
    click <- sub(' ','',click)
    selection <- crime10 %>% filter(State == toupper(click))
    selection %>%
      plot_ly(x=~year) %>%
      add_trace(y=~Robbery,name='Robbery',mode='lines+markers',color='red') %>%
      layout(title="State Robbery Cases From 2010-2017",
             font = list(family='Arial', size = 11))
  })
  output$burplot00 <- renderPlotly({
    click <- input$map_shape_click$id 
    click <- sub(' ','',click)
    selection <- crime00 %>% filter(Area == toupper(click))
    selection %>%
      plot_ly(x=~year) %>%
      add_trace(y=~Burglary,name='Burglary',mode='lines+markers') %>%
      layout(title="State Burglary Cases From 2000-2007",
             font = list(family='Arial', size = 11))
  })
  output$burplot10 <- renderPlotly({
    click <- input$map_shape_click$id 
    click <- sub(' ','',click)
    selection <- crime10 %>% filter(State == toupper(click))
    selection %>%
      plot_ly(x=~year) %>%
      add_trace(y=~Burglary,name='Burglary',mode='lines+markers') %>%
      layout(title="State Burglary Cases From 2010-2017",
             font = list(family='Arial', size = 11))
  })
  }



# Run the application 
shinyApp(ui = ui, server = server)
