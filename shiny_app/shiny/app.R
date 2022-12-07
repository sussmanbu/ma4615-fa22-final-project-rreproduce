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


# Build UI
ui <- fluidPage(
    tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"), 
    # Application title
    span(style = "font-weight: 600; font-size: 25px; width: 100%;
         color: #022DB7;", "State Immigration Status, Income, Crime Rate"),
    # Line Breaks
    br(),br(),
    # Map
    fluidRow(
      column(8, leafletOutput("map")
      ),
      column(4, 
             span("Select "), span(style="color:green", "State"), 
             span(" from the map:"),
             br(),br(),
             htmlOutput("name"),
             hr()
      )),
    br(),br(),
    hr(),
    fluidRow(
      column(5, plotOutput("plot", width = "120%", height = "400px")
    ))
  )


# Define server logic required
server <- function(input, output, session) {
    
  ####### GLOBAL VARIABLE #########
  click_count <- 0 ###COUNTING CLICKs
  sn <- " "
  ###############
  ####INPUT DATASET#####
  movein <- read_csv(here::here("dataset","movein.csv")) %>%
    rename(NAME=d_state_name)
  state <- st_read(here::here("dataset/cb_2019_us_state_20m/cb_2019_us_state_20m.shp")) %>%
    inner_join(movein,by='NAME')
  state_inc <- read_csv(here::here("dataset","state_inc.csv"))
  ##MAP ELEMENTS###
  bins <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
  pal <- colorBin("YlOrRd", domain = state$prop_movein, bins = bins)
  labels <- sprintf(
    "<strong>%s</strong><br/>%g percent",
    state$NAME, state$prop_movein
  ) %>% 
    lapply(htmltools::HTML)
  
  ########MAP###########
  output$map <- renderLeaflet({
    leaflet(state) %>%
    setView(-96, 37.8, 4) %>%
    addTiles() %>%
    addPolygons(layerId = ~NAME,
                fillColor = ~pal(prop_movein),
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
              title = NULL, position = "bottomright")})
  
  observeEvent(input$map_shape_click, {
    click_count <<- click_count+1
  })
  
  output$name <- renderText({
    click <- input$map_shape_click$id
    selection <- state %>% filter(NAME == click)
    sn <<- selection$NAME
    paste("<strong> <span style = \'font-weight: 700;\'> Origin:            </span> </strong> 
        <strong> <span style = \'font-weight: 500;\'> ",sn, "</span> </strong>")
  })
  
  output$plot <- renderPlot({
    click <- input$map_shape_click$id
    selection <- state_inc %>% filter(o_state_name==toupper(click))
    selection %>%
      ggplot() +
      geom_col(aes(x="",y=inc_n,fill=income),position="stack")+
      coord_polar(theta="y")+
      labs(x="Population",y="",fill="Income Quantile")+
      ggtitle("State Income Quantile Proportion")+
      theme(plot.title=element_text(size=18,hjust=0.5,face="bold"))
  })
  }
   
  



# Run the application 
shinyApp(ui = ui, server = server)
