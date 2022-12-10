#
# This is a Shiny web application. 

library(shiny)
library(readr)
aa <- read_csv("aa.csv")
aa <- aa %>%  select(Area, prop_movein,prop_leave,early_meanRobbery,
                     early_meanBurglary,later_meanRobbery,
                     later_meanBurglary,Latitude,Longitude)
d <- SharedData$new(aa[sample(nrow(aa), 50),])

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Map info with data table"),

    # Sidebar with a slider input for number of bins 
    filter_slider("prop_movein", "movein", d, column=~prop_movein, step=0.1, width=250),
    filter_slider("prop_leave", "leave", d, column=~prop_leave, step=0.1, width=250),
    bscols(widths = c(10, 10),leafletOutput("map"),DTOutput("table"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- DT::renderDataTable({
        # generate bins based on input$bins from ui.R
      DT::datatable(d, extensions="Scroller", style="bootstrap", class="compact", width="100%",
                options=list(list(className = 'dt-left',deferRender=TRUE, scrollY=300, scroller=TRUE)))},server = F,fillContainer = T)
    
    output$map <- renderLeaflet({ leaflet(d) %>% addTiles() %>% addMarkers()})
}
# Run the application 
shinyApp(ui = ui, server = server)
