####### Prison Justice App ##########################
####### Matthieu Huy, March 2023 ####################


#Load packages


library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(rgdal)
library(spatialEco)
library(glue)
library(here)
library(stringr)
library(htmltools)
library(tmap)
library(usmap)
library(shinythemes)
library(rcartocolor)



# Style ###############################################

bgcolor <- "#262626"
  bg2 <- "#222223"

  label_style <- list(
    "color"        = "black",
    "font-family"  = "default",
    "font-weight"  = "bold",
    "box-shadow"   = "3px 3px rgba(0,0,0,0.25)",
    "font-size"    = "15px",
    "border-color" = "rgba(0,0,0,0.5)")

  # all colors (palette)
  colors <- carto_pal(12, "Vivid")[c(3:5, 9)]



# Read in data ##########################################

state_data <- map_data('state')

county_data <- map_data('county')

county_names <- read_csv(url('https://raw.githubusercontent.com/pdil/usmap/master/inst/extdata/county_fips.csv'))

state_names <- read_csv(url('https://raw.githubusercontent.com/pdil/usmap/master/inst/extdata/state_fips.csv'))



# clean/prep data #########################################





# create outputs/maps/plots ###############################

us_counties_map <- ggplot(data=county_data,
                          aes(x=long,
                              y=lat,
                              fill=region,
                              group=group)) +
  geom_polygon(color = "white") +
  guides(fill=FALSE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  ggtitle('U.S. Map with States') +
  coord_fixed(1.3)






# UI ##########################################
ui <- shiny::navbarPage(
  title = tags$div(
    "Prison Justice"),
    tabPanel("Home",
             tags$div(
               h2(strong("Incarceration and Environmental Justice")),
               p("Environmental justice is the belief that all people deserve safe and healthy conditions wherever they live,
work, learn, pray, or play. One of the most vulnerable groups to environmental injustices are incarcerated
people, as they have little agency over their living conditions. Reports have documented both the
connection between carceral spaces and impacts to human health from toxic exposure. Similarly, reports
from across the U.S. have identified incarcerated people left behind in the face of deadly natural
disasters. We are conducting a first-of-its-kind study mapping carceral facilities across the U.S. to
determine how close they are to toxic land, as well as how likely they are to experience natural disasters
now, and in the year 2050 using modeling data from the EPA as climate impacts will only increase. We
aspire to develop an open-access tool using R and ArcGIS that can be used by community members,
policy makers, and researchers alike, to better understand the linkages of harm between carceral
communities and the marginalized communities they are often collocated with. This tool fills a critical
education gap, while providing quantitative backing to advocate for vulnerable communities."),
                br(),
                br()),

tags$footer(
  h6(strong("Incarceration and Environmental Injustice: An Interactive Look at the Exposure of
            Carceral Facilities Environmental Hazards")),
  h6(em("Created by Matthieu Huy for Environmental Science and Management 244
        (Advanced Data Analytics)")),
  br())),

##### UI: Maps Page #################################

tabPanel("Mapping",
         sidebarPanel(
           tags$div(
             selectInput(
               inputId = "SelectHazard",
               label = "Select Hazard",
               choices = c("Superfund Sites",
                           "Heat Risk",
                           "Flood Risk")
             ),
             p(strong("Superfund Sites"), "are designated by the EPA (include data link and further explanation here"),
             p(strong("Heat Risk"), "data from (further explain and link)"),
             p(strong("Flood Risk"), "data from (further explain and link)")
           )
         ), #end sidebarPanel

         mainPanel(
           tags$div(
             h3("Mapping Carceral Facilities' Exposure to Environmental Hazards",
                style = "font-weight: bold"),
             p("On this page, we'll look at blah blah blah.")),
           plotOutput("state_plot"),
           tags$div(
             p("Write some analysis of what we see on the map and what all this stuff means here."),
             style = "padding: 10px 10px 0px 0px"
           ),
           tags$footer(
             p("These maps were created using data provided by (list data sources here)"),
             br(),
             br()
           )
         ) #end mainPanel
), #end Mapping

##### References ########################

tabPanel("References")
) #end navbarPage

# end ui


# Define server logic required to draw a histogram
server <- function(input, output) {

  #state_reactive <- reactive({
   # state_data %>%
     # filter(region %in% input$pick_state)
  #}) #end state_reactive

  output$state_plot <- renderPlot(
    us_counties_map
  )#end output$state_plot
  #output$counties_t_map = renderTmap({
  # tm_shape(county_data) +
  # tm_polygons("")

}
# }


# Run the application
shinyApp(ui = ui, server = server)
