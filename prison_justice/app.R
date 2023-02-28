####### Prison Justice App ##########################
####### Matthieu Huy, March 2023 ####################


#Load packages


library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(sf)
library(janitor)
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



### Style ###############################################

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



### Read in data ##########################################

state_sf <- read_sf(here("data/tl_2022_us_state/tl_2022_us_state.shp")) |>
    clean_names()

county_sf <- read_sf(here("data/tl_2022_us_county/tl_2022_us_county.shp")) |>
  clean_names()

superfund_csv <- read_csv(here("data/superfund_data/superfund_data_updated.csv")) |>
  clean_names()

superfund_sf <- st_as_sf(superfund_csv,
                         coords = c("longitude", "latitude"))

#st_crs(county_sf) ### EPSG 4296
#st_crs(superfund_sf) ### no crs

superfund_sf <- st_set_crs(superfund_sf, st_crs(county_sf))

#st_crs(superfund_sf) ### EPSG 4296

superfund_buffers_1m_sf <- read_sf(here("data/superfund_data/active_sites_1.shp"))

superfund_buffers_3m_sf <- read_sf(here("data/superfund_data/active_sites_3mile.shp"))

prison_boundaries_sf <- read_sf(here("data/Prison_Boundaries/Prison_Boundaries.shp")) |>
  clean_names()

superfund_buffers_3m_sf <- st_set_crs(superfund_buffers_3m_sf, st_crs(county_sf))
prison_boundaries_sf <- st_transform(prison_boundaries_sf, st_crs(county_sf))


### clean/prep data #########################################





### create outputs/maps/plots ###############################



### UI ##########################################
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

tabPanel("Interactive U.S. Map",
         sidebarPanel(
           tags$div(
             selectInput(
               inputId = "select_hazard",
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
           withSpinner(
           tmapOutput("prison_map")),
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
), #end Interactive Map

tabPanel("Mapping by State",
         sidebarPanel(
           tags$div(
             selectInput(
               inputId = "select_state",
               label = "Select State",
               choices = unique(prison_boundaries_sf$state_1)
             ) #end selectinput
           ), #end tags$div
           tags$div(
             selectInput(
               inputId = "select_hazard",
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
           # withSpinner(
           #   leafletOutput(outputId = "prison_map")
           # ),
           withSpinner(
          tmapOutput("test")),
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

  tmap_mode("view")
  output$test = renderTmap({
    tm_shape(state_sf) +
      tm_polygons()

  })

  #state_reactive <- reactive({
   # state_data %>%
     # filter(region %in% input$pick_state)
  #}) #end state_reactive

  #output$state_plot <- renderPlot(
    #us_counties_map
  #end output$state_plot
  #prison_boundaries_3_reactive <- reactive({
    #prison_boundaries_3_sf %>%
      #filter(state_1 %in% input$select_state)
  #})
  tmap_mode("view")
  output$prison_map = renderTmap({

    tm_shape(superfund_buffers_3m_sf) +
      tm_fill(col = "red",
              alpha = 0.3) +
      tm_polygons() +
      tm_shape(superfund_buffers_1m_sf) +
      tm_fill(col = "darkred",
              alpha = 0.3) +
      tm_polygons() +
      tm_shape(prison_boundaries_sf) +
      tm_dots("name",
              palette = "Reds",
              legend.show = FALSE)
    })
}


# Run the application
shinyApp(ui = ui, server = server)
