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

prison_boundaries_sf$county <- str_to_title(prison_boundaries_sf$county)

superfund_buffers_3m_sf <- st_set_crs(superfund_buffers_3m_sf, st_crs(county_sf))
prison_boundaries_sf <- st_transform(prison_boundaries_sf, st_crs(county_sf))


### clean/prep data #########################################

state_names <- state_sf |>
  as.data.frame() |>
  select(statefp, state = name)
county_state_names_df <- county_sf |>
  as.data.frame() |>
  select(statefp, countyfp, county = name) |>
  inner_join(state_names, by = c("statefp")) |>
  mutate(countyfips = paste(statefp, countyfp, sep = "")) |>
  select(county, state, countyfips)
county_state_names_sf <- county_sf |>
  select(statefp, countyfp, county = name) |>
  inner_join(state_names, by = c("statefp")) |>
  mutate(countyfips = paste(statefp, countyfp, sep = "")) |>
  select(county, state, countyfips)

prison_boundaries_sf <- prison_boundaries_sf |>
  select(-c("state", "county")) |>
  inner_join(county_state_names_df, by = c("countyfips"))


### UI ##########################################
ui <- shiny::navbarPage(theme = "shiny_theme.css",
  title = tags$div(
    "Intentional
    Indifference"),
    tags$head(
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "shiny_theme.css")
    ),
    tabPanel("Home",
             img(src = "prison_logo3.png", height = "100%", width = "100%"),

             tags$div(
               h3(strong("About this site")),
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
                br(),
style = "padding: 20px 100px 40px 20px"
),

tags$footer(
  h6(strong("Intentional Indifference: An Interactive Look at the Exposure of
            U.S. Carceral Facilities to Environmental Hazards")),
  h6(em("Created by Matthieu Huy for Environmental Science and Management 244
        (Advanced Data Analytics)")),
  br(),
  style =
    "float:      center;
                text-align: left;
                bottom:     40px;
                width:      100%;
                height:     10px;
                color:      gray;
                padding:    30px 100px 40px 20px;
                z-index:    1;"
  )
),

##### UI: Maps Page #################################

tabPanel("Mapping by County",
         sidebarPanel(
           tags$div(
             selectInput(
               inputId = "state",
               label = "Select a state:",
               choices = unique(prison_boundaries_sf$state)
             ), #end selectinput
             selectInput(
               inputId = "county",
               label = "Select a county:",
               choices = NULL
             )
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
           ),
           tags$footer(
             p("These maps were created using data provided by (list data sources here)"),
             br(),
             br()
           )
         ), #end sidebarPanel

         mainPanel(
           tags$div(
             h3("Mapping Carceral Facilities' Exposure to Environmental Hazards",
                style = "font-weight: bold"),
             p("On this page, we'll look at blah blah blah.")),
           withSpinner(
          tmapOutput("county_plot")),
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
server <- function(input, output, session) {

  #Update the choices of county selectInput based on selected state
  observeEvent(input$state, {
    state_input <- input$state

    choices <- prison_boundaries_sf |>
      filter(state == state_input) |>
      pull(county)

    updateSelectInput(session, "county",
                      choices = choices)
  })


  tmap_mode("view")
  output$county_plot <- renderTmap({
    county_input <- input$county
    state_input <- input$state
    county_filtered <- county_state_names_sf |>
      filter(state == state_input) |>
      filter(county == county_input)
    county_prisons <- prison_boundaries_sf |>
      filter(state == state_input) |>
      filter(county == county_input)

    tm_shape(county_filtered) +
      tm_fill(col = "lightgrey",
              alpha = 0.3) +
      tm_polygons() +
      tm_shape(superfund_buffers_3m_sf) +
      tm_fill(col = "red",
              alpha = 0.3) +
      tm_polygons() +
      tm_shape(superfund_buffers_1m_sf) +
      tm_fill(col = "darkred",
              alpha = 0.3) +
      tm_polygons() +
      tm_shape(county_prisons) +
      tm_dots("name",
              palette = "Purples",
              legend.show = FALSE)
    })

  #end output$county_plot
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
              palette = "Purples",
              legend.show = FALSE)
    })
}


# Run the application
shinyApp(ui = ui, server = server)
