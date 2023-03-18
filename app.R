####### Prison Justice App ##########################
####### Matthieu Huy, March 2023 ####################


#Load packages


library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(sf)
library(janitor)
library(raster)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(rgdal)
library(spatialEco)
library(glue)
library(here)
library(stringr)
library(ggtext)
library(htmltools)
library(tmap)
library(usmap)
library(shinythemes)
library(rcartocolor)
library(readxl)

select <- dplyr::select ##dplyr::select() was clashing with another function

######################## Read in data ##########################################

### State polygons from US Census Bureau
state_sf <- read_sf(here("data/tl_2022_us_state/tl_2022_us_state.shp")) |>
    clean_names()

### County polygons from US Census Bureau
county_sf <- read_sf(here("data/tl_2022_us_county/tl_2022_us_county.shp")) |>
  clean_names()

### Superfund data
superfund_csv <- read_csv(here("data/superfund_data/superfund_data_updated.csv")) |>
  clean_names()

### convert superfund df to sf
superfund_sf <- st_as_sf(superfund_csv,
                         coords = c("longitude", "latitude"))

#st_crs(county_sf) ### EPSG 4296
#st_crs(superfund_sf) ### no crs

### set crs for superfund_sf to same same county_sf
superfund_sf <- st_set_crs(superfund_sf, st_crs(county_sf))

#st_crs(superfund_sf) ### EPSG 4296

### superfund 1 mile and 3 mile buffers
superfund_buffers_1m_sf <- read_sf(here("data/superfund_data/active_sites_1.shp"))

superfund_buffers_3m_sf <- read_sf(here("data/superfund_data/active_sites_3mile.shp"))

### prison locations from US Bureau of Prisons

prison_boundaries_sf <- read_sf(here("data/Prison_Boundaries/Prison_Boundaries.shp")) |>
  clean_names()

### convert all caps county names to match names in county_sf
prison_boundaries_sf$county <- str_to_title(prison_boundaries_sf$county)

### set crs for superfund buffers and prisonsto same same county_sf
superfund_buffers_3m_sf <- st_set_crs(superfund_buffers_3m_sf, st_crs(county_sf))
prison_boundaries_sf <- st_transform(prison_boundaries_sf, st_crs(county_sf))

### county heat data (current days above 100F from union of concerned scientists)
county_heat_100 <- readxl::read_excel(here("data/killer-heat-data-by-county.xlsx"),
                                      sheet = 3, skip = 2)

######### temperature climate projection rasters

### download all climate change models/scenarios suggested by EPA's LASSO Tool
### temp rasters region 9 (CA, NV, AZ)

temp_9a <- raster(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_CanESM2_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9b <- raster(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9c <- raster(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC-ESM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9d <- raster(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9e <- raster(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MIROC5_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_9f <- raster(here("rasters/climate_scenarios/lasso_data_9/LOCA_rcp85_eparegion9_MRI-CGCM3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))

temp9_stack <- stack(temp_9a, temp_9b, temp_9c, temp_9d, temp_9e, temp_9f) # Put all temp rasters in a stack

temp9_raster_avg <- calc(temp9_stack, mean) #avg all climate models to generate 1 "mean" raster layer

### temp rasters region 6 (AR, LA, NM, OK, TX)

temp_6a <- raster(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_ACCESS1-3_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6b <- raster(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_HadGEM2-ES_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6c <- raster(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_inmcm4_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6d <- raster(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_IPSL-CM5A-MR_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))
temp_6e <- raster(here("rasters/climate_scenarios/lasso_data_6/LOCA_rcp85_eparegion6_MIROC-ESM-CHEM_r1i1p1_tas_F_2041_2070_1981_2010_Annual.tif"))

temp6_stack <- stack(temp_6a, temp_6b, temp_6c, temp_6d, temp_6e) # Put all pr rasters in a stack

temp6_raster_avg <- calc(temp6_stack, mean) #avg all climate models to generate 1 "mean" raster layer

temp_raster_merge <- merge(temp9_raster_avg, temp6_raster_avg)

temp_raster_merge <- projectRaster(temp_raster_merge, crs = crs(county_sf))

### clean/prep data #########################################

state_names <- state_sf |>
  as.data.frame() |>
  select(statefp, state = name, state_abb = stusps)

county_state_names_df <- county_sf |>
  as.data.frame() |>
  select(statefp, countyfp, county = name, namelsad) |>
  inner_join(state_names, by = c("statefp")) |>
  mutate(countyfips = paste(statefp, countyfp, sep = "")) |>
  mutate(state_county = paste(state_abb, namelsad, sep = " ")) |>
  select(county, state, countyfips, state_county)

county_state_names_sf <- county_sf |>
  select(statefp, countyfp, county = name, namelsad) |>
  inner_join(state_names, by = c("statefp")) |>
  mutate(countyfips = paste(statefp, countyfp, sep = "")) |>
  mutate(state_county = paste(state_abb, namelsad, sep = " ")) |>
  select(county, state, countyfips, state_county)

prison_boundaries_sf <- prison_boundaries_sf |>
  select(-c("state", "county")) |>
  inner_join(county_state_names_df, by = c("countyfips"))

county_heat_100 <- county_heat_100 |>
  clean_names() |>
  select(state = x1, county = x2, historical, no_action_5) |>
  mutate(state_county = paste(state, county, sep = " "))

county_heat_100_sf <- county_state_names_sf |>
  inner_join(county_heat_100, by = c("state_county")) |>
  select(county = county.x, state = state.x, historical, projected = no_action_5) |>
  filter(state %in% c("California", "Arizona", "Nevada", "New Mexico", "Texas",
                      "Oklahoma", "Louisiana", "Arkansas")) ### same states included in raster


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

tabPanel("Superfund Sites",

         sidebarPanel(
           tags$div(
             selectInput(
               inputId = "state",
               label = "Select a state:",
               choices = sort(unique(prison_boundaries_sf$state))
             ), #end selectinput
             selectInput(
               inputId = "county",
               label = "Select a county:",
               choices = NULL
             ),
             style = "color: black;"
           ), #end tags$div
           tags$div(
             p(strong("Superfund Sites"), "are designated by the EPA (include data link and further explanation here"),
             p(strong("Heat Risk"), "data from (further explain and link)"),
             p(strong("Flood Risk"), "data from (further explain and link)"),
             style = "color: black;"
           ),
           tags$footer(
             p("These maps were created using data provided by (list data sources here)"),
             br(),
             br(),
             style = "color: black;"
           )
         ), #end sidebarPanel

         mainPanel(
           tags$div(
             h3("Mapping Carceral Facilities' Exposure to Superfund Sites",
                style = "font-weight: bold"),
             p("On this page, we'll look at blah blah blah.")),
           withSpinner(
          tmapOutput("county_superfund_plot")),
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
), #end Superfund

##### References ########################

tabPanel("Heat Risk",

         sidebarPanel(
           tags$div(
             selectInput(
               inputId = "state_heat",
               label = "Select a state:",
               choices = sort(unique(county_heat_100_sf$state))
             ), #end selectinput
             radioButtons("heat_map", label = "Choose Heat Map:",
                          choices = c("Historical",
                                      "Projected",
                                      "Climate Change")),
             style = "color: black;"
           ), #end tags$div

           tags$div(

             p(strong("Heat Risk"), "data from (further explain and link)"),
             style = "color: black;"
           ),

           tags$footer(
             p("These maps were created using data provided by (list data sources here)"),
             br(),
             br(),
             style = "color: black;"
           )
         ), #end sidebarPanel

         mainPanel(
           tags$div(
             h3("Mapping Carceral Facilities' Exposure to Heat",
                style = "font-weight: bold"),
             p("On this page, we'll look at blah blah blah.")),
           withSpinner(
             tmapOutput("county_heat_map")),
           tags$div(
             p("Write some analysis of what we see on the map and what all this stuff means here."),
             style = "padding: 10px 10px 0px 0px"
           ),
           tags$footer(
             p("These maps were created using data provided by (list data sources here)"),
             p("Citation:\n Dahl, Kristina, Erika Spanger-Siegfried, Rachel Licker, Astrid Caldas, John Abatzoglou, Nicholas Mailloux, Rachel Cleetus, Shana Udvardy, Juan Declet-Barreto, and Pamela Worth. 2019. Killer Heat in the United States: Climate Choices and the Future of Dangerously Hot Days. Cambridge, MA: Union of Concerned Scientists. https://www.ucsusa.org/resources/killer-heat-united-states-0"),
             br(),
             br()
           )
         ) #end mainPanel
), #end Heat Risk


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
                      choices = sort(choices))
  })

  ### Superfund Sites reactive map ###

  tmap_mode("view")
  output$county_superfund_plot <- renderTmap({
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
      tm_dots(id = "name",
              col = "slategray2",
              size = 0.025,
              legend.show = FALSE)
    })

  ### Heat Risk reactive map ###

  output$map <- renderPrint({ input$radio })

  tmap_mode("view")
  output$county_heat_map <- renderTmap({
    state_heat_input <- input$state_heat
    state_heat_filtered <- county_heat_100_sf |>
      filter(state == state_heat_input)
    state_prisons <- prison_boundaries_sf |>
      filter(state == state_heat_input)

    ###crop heat raster according to county
    temp_raster_cropped <- crop(temp_raster_merge, extent(state_heat_filtered))
    ###clip cropped raster according to county boundaries
    temp_raster_clipped <- mask(temp_raster_cropped, state_heat_filtered)

    if(input$heat_map == "Historical") {
      tm_shape(state_heat_filtered) +
        tm_fill("historical",
                palette = "YlOrRd",
                alpha = 0.8) +
        tm_polygons() +
        tm_shape(state_prisons) +
        tm_dots(id = "name",
                col = "slategray2",
                size = 0.015,
                legend.show = FALSE)

    }

    else if(input$heat_map == "Projected") {
      tm_shape(state_heat_filtered) +
        tm_fill("projected",
                palette = "YlOrRd",
                alpha = 0.8) +
        tm_polygons() +
        tm_shape(state_prisons) +
        tm_dots(id = "name",
                col = "slategray2",
                size = 0.015,
                legend.show = FALSE)
    }

    else if(input$heat_map == "Climate Change") {
      tm_shape(temp_raster_clipped) +
        tm_raster(palette = "YlOrRd",
                  legend.show = TRUE) +
        tm_shape(state_prisons) +
        tm_dots(id = "name",
                col = "slategray2",
                size = 0.015,
                legend.show = FALSE)
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
