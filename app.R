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
  select(county = county.x, state = state.x, historical, projected = no_action_5)
  #filter(state %in% c("California", "Arizona", "Nevada", "New Mexico", "Texas",
                     # "Oklahoma", "Louisiana", "Arkansas")) ### same states included in raster


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
                 people, as they have little agency over their living conditions. Reports have documented the
                 connection between carceral spaces and impacts to human health from exposure to toxic chemicals as well as extreme heat.
                 This website aims to explore and visualize the severity of this exposure. Similarly, reports from across the U.S. have identified incarcerated people left
                 behind in the face of deadly natural disasters, and flood risk is another area we would like to explore in future analyses."),
               br(),
               p("This RShiny app attempts a first-of-its-kind study mapping carceral facilities
                 across the U.S. to determine how close they are to toxic land, as well as how likely they are to experience extreme heat events
                 now, and in midcentury (years 2040-2070) using modeling data from the EPA as climate impacts will only increase.
                 We aspire to develop an open-access tool using R and ArcGIS that can be used by community members,
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
), ### End Home

##### Superfund Maps Page #################################

tabPanel("Superfund Sites",

         tags$div(
           h3(strong("Background")),
           br(),
           p(strong('From Baker E., Puig-Santana, A., Kaveh, S., Lenihan, T. "Assessing the risk and proximity of carceral facilities
               to superfund sites." (in preparation):')),
           p("In 1980 Congress established the Comprehensive Environmental Response,
             Compensation, and Liability Act (CERCLA), informally referred to as Superfund,
             to address the mismanagement of hazardous waste throughout the United States.
             The Superfund program provides the Environmental Protection Agency (EPA) with
             funding and the authority to identify and clean up Superfund sites through a
             remediation process. While the program has had successes, it also has flaws.
             There have been documented issues with getting a proposed site approved,
             and there are many sites with no clean-up date in sight."),
            br(),
            p("Modern life is dependent on many facilities and resources that can lead to the creation
              of a Superfund site. These include, but are not limited to: airports, power plants,
              hazardous landfills, waste incinerators, and mining and extractive activities.
              These are all examples of what land use planners refer to as “locally undesirable
              land uses” (LULUs). Opsal and Malin argue that prisons themselves are another form of
              LULU, as they are pushed out of wealthy communities and into poorer or more marginalized
              communities. (Opsal and Malin, 2020) Because Superfund sites are hazardous, they have often been utilized for
              siting LULUs, including prisons."),

           br(),

           tags$div(
             h3("Mapping Carceral Facilities' Exposure to Superfund Sites",
                style = "font-weight: bold")),
style = "padding: 20px 100px 40px 20px"
         ),

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
             p("These maps were created using data on active superfund sites from the", strong("EPA's Superfund National Priorities List (NPL)"),
               ", with state and county boundary shapefiles from", strong("U.S. Census Bureau.")),
             style = "color: black;"
           ),
         ), #end sidebarPanel

         mainPanel(

           withSpinner(
          tmapOutput("county_superfund_plot")),
           tags$div(
             br(),
             p("Although carceral facilities can contribute to environmental hazards, they are also built
             near or on undesirable lands, such as a Superfund site. Research on Superfund sites is
             challenging because of the fact that no two superfund sites are the same. In addition,
             carceral facilities located near or at a Superfund site can expose individuals with little
             to no agency to change their conditions to a myriad of hazards and toxins - resulting in
             environmental injustice. Currently, approximately 600 federal and state prisons fall within
             a 3-mile radius of a listed Superfund site, with over 100 falling within a 1-mile radius of
             a hazardous site (Bernd et al., 2017)"),
             br(),
             br(),
             br(),
             style = "padding: 10px 10px 0px 0px"
           )
         ), #end mainPanel

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
), #end Superfund


##### Heat Maps Page #################################

tabPanel("Heat Risk",

         tags$div(
           br(),
           h4(strong("“People tell us that they are suffering without access to breathable air.
                     Every summer we know this is coming and nothing changes.”"),
              style =
                "float:      center;
                text-align: center;"),
           p("- Julie Skarha, PhD in epidemiology at Brown University and author of
             'Heat-related mortality in U.S. state and private prisons: A case-crossover analysis'.",
             style =
               "float:      center;
                text-align: center;
                bottom:     40px;
                width:      100%;
                color:      gray;
                padding:    0px 0px 40px 20px;
                z-index:    1;"),
           h3(strong("Background")),
              br(),
              p("Heat-related deaths in are not uncommon in U.S. Prisons during the hot summer months of June, July, and August.
                Incarcerated people are uniquely vulnerable to heat-related illness for a multitude of reasons.
                People in prison are disproportionately diagnosed with diseases such as diabetes and have been found to age
                more rapidly while incarcerated. Mental-illness is also much more prevalent among incarcerated people, and
                drugs used to treat certain mental illnesses are known to cause heat-sensitivity (Brown, 2023).

                A study by Julie Skarha published in the peer-reviewed scientific journal PLOS ONE established a link
                between extreme heat and prison mortality. Researchers found that a temperature increase of 10-degrees above
                the average was associated with a significant increase in prison mortality, with deaths increasing 5.2% - or 6.7%
                among people diagnosed with heart disease. Although no publicly available national
                data exists to indicate prisons that lack air conditioning, the increase in mortality was much higher
                in the Northeast, at 21%, where prisons are less likely to have air conditioning or to be
                prepared to handle heatwaves. Shockingly, Suicide rates increased by 22.8% in the three days
                following an extreme heat event, as desperate prisoners reach a dangerous tipping point during these periods (Skarha et al., 2023)."),

           br(),

           tags$div(
             h3("Mapping Carceral Facilities' Exposure Heat",
                style = "font-weight: bold")),
           style = "padding: 20px 100px 40px 20px"
         ),

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

             p(strong("Historical"), ": This map displays the average days per year from 1971-2000 with a heat index
               above 100°F. Heat index is defined as the 'feels like' temperature resulting from the combination of temperature and humidity."),
             p(strong("Projected"), ": This map displays the projected average days per year with a heat index
               above 100°F in midcentury (2036-2065), with little to no action to curb current heat-trapping emissions growth."),
             p(strong("Climate Change"), ": This map displays the projected increase in average daily temperatures
               by midcentury (2041-2070)."),
             br(),
             p("These maps were created using data on 'Days with a heat index above 100°F' from",
               strong("the Union of Concerned Scientists"), "as well as models of average temperature
               increase due climate change from the", strong("EPA's LASSO tool for climate change data.")),
             style = "color: black;"
           )
         ), #end sidebarPanel

         mainPanel(

           withSpinner(
             tmapOutput("county_heat_map")),
           tags$div(
             br(),
             p("Across the U.S, the number of days with a heat index above 100°F is expected to
               increase substantially by midcentury, with some areas that rarely experienced such heat
               being subject to it for the first time. Results from combining multiple climate change
               models by the EPA also predict an increase in average daily temperatures across the U.S. As
               this new reality comes into being, extreme heat events will become more frequent as will heat-related
               deaths in prisons. Prisons are not equipped to protect inmates from prolonged exposure to heat.
               This data highlights the need for prison reform and is a call to action for policymakers."),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             style = "padding: 10px 10px 0px 0px"
           )
         ), #end mainPanel

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
         ) #end footer
), #end Heat Risk

##### References #################################

tabPanel("References",

           tags$div(h1("Acknowledgements", style = "font-weight: bold")),
           tags$div(
             br(),
             p("This project was intended to complement a working paper by Masters students at the Bren School of Environmental Science and Management:"),
             p(strong('Baker E., Puig-Santana, A., Kaveh, S., Lenihan, T. "Assessing the risk and proximity of carceral facilities
               to superfund sites." (in preparation).')),
             br(),
             p('Excerpts from this paper are included in the "Superfund Sites" page to provide context. Thank you to Elijah Baker and Alessandra Puig-Santana for their help.'),
             p('Thank you to Shayan Kaveh for his work in extracting and cleaning the
               data from HIFLD and the EPA on prison boundaries and superfund sites in preparation for this geospatial analysis.'),
             br()
           ),

           tags$div(h1("References", style = "font-weight: bold")),
           tags$div(
             br(),
             p(a("Bernd, C., Loftus-Farren, Z., & Maureen Nandini Mitra. (1 June, 2017).",
                 href ="https://truthout.org/articles/america-s-toxic-prisons-the-environmental-injustices-of-mass-incarceration/"),
               '"America’s Toxic Prisons: The Environmental Injustices of Mass Incarceration."', em("Truthout.")),
             br(),
             p(a("Brown, Allen. (1 Mar. 2023).  ",
                 href = "https://grist.org/equity/new-study-people-dying-extreme-heat-in-prisons-us/?utm_campaign=Hot+News&utm_medium=email&_hsmi=248459899&_hsenc=p2ANqtz-_hs_Me2aJ16fjDL4QWqfYYLYqxauPN0cPn53EtXGqGdrmizF5SOAosR13LzqXTp-syv-1cFVsm04Zx3ULwnNFs3m8rDA&utm_content=248459899&utm_source=hs_email"),
               '"Study: Extreme Heat Is Driving Deaths in US Prisons."', em("Grist.")),
             br(),
             p(a("Dahl, Kristina, Erika Spanger-Siegfried, Rachel Licker, Astrid Caldas, John Abatzoglou, Nicholas Mailloux, Rachel Cleetus, Shana Udvardy, Juan Declet-Barreto, and Pamela Worth. 2019.",
                 href =  "https://www.ucsusa.org/resources/killer-heat-united-states-0"),
               '"Killer Heat in the United States: Climate Choices and the Future of Dangerously Hot Days."', em("Cambridge, MA: Union of Concerned Scientists."), "91, 73–77."),
             br(),
             p(a(em("LASSO : Locating and Selecting Scenarios Online."),
                 href = "https://lasso.epa.gov"),
               "Environmental Protection Agency."),
             br(),
             p(a("Opsal, T., & Malin, S. A. (2020).",
                 href = "https://doi.org/10.1111/soin.12290"),
               '"Prisons as LULUs: Understanding the Parallels between Prison Proliferation and Environmental Injustices."', em("Sociological Inquiry,"), "90(3), 579–602."),
             br(),
             p(a(em("Prison Boundaries."), " (2020).",
                 href = "https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::prison-boundaries/about"),
              "Homeland Infrastructure Foundation Level Data (HIFLD)."),
             br(),
             p(a("Skarha J, Spangler K, Dosa D, Rich JD, Savitz DA, Zanobetti A (2023).",
                 href = "https://doi.org/10.1371/journal.pone.0281389"),
               '"Heat-related mortality in U.S. state and private prisons: A case-crossover analysis."', em("PLOS ONE.")),
             br(),
             p(a(em("State and County TIGER/Line Shapefiles."), " (2022).",
                 href = "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2022.html#list-tab-790442341"),
               "U.S. Census Bureau."),
             br(),
             p(a(em("Superfund: National Priorities List (NPL)."), "(updated 22 Feb. 2023)",
                 href = "https://www.epa.gov/superfund/superfund-national-priorities-list-npl"),
               "Environmental Protection Agency. (accessed 25 Aug. 2022)."),
            br(),
            br(),
            br(),
            br(),
            br()
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
         ) #end footer
         ) # end refs page

) #end navbarPage

############# end ui ################################


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
                title = "Number of days above 100°F",
                palette = "YlOrRd",
                alpha = 0.8,
                breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
                labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80+")) +
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
                title = "Number of days above 100°F",
                palette = "YlOrRd",
                alpha = 0.8,
                breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
                labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80+")) +
        tm_polygons() +
        tm_shape(state_prisons) +
        tm_dots(id = "name",
                col = "slategray2",
                size = 0.015,
                legend.show = FALSE)
    }

    else if(input$heat_map == "Climate Change") {
      tm_shape(temp_raster_clipped) +
        tm_raster(title = "Temperature increase (°F)",
                  palette = "YlOrRd",
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
