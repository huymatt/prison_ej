#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(tmap)
library(usmap)

state_data <- map_data('state')

county_data <- map_data('county')

county_names <- read_csv(url('https://raw.githubusercontent.com/pdil/usmap/master/inst/extdata/county_fips.csv'))

state_names <- read_csv(url('https://raw.githubusercontent.com/pdil/usmap/master/inst/extdata/state_fips.csv'))

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

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Prison Justice",
             tabPanel("Overview",
                      textInput("txt"),
                      textOuput("text here")),
             tabPanel("Superfund Sites Map",
                      sidebarLayout(
                        sidebarPanel("widgets",
                                     #dropdown(unique(state_data$region),
                                     # label = "Choose State:",
                                     #inputId = "pick_state") #end dropdown

                                     checkboxGroupInput(inputId = "pick_state",
                                                        label = "Choose State:",
                                                        choices = unique(state_data$region)
                                     ) #end checkbox
                        ), #end sidebarPanel

                        mainPanel("output",
                                  plotOutput("state_plot"),
                                  tmapOutput("counties_tmap")
                        )
                      ) #end sidebarLayout

             ), #end tabpanel Map

             tabPanel("Heat Risk"),
             tabPanel("Flood Risk")
  ) #end navbarPage
)
#end ui

# Application title
#titlePanel("Old Faithful Geyser Data"),

# Sidebar with a slider input for number of bins
# sidebarLayout(
# sidebarPanel(
# sliderInput("bins",
#      "Number of bins:",
#      min = 1,
#     max = 50,
#      value = 30)
#   ),#

# Show a plot of the generated distribution
#   mainPanel(
#    plotOutput("distPlot")
#  )
# )
#)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$text <- renderText({ input$txt })

  state_reactive <- reactive({
    state_data %>%
      filter(region %in% input$pick_state)
  }) #end state_reactive

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
