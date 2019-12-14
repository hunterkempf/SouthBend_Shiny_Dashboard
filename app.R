#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)
library(leaflet)
library(tidyverse)
library(htmltools)
library(sf)


#### Code that runs once at the beginning #####
# Leaflet Basemap
Leaflet_map <- leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE))

# Ammenities Datasets
facilities.points <- read_csv(
    "./Data/Public_Facilities.csv",
    col_types = cols(
        POPL_NAME = col_character(),
        POPL_TYPE = col_character(),
        POPL_ADDR1 = col_character(),
        POPL_CITY = col_character(),
        POPL_ZIP = col_double(),
        POPL_STATE = col_character(),
        POPL_PHONE = col_character(),
        FID = col_double(),
        Lat = col_double(),
        Lon = col_double()
    )
)
park.points <- read_csv(
    "./Data/Parks_Locations_and_Features.csv",
    col_types = cols(
        .default = col_double(),
        Park_Name = col_character(),
        Park_Type = col_character(),
        Address = col_character()
    )
)

# Census Data
Census_st <-
    st_read(
        "./Data/2010_CensusData/2010_CensusData.shp",
        quiet = TRUE,
        stringsAsFactors = FALSE
    )


# Combine Datasets for Leaflet Plot
ammenities.points <-
    facilities.points %>% select(Name = POPL_NAME, Type = POPL_TYPE, Lat, Lon)
ammenities.points <-
    park.points %>% select(Name = Park_Name, Type = Park_Type, Lat, Lon) %>% rbind(ammenities.points)

# Set Pin Color for leaflet
getColor <- function(inType) {
    sapply(inType, function(Type) {
        if (Type == "LIBRARY") {
            "purple"
        } else if (Type == "POLICE STATION") {
            "darkblue"
        } else if (Type == "FIRE STATION") {
            "red"
        } else if (Type == "Cemetery") {
            "lightgray"
        } else if (Type == "Golf Course") {
            "darkgreen"
        } else if (Type %in% c("Community Park", "Neighborhood Park", "Block Park")) {
            "lightgreen"
        } else if (Type == "Zoo") {
            "orange"
        } else if (Type == "Memorial") {
            "lightgray"
        } else {
            # Should only be Special Parks
            "pink"
        }
    })
}
# Add pin color to ammenities and parks dataframes
ammenities.points <-
    ammenities.points %>% mutate(pinColor = getColor(ammenities.points$Type))
park.points <-
    park.points %>% mutate(pinColor = unname(getColor(park.points$Park_Type)))

# make legend lists for ammenities
color_list = c(
    "purple",
    "darkblue",
    "red",
    "gray",
    "darkgreen",
    "lightgreen",
    "orange",
    "lightgray",
    "pink"
)
label_list = c(
    "Library",
    "Police Station",
    "Fire Station",
    "Cemetery",
    "Golf Course",
    "Park",
    "Zoo",
    "Memorial",
    "Special"
)

# make legend lists for parks
color_list_parks = c("gray",
                     "darkgreen",
                     "lightgreen",
                     "orange",
                     "lightgray",
                     "pink")
label_list_parks = c("Cemetery", "Golf Course", "Park", "Zoo", "Memorial", "Special")



# Define UI for application that draws a histogram
ui <- navbarPage("South Bend Dashboard",
                 tabPanel("Public works",
                          fluidPage(
                              # Application title
                              titlePanel("City of South Bend Ammenities"),
                              
                              tabsetPanel(
                                  tabPanel("Map by Type",
                                           sidebarLayout(
                                               sidebarPanel(
                                                   checkboxGroupInput(
                                                       inputId = "FacilityType",
                                                       label = "Select Amenity Types to Plot",
                                                       choices = c(
                                                           "FIRE STATION",
                                                           "LIBRARY",
                                                           "POLICE STATION",
                                                           "Block Park",
                                                           "Cemetery",
                                                           "Community Park",
                                                           "Golf Course",
                                                           "Memorial",
                                                           "Neighborhood Park",
                                                           "Special",
                                                           "Zoo"
                                                       ),
                                                       selected = "FIRE STATION"
                                                   ),
                                                   radioButtons(
                                                       inputId = "aggSelect",
                                                       label= "Select Aggregation",
                                                       choiceNames = c(
                                                           "Count",
                                                           "Count/Population"
                                                       ),
                                                       choiceValues = c(
                                                           "count",
                                                           "countDivPop"
                                                       )
                                                   ),
                                                   p("Made with", a("Shiny", href = "http://shiny.rstudio.com"), ".")
                                               ),
                                               
                                               # Show a plot of the generated distribution
                                               mainPanel(
                                                   span(textOutput("noneSelectedWarningAmmenities"), style = "color:red"),
                                                   leafletOutput("amenitiesLeaflet"),
                                                   leafletOutput("AggLeaflet")
                                               )
                                           )),
                                  tabPanel(
                                      "Map by Park Features",
                                      sidebarLayout(
                                          sidebarPanel(
                                              style = "overflow-y:scroll; max-height: 400px",
                                              checkboxGroupInput(
                                                  "ParkFeature",
                                                  "Filter Park Features",
                                                  choices = c(
                                                      "Aqua_Feat__Pool",
                                                      "Aqua_Feat__Spray",
                                                      "Backstop__Practice",
                                                      "Ballfield",
                                                      "Basketball",
                                                      "Blueway",
                                                      "Complex__Ballfield",
                                                      "Complex__Tennis",
                                                      "Concessions",
                                                      "Disk_Golf",
                                                      "Driving_Range",
                                                      "Educational_Experience",
                                                      "Event_Space",
                                                      "Fitness_Course",
                                                      "Garden__Community",
                                                      "Garden__Display",
                                                      "Golf",
                                                      "Hockey__Ice",
                                                      "Loop_Walk",
                                                      "MP_Field__Large",
                                                      "MP_Field__Multiple",
                                                      "MP_Field__Small",
                                                      "Multiuse_Court",
                                                      "Natural_Area",
                                                      "Open_Turf",
                                                      "Open_Water",
                                                      "Other___Active",
                                                      "Other_Passive",
                                                      "Passive_Node",
                                                      "Picnic_Grounds",
                                                      "Playground__Destination",
                                                      "Playground__Local",
                                                      "Public_Art",
                                                      "Shelter",
                                                      "Shelter__Group",
                                                      "Skate_Park",
                                                      "Sledding_Hill",
                                                      "Structure",
                                                      "Tennis",
                                                      "Trail__Primitive",
                                                      "Volleyball",
                                                      "Water_Access__Developed",
                                                      "Water_Access__General",
                                                      "Water_Feature"
                                                  ),
                                                  selected = NULL,
                                                  inline = FALSE,
                                                  width = NULL,
                                                  choiceNames = NULL,
                                                  choiceValues = NULL
                                              )
                                          ),
                                          
                                          # Show a plot of the generated distribution
                                          mainPanel(
                                              span(htmlOutput("noneSelectedWarningParks"), style = "color:red"),
                                              leafletOutput("park_details")
                                          )
                                      )
                                  ) ,
                                  tabPanel(
                                      "Raw Data",
                                      basicPage(
                                          h2("Park Data"),
                                          downloadButton("downloadPark", "Download"),
                                          dataTableOutput('parks'),
                                          h2("Facilities Data"),
                                          downloadButton("downloadFacilities", "Download"),
                                          dataTableOutput('facilities')
                                      )
                                  )
                              )
                              
                          )),
                 tabPanel("Education and Business", fluidPage()),
                 tabPanel("Real Estate", fluidPage()),
                 tabPanel("Constituent Issues", fluidPage())
                 )

# Define server logic required to draw a histogram
server <- function(input, output) {
    # make datatables
    output$parks <- renderDataTable(datatable(park.points))
    output$facilities <- renderDataTable(datatable(facilities.points))
    
    # make download buttons
    output$downloadPark <- downloadHandler(
        filename = function() {
            paste("ParkData", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(park.points, file)
        }
    )
    output$downloadFacilities <- downloadHandler(
        filename = function() {
            paste("FacilitiesData", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(facilities.points, file)
        }
    )
    
    # Make Warning Text Labels
    output$noneSelectedWarningAmmenities <- renderText({
        ammenities.show <-
            ammenities.points %>% filter(Type %in% input$FacilityType)
        if (dim(ammenities.show)[1] != 0) {
            ""
        } else {
            "Please Select an Ammenity Type to Plot"
        }
    })
    
    output$noneSelectedWarningParks <- renderUI({
        # Read input column and find the values that arent NA within that column
        cols <- c(input$ParkFeature,"Park_Name","Park_Type","pinColor","Lat","Lon")
        
        park.points.subset <- select(park.points, one_of(cols))
        park.show <- park.points.subset[complete.cases(park.points.subset),]
        if (dim(park.show)[1] != 0) {
            ""
        } else {
            HTML(paste("There are no parks in south bend with all of those features... ",
                    "Please try again with another set of features",sep = "<br/>"
                )
            )
        }
    })
    
    output$amenitiesLeaflet <- renderLeaflet({
        ammenities.show <- ammenities.points %>% filter(Type %in% input$FacilityType)
        
        if (dim(ammenities.show)[1] != 0) {
            # Make Html label with Point Name and Type
            pin_labels <-
                sprintf("Name - %s:<br/>Type - %s",
                        ammenities.show$Name,
                        ammenities.show$Type) %>%
                lapply(htmltools::HTML)
            Leaflet_map %>% addAwesomeMarkers(
                lng = ammenities.show$Lon,
                lat = ammenities.show$Lat ,
                icon = awesomeIcons(
                    icon = 'ion-ionic',
                    library = 'ion',
                    markerColor = ammenities.show$pinColor
                ),
                label = pin_labels
            ) %>%
                leaflet::addLegend(
                    colors = color_list,
                    labels = label_list,
                    opacity = .7,
                    title = NULL
                )
        } else {
            # if map is empty set to south bend with no values
            Leaflet_map %>%
                setView(lng = -86.2520,
                        lat = 41.6764,
                        zoom = 10)
        }
    })
    
    output$AggLeaflet <- renderLeaflet({
        #filter type then project the table as an sf and set the coordinate system
        ammenities.show.spatial <- ammenities.points %>%
            filter(Type %in% input$FacilityType) %>%
            st_as_sf(coords = c("Lon", "Lat")) %>%
            st_set_crs("+init=epsg:4269")
        
        # if there are rows to use continue otherwise print an empty graph
        if (dim(ammenities.show.spatial)[1] != 0) {
            Census_st <- st_transform(Census_st, "+proj=longlat +datum=WGS84")
            ammenities.show.spatial <- st_transform(ammenities.show.spatial,"+proj=longlat +datum=WGS84")
            
            Census_count_join <- st_join(ammenities.show.spatial, Census_st, join = st_within) %>%
                count(NAME)  %>% as_tibble() %>%
                select(NAME, count = n)
            Census_st_joined <- left_join(Census_st, Census_count_join, by = "NAME")
            Census_st_joined$count[is.na(Census_st_joined$count)] <- 0 # fill na with 0 from join
            Census_st_joined$countDivPop <- Census_st_joined$count/Census_st_joined$SE_T001_00
            
            # Create leaflet
            # set color scheme on census tract counts
            pal <- colorNumeric("YlOrRd", domain = Census_st_joined[[input$aggSelect]])
            
            # Make Html label with Census tract name and the count
            labels <- sprintf( "%s:<br/>Count - %g", Census_st_joined$Geo_QName,Census_st_joined[[input$aggSelect]]) %>%
                lapply(htmltools::HTML)
            
            # plot leaflet censu polygon map
            leaflet(Census_st_joined) %>%
                addProviderTiles(providers$Stamen.TonerLite,
                                 options = providerTileOptions(noWrap = TRUE)) %>%
                addPolygons(
                    fillColor = ~ pal(Census_st_joined[[input$aggSelect]]),
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    label = labels
                ) %>%
                leaflet::addLegend(
                    pal = pal,
                    values = ~ Census_st_joined[[input$aggSelect]],
                    opacity = 0.7,
                    title = NULL
                )
        } else {
            # if map is empty set to south bend with no values
            Leaflet_map %>%
                setView(lng = -86.2520,
                        lat = 41.6764,
                        zoom = 10)
        }
    })
    
    output$park_details <- renderLeaflet({
        # Read input column and find the values that arent NA within that column
        cols <-
            c(input$ParkFeature,
              "Park_Name",
              "Park_Type",
              "pinColor",
              "Lat",
              "Lon")
        park.points.subset <- select(park.points, one_of(cols))
        park.show <- park.points.subset[complete.cases(park.points.subset),]
        if (dim(park.show)[1] != 0) {
            # Make Html label with Census tract name and the count
            pin_labels <- sprintf("Name - %s:<br/>Type - %s", park.show$Park_Name, park.show$Park_Type) %>%
                lapply(htmltools::HTML)
            
            Leaflet_map %>% addAwesomeMarkers(
                lng = park.show$Lon,
                lat = park.show$Lat ,
                icon = awesomeIcons(
                    icon = 'ion-ionic',
                    library = 'ion',
                    markerColor = park.show$pinColor
                ),
                label = pin_labels
            ) %>%
                leaflet::addLegend(
                    colors = color_list_parks,
                    labels = label_list_parks,
                    opacity = .7,
                    title = NULL
                )
        } else{
            # if map is empty set to south bend with no values
            Leaflet_map %>%
                setView(lng = -86.2520,
                        lat = 41.6764,
                        zoom = 10)
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
