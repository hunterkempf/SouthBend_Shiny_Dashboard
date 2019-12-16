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
library(RColorBrewer)
library(sf)
library(sp)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(lubridate)
library(scales)
library(ggmap)
library(shinydashboard)
library(rgeos)

#### Code that runs once at the beginning #####

########## RYAN ########################

monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
}

dat_COX <- read.csv("Data/Code_Enforcement_Cases.csv")
map_COX <- st_read("Data/City_Council_Districts/City_Council_Districts.shp")


# Assignment modified according
df1_COX <- dat_COX %>% st_as_sf(coords = c("Lon", "Lat"))
st_crs(df1_COX) <- 4326
st_crs(map_COX) <- 4326

cases_in_districts_COX <- st_intersects(df1_COX, map_COX)
OBJECTID_COX <- vector()

for (i in 1:length(cases_in_districts_COX)){
    if(length(unlist(cases_in_districts_COX[i]))==0)
        OBJECTID_COX[i] <- NA
    else
        OBJECTID_COX[i] <- unlist(cases_in_districts_COX[i])
}

dat_COX$OBJECTID <- OBJECTID_COX
dat_COX <- dat_COX %>% inner_join(map_COX)
dat_COX$year <- lubridate::year(as.Date(dat_COX$Date_Case_Reported___Calc, format = "%m/%d/%Y"))
dat_COX$month <- monthStart(as.Date(dat_COX$Date_Case_Reported___Calc, format = "%m/%d/%Y"))
dat_COX$monthLabel <- format(dat_COX$month, "%Y-%m")

data_COX <- dat_COX %>%
    group_by(month, monthLabel, Council_Me, Case_Type_Code_Description) %>%
    summarize(calls = n()) %>% 
    filter(month >= "2008-01-01" & month <= "2014-06-01")
data_COX$Council_Me <- as.character(data_COX$Council_Me)

map_COX <- readOGR("Data/City_Council_Districts/City_Council_Districts.shp")

########## CASEY ########################

# Districts Data
# Load in the shape file
districts_cd <- st_read("Data/City_Council_Districts/City_Council_Districts.shp")

# Census data
# Load in the shape file
census_cd <- st_read("Data/2010_CensusData/2010_CensusData.shp")

# Set the coordinate system
st_crs(census_cd) <- 4326

# Limit census data to population and household numbers
census_cd <- census_cd %>% select(Population = SE_T001_00, Households = SE_T058_00, LandArea = SE_T02A_01)

# Find the centroid of the census tracts
census_center_cd <- census_cd %>% st_centroid()

# Find if any of the centers are in the districts
census_districts_cd <- st_intersection(census_center_cd, districts_cd)

# Produce summary statistics
district_summary_cd <- census_districts_cd %>% group_by(Dist) %>% summarize(Pop = sum(Population), Houses = sum(Households), Area = sum(LandArea))


# Street Lights Data
# Load in the CSV
street_lights_cd <- read.csv("Data/Street_Lights.csv")

# Convert to spatial 
street_lights_spatial_cd <- street_lights_cd %>% st_as_sf(coords = c("Lon", "Lat"))

# Set the coordinate reference 
st_crs(street_lights_spatial_cd) <- 4326

# Get the intersection of where streetlights are in the districts 
streetlights_in_districts_cd <- st_intersection(street_lights_spatial_cd,districts_cd)

# Show summary statistics
streetlights_summary_cd <- streetlights_in_districts_cd %>% group_by(Dist) %>% summarise(n = n())

# Combine with other summary data
district_summary_cd$Streetlights <- streetlights_summary_cd$n

# Abandoned Properties Data
# Load in the shape file
abandoned_cd <- st_read("Data/Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp")

# Set coordinate reference
st_crs(abandoned_cd) <- 4326

# Extract the year for the abandoned properties
abandoned_cd$year <- lubridate::year(as.Date(abandoned_cd$Date_of_Ou, format = "%Y-%m-%d"))

# Convert the abandoned lots polygons to data points
abandoned_center_cd <- abandoned_cd %>% st_centroid()

# Get the intersection of where abandoned properties are in the districts
abandoned_in_districts_cd <- st_intersection(abandoned_center_cd,districts_cd)

# Get abandoned counts per year
abandoned_by_year_cd <- abandoned_in_districts_cd %>% group_by(Dist,year) %>% summarize(count = n())
# Remove NA values
abandoned_by_year_cd <- abandoned_by_year_cd %>% drop_na()

# Get abandoned count summary
abandoned_in_districts_summary_cd <- abandoned_in_districts_cd %>% group_by(Dist) %>% summarize(count = n())

# Add to summary data
district_summary_cd$abandoned <- abandoned_in_districts_summary_cd$count


# Summary Data
# Get per capita data and per household data
district_summary_cd$abandoned_pc <- district_summary_cd$abandoned / district_summary_cd$Pop
district_summary_cd$abandoned_ph <- district_summary_cd$abandoned / district_summary_cd$Houses
district_summary_cd$Streetlights_pc <- district_summary_cd$Streetlights / district_summary_cd$Pop
district_summary_cd$Streetlights_ph <- district_summary_cd$Streetlights / district_summary_cd$Houses
district_summary_cd$Streetlights_pa <- district_summary_cd$Streetlights / district_summary_cd$Area

# Assign ranking
streetlight_ranking <- c(6,3,2,1,4,5)
abandoned_ranking <- c(5,6,4,2,1,3)
districts_ranking <- c(1301, 1302, 1303, 1304, 1305, 1306)
ranking <- cbind.data.frame(districts_ranking,streetlight_ranking,abandoned_ranking)

# Register google key
register_google(key = "AIzaSyAe2Brb2eyF0ZeyEncpk_36JQkC-o9Xyvg")

########## OLIVIA ########################

# load school data
school <-
    st_read(
        "Data/School_Boundaries/School_Boundaries.shp",
        quiet = TRUE,
        stringsAsFactors = FALSE
    )

# load business data
bizOL <- read_csv(
    "Data/Business_Licenses_geocoded.csv",
    col_types = cols(
        Business_N = col_character(),
        Classifica = col_character(),
        X = col_double(),
        Y = col_double()
    )
)

# aggregate data to make it easier to plot
bizOL <- bizOL %>% rename (Lon = X, Lat = Y)
bizOL <- filter(bizOL, City == "SOUTH BEND")

# get license count by year
bizOL$License_Ye <- as.numeric(substr(bizOL$License_Ye, 0, 4))
licByYear <- bizOL %>% count(License_Ye)
licByYear<-licByYear[!(licByYear$License_Ye=="-"),]
licByYear$License_Ye <- licByYear$License_Ye

# get license types
licTypes <- unique(bizOL$Classifica)

# get subset of business for table
bizOL2 <- bizOL %>% select(License_Ye, Business_N, Classifi_1)
colnames(bizOL2) <- c("License_Year", "Business_Name", "Business_Type")

########## HUNTER ########################


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
                 ################ HUNTER #####################
                 tabPanel("Public works",
                          fluidPage(
                              # Application title
                              titlePanel("City of South Bend Ammenities"),
                              h4("Built by Hunter Kempf"),
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
                 tabPanel("Education and Business", 
                          ########### OLIVIA #################
                          fluidPage(
                              h4("Built by Olivia Lin"),
                              fluidRow(
                                  column(8,leafletOutput("mapOL")),
                              column(4,sliderInput("year", "License Year Issued", 2015, 2019,
                                                   value = range(bizOL$License_Ye), step = 1, sep = ""),
                                    selectInput("type", "Business Type", licTypes),
                                    checkboxInput("school", "Show schools", FALSE)),
                              DT::dataTableOutput("tableOL")
                          ))),
                 tabPanel("Property", 
                          ########### CASEY #################
                          fluidPage(
                              h4("Built by Casey Donnelly"),
                              dashboardPage(
                                  
                                  dashboardHeader(title = 'Property'),    
                                  dashboardSidebar(
                                      selectInput(                
                                          inputId = "districtselect",                
                                          label = "Select District",                
                                          choices = unique(districts_cd$Dist))                
                                  ),
                                  dashboardBody(
                                      tags$style(type='text/css', '#popheader {font-size:20px; color: black; text-align:center;}'),
                                      tags$style(type='text/css', '#population {font-size:30px; color: blue; text-align:center; font-weight:700;}'),
                                      tags$style(type='text/css', '#streetlightheader {font-size:20px; color: black; text-align:center;}'),
                                      tags$style(type='text/css', '#streetlightrank {font-size:30px; color: blue; text-align:center; font-weight:700;}'),
                                      tags$style(type='text/css', '#abandonedheader {font-size:20px; color: black; text-align:center;}'),
                                      tags$style(type='text/css', '#abandonedrank {font-size:30px; color: blue; text-align:center; font-weight:700;}'),
                                      tags$style(type='text/css', '#streetlight {text-align:center;}'),
                                      tags$style(type='text/css', '#abandoned {text-align:center;}'),
                                      
                                      fluidRow(
                                          column(width = 8,
                                                 box(leafletOutput("PopDensity"), title = 'Population Density', width = NULL)),
                                          #box(plotOutput("AbandonedOverTime"), title = 'New Abandoned Properties per Year', width = NULL)),
                                          column(width = 4,
                                                 br(),
                                                 br(),
                                                 span(textOutput("popheader")),
                                                 span(textOutput("population")),
                                                 br(),
                                                 br(),
                                                 span(textOutput("abandonedheader")),
                                                 span(textOutput("abandonedrank")),
                                                 span(textOutput("abandoned")),
                                                 br(),
                                                 br(),
                                                 span(textOutput("streetlightheader")),
                                                 span(textOutput("streetlightrank")),
                                                 span(textOutput("streetlight"))
                                          )))
                              )
                              
                          )),
                 tabPanel("Constituent Issues", fluidPage(
                     ########### Ryan #################
                     titlePanel(p("Calls by District")), 
                     h4("Built by Ryan Cox"),
                     sidebarLayout(        
                         sidebarPanel(            
                             selectInput(                
                                 inputId = "monthselected",                
                                 label = "Select Month",                
                                 choices = unique(data_COX$monthLabel)                
                             ),
                             selectInput(                
                                 inputId = "casetypeselected",                
                                 label = "Select Case Type",                
                                 choices = append("(ALL)",levels(data_COX$Case_Type_Code_Description))                
                             ),
                             
                             p("Made with", a("Shiny",
                                              href = "http://shiny.rstudio.com"
                             ), ".")
                         ),
                         
                         mainPanel(
                             span(htmlOutput("noCallsSelected_COX"), style = "color:red"),
                             leafletOutput(outputId = "map_COX"),
                             dygraphOutput(outputId = "timetrend_COX"),
                             DTOutput(outputId = "table_COX")
                         )
                     )
                 ))
)


server <- function(input, output) {
    ############ HUNTER ################
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
    
    
    ############ OLIVIA ################
    
    # # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        bizOL %>% filter(License_Ye >= input$year[1] & License_Ye <= input$year[2]) %>% filter(Classifica == input$type)
    })
    
    output$mapOL <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(bizOL) %>% addTiles() %>% fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat)) %>%
            addMarkers(data=bizOL, popup=~bizOL$Business_N, clusterOptions = markerClusterOptions())
    })
    
    # # Incremental changes to the map (in this case, replacing the
    # # circles when a new color is chosen) should be performed in
    # # an observer. Each independent set of things that can change
    # # should be managed in its own observer.
    observe({
        # pal <- colorpal()
        leafletProxy("mapOL") %>% 
            clearMarkers() %>%
            clearMarkerClusters() %>%
            addMarkers(data=(bizOL %>% filter(bizOL$License_Ye >= input$year[1] & bizOL$License_Ye <= input$year[2])), popup=~bizOL$Business_N, clusterOptions = markerClusterOptions())
    })
    
    observe({
        if(input$school == TRUE) {
            leafletProxy("mapOL") %>%
                clearShapes() %>%
                clearControls() %>%
                addPolygons(data=school, 
                            label= as.character(school$School),
                            color = c("orange", "blue")
                ) #%>%
                # Error in get: object '.xts_chob' not found
                #leaflet:addLegend(data = school, 
                #          colors = c("orange", "blue"), 
                #          labels= c("Private", "Public"), 
                #          opacity = 1)
        }
        else {
            leafletProxy("mapOL") %>%
                clearShapes() %>%
                clearControls() 
        }
    })
    
    observe({
        leafletProxy("mapOL") %>% 
            clearMarkers() %>%
            clearMarkerClusters() %>%
            addMarkers(data=(bizOL %>% filter(str_detect(Classifica, input$type))), popup=~bizOL$Business_N, clusterOptions = markerClusterOptions())
    })
    
    # create table for business licenses by year
    output$tableOL = DT::renderDataTable({
        bizOL2
    })
    
    ############ CASEY ################
    
    # Population Density 
    output$PopDensity <- renderLeaflet({
        # Set color scheme for population density 
        pal <- colorNumeric("YlOrRd", domain = district_summary_cd$Pop)
        
        # Create leaflet for population density
        leaflet(districts_cd) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE))%>%
            addPolygons(
                fillColor = ~ pal(district_summary_cd$Pop),
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                popup = district_summary_cd$Pop,
                label = c('1301', '1302', '1304', '1304', '1305', '1306')
            ) %>%
            leaflet::addLegend(
                pal = pal, values = ~district_summary_cd$Pop,
                opacity = 0.7, title = NULL
            )
    })
    
    # Abandoned Lots Over Time
    output$AbandonedOverTime <- renderPlot({
        ggplot(
            data = abandoned_by_year_cd[abandoned_by_year_cd$Dist == input$districtselect,],
            aes(x = year, y = count)
        ) +
            geom_line()+
            theme_minimal()+
            xlab('Abandoned Properties')+
            ylab('Year')
    })
    
    # Variable handling
    abandoned_rank_dynamic <- reactive({
        abandoned_rank_dynamic <- ranking$abandoned_ranking[ranking$districts_ranking ==input$districtselect]
    })
    streetlight_rank_dynamic <- reactive({
        streetlight_rank_dynamic <- ranking$streetlight_ranking[ranking$districts_ranking ==input$districtselect]
    })
    
    stringX <- reactive({
        if(streetlight_rank_dynamic() == 1){stringX <- 'st'}
        else if (streetlight_rank_dynamic() == 2){stringX <- 'nd'}
        else if (streetlight_rank_dynamic() == 3) {stringX <- 'rd'}
        else {stringX <- 'th'}
    })
    stringY <- reactive({
        if(abandoned_rank_dynamic() == 1){stringY <- 'st'}
        else if (abandoned_rank_dynamic() == 2){stringY <- 'nd'}
        else if (abandoned_rank_dynamic() == 3) {stringY <- 'rd'}
        else {stringY <- 'th'}
    })
    
    # Print out the ranking 
    output$popheader <- renderText({"District Population:"})
    output$population <- renderText({district_summary_cd$Pop[district_summary_cd$Dist == input$districtselect]})
    
    output$abandonedheader <- renderText({"Abandoned Lot Scarcity:"})
    output$abandonedrank <- renderText({paste(abandoned_rank_dynamic(),stringY(), sep = '')})
    output$abandoned <- renderText({paste("There are ",
                                          round(district_summary_cd$abandoned_ph[district_summary_cd$Dist ==input$districtselect],2),
                                          "abandoned properties per household in this district")})
    
    output$streetlightheader <- renderText({"Streetlights Abundance:"})
    output$streetlight <- renderText({paste("There are ",
                                            round(district_summary_cd$Streetlights_pa[district_summary_cd$Dist == input$districtselect],2),
                                            "streetlights per square mile in this district")})
    output$streetlightrank <- renderText({paste(streetlight_rank_dynamic(),stringX(), sep = '')})
    
    
    output$table_COX <- renderDT({
        
        datafiltered_COX <- data_COX %>% filter(monthLabel == input$monthselected)
        
        if(input$casetypeselected!="(ALL)")
            datafiltered_COX <- datafiltered_COX %>% 
                filter(Case_Type_Code_Description == input$casetypeselected)
        
        if(input$casetypeselected=="(ALL)")
            datafiltered_COX <- datafiltered_COX %>%
                group_by(month, monthLabel, Council_Me) %>%
                summarize(calls = sum(calls))
        
        datafiltered_COX 
    } ) 
    
    output$timetrend_COX <- renderDygraph({
        dataxts_COX <- NULL
        
        datafiltered_COX <- data_COX 
        if(input$casetypeselected!="(ALL)") 
            datafiltered_COX <- datafiltered_COX %>% 
            filter(Case_Type_Code_Description == input$casetypeselected)
        if(input$casetypeselected=="(ALL)")
            datafiltered_COX <- datafiltered_COX %>%
            group_by(month, monthLabel, Council_Me) %>%
            summarize(calls = sum(calls))
        
        councilMembers_COX <- unique(datafiltered_COX$Council_Me)
        
        for (l in 1:length(councilMembers_COX)) {
            dataCouncil_Me_COX <- datafiltered_COX[datafiltered_COX$Council_Me == councilMembers_COX[l], ]
            dd_COX <- xts(dataCouncil_Me_COX[, "calls"],dataCouncil_Me_COX$month)
            dataxts_COX <- cbind(dataxts_COX, dd_COX)
        }
        
        colnames(dataxts_COX) <- councilMembers_COX
        
        dygraph(dataxts_COX) %>%
            dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1_COX
        
        d1_COX$x$css <- "
    .dygraph-legend > span {display:none;}
    .dygraph-legend > span.highlight { display: inline; }
    "
        d1_COX
    })
    
    output$map_COX <- renderLeaflet({
        
        datafiltered_COX <- data_COX %>% filter(monthLabel == input$monthselected)
        
        if(input$casetypeselected!="(ALL)")
            datafiltered_COX <- datafiltered_COX %>% 
                filter(Case_Type_Code_Description == input$casetypeselected)
        
        if(input$casetypeselected=="(ALL)")
            datafiltered_COX <- datafiltered_COX %>%
                group_by(month, monthLabel, Council_Me) %>%
                summarize(calls = sum(calls))
        
        ordermembers_COX <- match(map_COX@data$Council_Me, datafiltered_COX$Council_Me)
        map_COX@data <- datafiltered_COX[ordermembers_COX, ]
        
        # Create variableplot
        # ADD this to create variableplot
        map_COX$variableplot <- 
            as.numeric(unlist(map_COX@data[, "calls"][1]))
        
        # Create leaflet
        # CHANGE map$Council_Me by map$variableplot
        pal_COX <- colorBin("YlOrRd", domain = map_COX$variableplot, bins = 6)
        
        # CHANGE map$Council_Me by map$variableplot
        labels_COX <- sprintf("%s: %g", map_COX$Council_Me, map_COX$variableplot) %>%
            lapply(htmltools::HTML)
        
        # CHANGE calls by variableplot
        l_COX <- leaflet(map_COX) %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~ pal_COX(variableplot),
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                label = labels_COX
            ) %>%
            
            # CHANGE cases by variableplot
            leaflet::addLegend(
                pal = pal_COX, values = ~variableplot,
                opacity = 0.7, title = NULL
            )
    })
    
    
    
    output$noCallsSelected_COX <- renderText({
        
        datafiltered_COX <- data_COX %>% filter(monthLabel == input$monthselected)
        
        if(input$casetypeselected!="(ALL)")
            datafiltered_COX <- datafiltered_COX %>% 
                filter(Case_Type_Code_Description == input$casetypeselected)
        
        if(input$casetypeselected=="(ALL)")
            datafiltered_COX <- datafiltered_COX %>%
                group_by(month, monthLabel, Council_Me) %>%
                summarize(calls = sum(calls))
        
        returnMessage_COX <- ""
        if (nrow(datafiltered_COX) == 0) {
            returnMessage_COX <- paste("No calls for Type: '", input$casetypeselected, 
                                       "' and Month: ",input$monthselected, sep="")
        }
        
        returnMessage_COX
    })
}

# Run the application
shinyApp(ui = ui, server = server)
