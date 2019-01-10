# Packages ----

library(shiny)
library(tidyverse)
library(soql) 
library(jsonlite)
library(lubridate)
library(leaflet)
library(rgdal)
library(DT)
library(rsconnect)

# Underlying code----

#  Create a function to access and parse data, called get_crime_data().
#  This function will be called within the Shiny server function.
  
  get_crime_data <- function(){
  
# Create URL to access API data
# This uses the soql() package to help construct the URL
  my_url <- soql('https://www.dallasopendata.com/resource/9s22-2qus.json') %>%
    soql_where('zip_code = 75223 OR zip_code = 75214') %>%
    soql_order('date1', desc = TRUE) %>% 
    soql_limit(10000) %>%
    as.character()

# Access the API data
  df01 <- jsonlite::fromJSON(txt = my_url)

# Select columns of interest from the API data
  df02 <- df01 %>% 
    select(incident_num = incidentnum,
           datetime = reporteddate,
           address = incident_address,
           geocoded_column,
           crime_category1 = nibrs_crime_category,
           crime_category2 = nibrs_crime,
           crime_incident = offincident,
           crime_description = mo,
           penalcode) # Used to filter incidents w/out offence

# I cannot find a way to map the geocoded coordinates from the API
# so I convert them into simple X and Y coordinates as follows:
  
# Convert geocoded coordinates into a list column called 'coords'
  df02$coords <- as.list(df02$geocoded_column[[2]])

# Drop the original geocoded column & filter out indcidents without coords
  df03 <- df02 %>% 
    select(-geocoded_column) %>% 
    filter(coords != 'NULL') 

# Convert the coordinate data into seperate lon and lat columns
  df03$lon <- map(df03$coords, 1) %>% unlist()
  df03$lat <- map(df03$coords, 2) %>% unlist()

# Finally, drop the coords columns as it's no longer needed
  df03 <- df03 %>% 
    select(-coords) 

# Create a list of incidents that I don't want to see on the final map
  list_filter <- paste(c('no offense','no violation',
                         'false alarm','warrant','child custody','speeding'), 
                       collapse = "|")

# Filter out cases where one the terms above appears
# Order by datetime, add simplified date column
  df03 <- df03 %>%  
    filter(!str_detect(str_to_lower(penalcode),list_filter)) %>%
    filter(!str_detect(str_to_lower(crime_incident),list_filter)) %>% 
    filter(!str_detect(str_to_lower(crime_description),list_filter)) %>% 
    arrange(desc(datetime)) %>% 
    mutate(date = as.Date(datetime))

# Read in shapefile 
# This is boundaries of local neighborhoods that I drew manually in Google My Maps
# This produced a .kml file which I converted to a .shp file online
  shape1 <- readOGR("shapefiles/myneighborhoods.shp") 

# Identify coordinates that fall within the neighborhoods above
  final_coords <- df03 %>% select(lon,lat)
  coordinates(final_coords) <- ~ lon + lat
  proj4string(final_coords) <- proj4string(shape1)
  # Identifies the coordinates within the shapefile

# Add column that identifies if incident occurred in a relevant neighborhood 
  df04 <- df03 %>% 
    cbind(match_coords) %>% 
    filter(!is.na(neighborhood))
  
# Now I want to obscure some of the address details, to preverse anonymity
  
# Split out street number & name; round street number to block number  
  df05 <- df04 %>% 
    extract(address,
            c("block_num", "str_name"), 
            regex = "([[:digit:]-]+)\\s(.*)",
            remove = FALSE) %>% 
    # Round down to give block number
    mutate(block_num = floor(as.numeric(block_num)/100)*100) %>% 
    # Remove any symbols and numbers left in street name
    mutate(str_name = str_replace_all(str_name, "[^[:alpha:]]", " "))
 
# Randomised coordinates
  set.seed(1) 
  row_count <- nrow(df05)
  df06 <- df05 %>%
    mutate(lon_random = lon + 0.000275 * runif(row_count,-1,1) *1.05) %>% 
    mutate(lat_random = lat + 0.000325 * runif(row_count,-1,1) *1.05)
  
  }
  
# Function ends

# User interface ----

ui <- fluidPage(
   
   # Application title
   titlePanel("Reported crime incidents"),

   # Sidebar layout
   fluidRow(
      
     column(3,
        checkboxGroupInput(inputId = "neighborhoods", 
                           label = "Show neighborhoods:", 
                           choices = c("Lakewood",
                                       "Lakewood Hills",
                                       "Hollywood Santa Monica"),
                           selected = "Hollywood Santa Monica"
                           ),
        hr(),
        selectInput(inputId = "numbermonths",
                    label = "Over past number of months:",
                    choices = c('1','3','6','12','24','36'),
                    selected = '1'
                    ),
        hr(),

        p("Map locations are ",span(strong("approximate")),"only 
          (accurate to within around 150 ft). Exact locations have been
          randomized to protect anonymity. Clicking on the table below 
          will highlight the approximate location of each incident.")
      ),
      
      column(9,
             leafletOutput("mymap")
             )
     
   ),
   hr(),
   fluidRow(
         column(12,dataTableOutput("mytable"))
      )
   
)

# Server ----

server <- function(input, output) {
  
    # Run the function that accesses the crime data
    crime_data <-  get_crime_data()
    
    # Get the latest incident date
    date_latest <- as.Date(max(crime_data$date))
    
    date_earliest <- reactive(
      date_latest %m-% months(as.numeric(input$numbermonths))
      )
    
    # We only show the first row for any incident, to avoid duplicates
    filtered_data <- reactive(
        crime_data %>%
        distinct(incident_num, .keep_all = TRUE) %>% 
        filter(neighborhood %in% input$neighborhoods) %>% 
        filter(date > date_earliest())
        )

    output$mymap <- renderLeaflet({
      # Note that we only plot distinct incident numbers, but show
      # all incidents in table
      r <- input$mytable_rows_selected
      
      leaflet(data = filtered_data()) %>% 
        addTiles() %>% 
        addCircleMarkers(~lon_random, ~lat_random, radius = 8, 
          popup = paste(
            "<b>Date:</b>", filtered_data()$date, "<br>",
            "<b>Incident no.:</b>", filtered_data()$incident_num,"<br>",
            "<b>Street:</b>", filtered_data()$str, "<br>",
            "<b>Block:</b>", filtered_data()$block, "<br>",
            "<b>Category1:</b>", filtered_data()$crime_category1, "<br>",
            "<b>Category2:</b>", filtered_data()$crime_category2, "<br>",
            "<b>Description:</b>", filtered_data()$crime_description)) %>% 
        addCircleMarkers(
                   filtered_data()[r,]$lon_random,
                   filtered_data()[r,]$lat_random,
                   color = 'red',
                   fillColor = 'none',
                   opacity = .9)
        
    })
      
    table_data <- reactive(
      filtered_data() %>%
      select(Reported = date,
             #address,
             Incident = incident_num,
             #Neighborhood = neighborhood,
             Street = str_name,
             Block = block_num,
             Category1 = crime_category1,
             Category2 = crime_category2,
             Description = crime_description)
      )
    
    output$mytable  <- DT::renderDataTable({
      DT::datatable(data = table_data(), rownames = FALSE)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

