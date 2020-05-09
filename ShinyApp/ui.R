###############################################################################
# Entrypoint for the shiny app
#
# Author: Fabian Karst
# Created 2020-05-20
###############################################################################

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  # home page
  ## title
  page_title = textOutput(
    "page_title_text",
    inline = T
  ),
  
  ## subtitle
  page_subtitle = textOutput(
    "page_subtitle_text",
    inline = T
  ),
  
  
  
  
  
  # Number of trips
  number_of_trips = textOutput(
    "num_trips",
    inline = T
  ),
  
  # Number of hours
  num_hours_text = textOutput(
    "num_hours",
    inline = T
  ),
  
  # Longest Trip
  longest_trip_time = textOutput(
    "longest_trip_time_text",
    inline = T
  ),
  
  # Number of Kms
  num_distance_text = textOutput(
    "num_distance",
    inline = T
  ),
  
  longest_trip_distance = textOutput(
    "longest_trip_distance_text",
    inline = T
  ),
  
  # Expensive Trip
  
  num_dollars_spent = textOutput(
    "num_distance",
    inline = T
  ),
  
  expensive_trip = textOutput(
    "most_expensive_trip_text",
    inline = T
  ),
  
  
  # Selector for Time
  time_selector = material_card(
    title = "",
    sliderInput(
      "time", 
      "Date",
      min(d_routes$request_time) %>% as.Date(), 
      max(d_routes$request_time) %>% as.Date(),
      value = max(d_routes$request_time) %>% as.Date(),
      step = 30,
      animate = animationOptions(
        playButton = HTML("<img src='images/icons/play-button.png' height='20' width='20'>"), 
        pauseButton = HTML("<img src='images/icons/pause-button.png' height='20' width='20'>")
      )
    )
  ),
  
  # Leaflet map
  leaflet_map = leafletOutput(outputId = "map") %>% 
    withSpinner(color="#0dc5c1")
)