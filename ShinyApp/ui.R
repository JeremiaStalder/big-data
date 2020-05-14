###############################################################################
# Entrypoint for the shiny app
#
# Author: Fabian Karst
# Created 2020-05-20
###############################################################################

ui = shiny::htmlTemplate(
  # Index Page
  "www/index2.html",
  
  # home page
  ## title
  page_title = textOutput(
    "Test title",
    inline = T
  ),
  
  ## subtitle
  page_subtitle = textOutput(
    "page_subtitle_text",
    inline = T
  )
)