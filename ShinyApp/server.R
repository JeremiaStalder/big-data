###############################################################################
# Defining Server Logic behind App
#
# Author Fabian Karst, Erik Senn, Jeremia Stadler
# Created 2019-01-30 20:32:44
###############################################################################

server <- function(input, output) {
  
  # Basic Numbers Page --------------------------------------------------------------
  output$pg_title <- renderText({"This is a Test Text"})
  
}