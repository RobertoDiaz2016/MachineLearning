#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)


# Define UI for application that draws a leaflet map
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Chicago Historic Buildings"),
        
        # Sidebar with a checkboxInput input for selecting building locations 
        sidebarLayout(
                sidebarPanel(
                        checkboxGroupInput("buildings",
                                           "Select two building names",
                                           skyScraperNames),
                        # Zoom locked message
                        htmlOutput("zoomLocked"),
                        fluidRow(
                                column(width=12,selectInput("units", "Distance Units:",
                                                            rownames(distanceNames))),
                                # Table of total distances
                                column("Total",width=4,tableOutput("tableTotalDistance")),
                                # Table of horizontal distances
                                column("Horizontal",width=4,tableOutput("tableHorizontalDistance")),
                                # Table verical distances
                                column("Vertical",width=4,tableOutput("tableVericalDistance"))
                        ),
                        # Source and destination text
                        htmlOutput("buildingNames"),
                        
                        width = 3
                ),
                
                mainPanel(
                        # Show a plot of the generated distribution
                        leafletOutput("buildingMap",width = "100%", height = 700),
                        width = 9
                ),
                fluid = FALSE
        )
))
