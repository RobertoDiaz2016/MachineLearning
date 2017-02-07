#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)



# Define server logic required to draw a Leaflet map
shinyServer(function(input, output, session) {
        
        output$buildingMap <- renderLeaflet({
                historicSkyScapersdf %>%
                        leaflet() %>% 
                        addTiles() %>%
                        addMarkers(icon = skyScraperIcon,popup = skyScraperSites,clusterOptions = markerClusterOptions()) %>%
                        addCircleMarkers(color = circleColors,popup = skyScraperSites,clusterOptions = markerClusterOptions())  %>%
                        addLegend(position = 'bottomright',labels = skyScraperNames, colors = circleColors)
                
        })
        
        
        my_min <- 1
        my_max <- 2
        lastBuildingsClicked <- ""
        
        observe({

                # print(paste("Clicked",paste(input$buildings),length(input$buildings)))
                
                if(length(input$buildings)==2){
                        # print(paste("lastBuildingsClicked",input$buildings[1],input$buildings[2]))
                        lastBuildingsClicked <<- input$buildings
                }
                
                if(length(input$buildings) > my_max)
                {
                        # print(paste("Clicked my max",input$buildings[1],input$buildings[2],input$buildings[3],length(input$buildings),lastBuildingsClicked))
                        
                        newBuildingSelected <- setdiff(input$buildings,lastBuildingsClicked)
                        index <- which(input$buildings %in% newBuildingSelected)
                        # print(paste(index,newBuildingSelected))
                        if(index == 1){
                                first <- 1;last <- 3
                        }
                        if(index == 2){
                                first <- 2;last <- 3
                        }
                        if(index == 3){
                                first <- 1;last <- 3
                        }
                        
                        updateCheckboxGroupInput(session, "buildings", selected= c(input$buildings[first],input$buildings[last]))
                }
                
                if(length(input$buildings) < my_min)
                {
                        updateCheckboxGroupInput(session, "buildings", selected= head(input$buildings,my_max))
                }
                
                
                
        # })
        
        # observe({
                
                # print(paste("renderTable",paste(input$buildings[1],input$buildings[2]),length(input$buildings),length(lastBuildingsClicked)))
                if(length(input$buildings) == 0){
                        output$tableTotalDistance <- renderTable({
                                distances[1,1][[1]][3]*distanceNames[input$units,] 
                        } , rownames = FALSE , colnames = FALSE
                        )
                        output$tableHorizontalDistance <- renderTable({
                                distances[1,1][[1]][1]*distanceNames[input$units,] 
                        } , rownames = FALSE , colnames = FALSE
                        )
                        output$tableVericalDistance <- renderTable({
                                distances[1,1][[1]][2]*distanceNames[input$units,] 
                        } , rownames = FALSE , colnames = FALSE
                        )
                        
                        leafletProxy("buildingMap") %>%
                                clearControls() %>%
                                removeControl("path") %>%
                                clearShapes() %>%
                                addLegend(position = 'bottomright',labels = skyScraperNames, colors = circleColors)
                }
        # })
        
        # observe({
                if(length(input$buildings)==0){
                        first <- 1 ; last <- 1
                }
                if(length(input$buildings)==1){
                        first <- input$buildings[1] ; last <- input$buildings[1]
                        isolate({
                                leafletProxy("buildingMap") %>%
                                        clearControls() %>%
                                        removeControl("path") %>%
                                        clearShapes() %>%
                                        addLegend(position = 'bottomright',labels = skyScraperNames, colors = circleColors)
                        })
                        
                }
                if(length(input$buildings)==2){
                        index <- 1; index2 <- 2
                        first <- input$buildings[index] ; last <- input$buildings[index2]
                        isolate({
                                # leafletProxy("buildingMap",session) 
                                proxy <- leafletProxy("buildingMap",session)
                                
                                proxy %>% clearControls() %>%
                                        addPolylines(lng=distances[input$buildings[1],input$buildings[2]][[1]][c(4,6,8)] , lat=distances[input$buildings[1],input$buildings[2]][[1]][c(5,7,9)] , color="red" , layerId = "path") %>%
                                        setView(lng=mean(distances[input$buildings[1],input$buildings[2]][[1]][c(4,8)]),
                                                lat=mean(distances[input$buildings[1],input$buildings[2]][[1]][c(5,9)]),
                                                zoom=input$buildingMap_zoom,
                                                options = list(reset=TRUE,animate=FALSE)
                                        ) %>%
                                        fitBounds(lng1=distances[input$buildings[1],input$buildings[2]][[1]][c(4)],
                                                  lat1=distances[input$buildings[1],input$buildings[2]][[1]][c(5)],
                                                  lng2=distances[input$buildings[1],input$buildings[2]][[1]][c(8)],
                                                  lat2=distances[input$buildings[1],input$buildings[2]][[1]][c(9)]
                                        )
                                input$buildingMap_zoom
                                
                        })
                        
                }
                if(length(input$buildings)==3){
                        index <- 1; index2 <- 2
                        first <- input$buildings[index] ; last <- input$buildings[index2]
                }
                # print(paste("first:",first,last,sep = " ",collapse = " "))
                output$tableTotalDistance <- renderTable({
                        distances[first,last][[1]][3]*distanceNames[input$units,]
                } , rownames = FALSE , colnames = FALSE
                )
                output$tableHorizontalDistance <- renderTable({
                        distances[first,last][[1]][1]*distanceNames[input$units,]
                } , rownames = FALSE , colnames = FALSE
                )
                output$tableVericalDistance <- renderTable({
                        distances[first,last][[1]][2]*distanceNames[input$units,]
                } , rownames = FALSE , colnames = FALSE
                )
                
        # })
        
        # observe({
                

                output$buildingNames <- renderText({ 
                        paste("<b>",paste(input$buildings,sep = " to ",collapse = " to ")," approximate walking distances","<b>",sep = " ",collapse = " ")
                })
                
        # })
        
        # observe({
                
                
        })
        

        
})
