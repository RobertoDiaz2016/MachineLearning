library(leaflet)
library(stringr)
library(geosphere)



skyScraperIcon <- makeIcon(
        iconUrl = c("skyscraper-icon-png-4.png"),
        iconWidth = 62*200/230, iconHeight = 62,
        iconAnchorX = 62*200/230/2, iconAnchorY = 62/2.1
)

skyScraperSites <- c(
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/auditorium-building/'>Auditorium Building<br>430 S. Michigan Ave.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/carbide-and-carbon-building/'>Carbide and Carbon Building<br>230 N. Michigan Ave.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/chicago-board-of-trade-building/'>Chicago Board of Trade Building<br>141 W. Jackson Blvd.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/fine-arts-building/'>Fine Arts Building<br>410 S. Michigan Ave.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/manhattan-building/'>Manhattan Building<br>431 S. Dearborn St.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/marquette-building/'>Marquette Building<br>56 W. Adams St.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/monadnock-building/'>Monadnock Building<br>53 W. Jackson Blvd.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/railway-exchange-building/'>Railway Exchange Building<br>224 S. Michigan Ave.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/reliance-building/'>Reliance Building<br>32 N. State St.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/sullivan-center/'>Sullivan Center<br>1 S. State St.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/the-rookery/'>The Rookery<br>209 S. LaSalle St.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/tribune-tower/'>Tribune Tower<br>435 N. Michigan Ave.</a>",
        "<a href='http://www.architecture.org/architecture-chicago/buildings-of-chicago/building/wrigley-building/'>Wrigley Building<br>400-410 N. Michigan Ave.</a>"
)


skyScraperNames <- str_replace_all(str_extract(skyScraperSites,">.*<br>"),"[<]|[>]|(br>)","")

# print(skyScraperNames)

# circleColors <- c("black","red","green","blue","cyan","magenta","yellow","gray", "purple","orange","violet","royalblue","tan")
circleColors <- c("chartreuse","magenta","blue","orange","darkorchid","seagreen","yellow","rosybrown","purple","saddlebrown","salmon","royalblue","orange")
# binpal <- colorBin("Set2", 1:13,13,pretty = FALSE)
# factpal <- colorFactor(topo.colors(13), levels=skyScraperNames)



historicSkyScapersdf <- data.frame(latitude = c(41.875996,41.886612,41.877823,41.876468,41.876073,41.879620,41.877979,41.878599,41.883034,41.881856,41.879090,41.890455,41.889451),longitude=c(-87.624855,-87.624994,-87.632380,-87.624838,-87.628924,-87.629726,-87.629566,-87.624891,-87.628226,-87.627410,-87.631857,-87.623100,-87.624891),skyScraperNames= skyScraperNames)


# distanceNames <- rbind(c("meters",1),c("feet",3),c("miles",3/5280))
distanceNames <- data.frame(cbind(c(1,3.280839895,0.000621371)))

# colnames(distanceNames) <- c("unit","mulplicationFactor")
colnames(distanceNames) <- c("mulplicationFactor")
rownames(distanceNames) <- c("meters","feet","miles")

#add for units and multifactors


perimeterDistance <- function(p1,p2){
        # print(p1)
        # print(p2)
        c(
                perimeter(rbind(c(p1),c(p2[,1],p1[,2]))), 
                perimeter(rbind(c(p2[,1],p1[,2]),c(p2))),
                perimeter(rbind(c(p1),c(p2[,1],p1[,2]))) 
                + perimeter(rbind(c(p2[,1],p1[,2]),c(p2))),
                c(p1[,1],p1[,2]),
                c(p2[,1],p1[,2]),
                c(p2[,1],p2[,2])
        )
}

# perimeterDistance(historicSkyScapersdf[1,2:1],historicSkyScapersdf[2,2:1])



n <- nrow(historicSkyScapersdf)
m <- nrow(historicSkyScapersdf)

# distances = matrix(ncol = m, nrow = n)
distances <- data.frame(matrix(ncol = m, nrow = n))
        
for (i in 1:n) {
        for (j in 1:m) {
                distances[i,j] <- list(list(perimeterDistance(historicSkyScapersdf[i,2:1],historicSkyScapersdf[j,2:1])))
        }
}

colnames(distances) <- skyScraperNames
rownames(distances) <- skyScraperNames

