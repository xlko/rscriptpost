rm(list=ls())
## Datenbezug für geojson files: http://opendatalab.de/projects/geojson-utilities/
setwd('/home/alex/Downloads/')
library(leaflet)
library(geojsonio)
#nrw <- geojson_read("./gemeinden_simplify200.geojson", what = "sp")
nrw <- geojson_read("./landkreise_simplify200.geojson", what = "sp")
nrw@data <- nrw@data[order(nrw@data$AGS), ]
nrw@data$stich <- rpois(nrow(nrw@data), lambda=1)
class(nrw)
names(nrw)

pal <- colorBin("YlOrRd", domain = nrw$stich, bins=length(unique(nrw$stich)))

m <- leaflet(nrw) %>%
  setView( 7.61,51.5,   zoom=0) %>%
  fitBounds( 5.5, 50.26, 9.51, 52.55) %>%
  addTiles()
m %>% addPolygons()


m %>% addPolygons(
  fillColor = ~pal(stich),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)


m %>% addPolygons(
  fillColor = ~pal(stich),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE))
nrw@data <- nrw@data[order(as.numeric(rownames(nrw@data))), ]

labels <- sprintf(
  "<strong>%s</strong><br/>%d Possion Sample",
  nrw$GEN, nrw$stich
) %>% lapply(htmltools::HTML)


m <- m %>% addPolygons(
  fillColor = ~pal(stich),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>%
    addLegend(pal = pal, values = ~stich, opacity = 0.7, title = NULL,
                position = "bottomright")

m
