rm(list=ls())
library(leaflet)
library(geojsonio)

setwd('/home/alex/Downloads/')
schulen.nrw <- read.table('https://www.schulministerium.nrw.de/docs/bp/Ministerium/Open_MSB/Open_Data/FAQ-Eckdaten/OpenData_Eckdaten.csv'
                          , header=TRUE
                          , sep=';'
                          , stringsAsFactors=FALSE
                          , dec=','
                          , fileEncoding= 'WINDOWS-1252')

# schulen.nrw <- read.table('./OpenData_Eckdaten.csv'
#                           , header=TRUE
#                           , sep=';'
#                           , stringsAsFactors=FALSE
#                           , dec=','
#                           , fileEncoding= 'WINDOWS-1252')

setwd('/home/alex/Downloads/')
#nrw <- geojson_read("./gemeinden_simplify200.geojson", what = "sp")
nrw <- geojson_read("./landkreise_simplify200.geojson", what = "sp")
m <- leaflet(nrw) 
m <-  setView(m, 7.61,51.5,   zoom=0) 
m <-  fitBounds(m, 5.5, 50.26, 9.51, 52.55)
m <-  addTiles(m)
m <-  addPolygons(m)
m


## einheitliche namen vergeben
names(schulen.nrw) <- tolower(names(schulen.nrw))
class(schulen.nrw)
sapply(schulen.nrw, class)
## entfernen von unnötigen leerstellen und Umbrüchen in der textvariablen
schulen.nrw.text <- sapply(schulen.nrw,  function(x){
  x <- x[is.character(x)]
  x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",perl=TRUE, x)
  return(x)
})

schulen.nrw.text <- do.call('cbind', schulen.nrw.text)
schulen.nrw <- schulen.nrw[!(names(schulen.nrw) %in% colnames(schulen.nrw.text))]
schulen.nrw <- as.data.frame(cbind(schulen.nrw, schulen.nrw.text))
names(schulen.nrw)
#datatable(schulen.nrw[1:10,])

## aussortieren von privaten schulen
table(schulen.nrw$rechtsform_text, useNA='always')
prop.table(tapply(schulen.nrw$schueler_innen,
                  schulen.nrw$rechtsform_text,
                  sum))
schulen.nrw <- schulen.nrw[!(schulen.nrw$rechtsform_text %in% 'privat'),]

names(schulen.nrw)
schulen.num <- sapply(schulen.nrw, is.numeric)
schulen.num <- schulen.nrw[schulen.num %in% TRUE]
# zummenfassung der numerischen variablen
apply(schulen.num,2, summary) 
## Zwei Dinge
## 1:

schulen.nrw$betreuung <- schulen.nrw$schueler_innen/schulen.nrw$lehrkraefte
summary(schulen.nrw$betreuung )
## Anscheinend existieren Schulen in NRW mit 0 Lehrkräften
## Wie viele Schüler*innen fallen unter diese Schulen
sum(schulen.nrw[schulen.nrw$lehrkraefte==0,]$schueler_innen)/sum(schulen.nrw$schueler_innen)
## Okay dies scheint der absolute Ausnahmefall,
## was auf einen Fehler in der Datenerfassung hindeutet
## diese Schulen können bedenklos aus der Betrachtung entfernt werden.
schulen.nrw <- schulen.nrw[which(schulen.nrw$lehrkraefte > 0),]
schulen.nrw$betreuung <- schulen.nrw$schueler_innen/schulen.nrw$lehrkraefte


unique(schulen.nrw$kreis_text)

schulen.nrw$kreis_match <- sub('(^Krfr. Stadt\\s|^Kreis\\s)', '', schulen.nrw$kreis_text)
unique(schulen.nrw$kreis_match)
table(nrw$GEN %in% schulen.nrw$kreis_match)
schulen.nrw$betreuung_kreis <- ave(schulen.nrw$betreuung,
                                   schulen.nrw$kreis_match,
                                   FUN = function(x){mean(x, na.rm=TRUE)})
schulen.match <- schulen.nrw[names(schulen.nrw) %in% c('kreis_match', 'betreuung_kreis')]
schulen.match <- schulen.match[!duplicated(schulen.match$kreis_match), ]

schulen.match <- schulen.match[order(schulen.match$kreis_match), ]
nrw@data <- nrw@data[order(nrw@data$GEN), ]
stopifnot(nrw@data$GEN == schulen.match$kreis_match)

nrw@data$betreuung <- schulen.match$betreuung_kreis
nrw@data <- nrw@data[order(as.numeric(rownames(nrw@data))), ]

#nrw@data$betreuung.bin <- cut(nrw@data$betreuung,5)


labels <- sprintf(
  "<strong>%s</strong><br/>%g Lehrkraft pro SuS",
  nrw$GEN, nrw$betreuung
)

labels <- lapply(labels, htmltools::HTML)
pal <- colorBin("YlOrRd", domain = nrw$betreuung, bins=4)


m <- addPolygons(m,
  fillColor = ~pal(nrw$betreuung),
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
    direction = "auto")) 
m <-  addLegend(m, pal = pal, values = ~nrw$betreuung, opacity = 0.7, title = NULL,
            position = "bottomright")

m

