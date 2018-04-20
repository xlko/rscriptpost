rm(list=ls())
library(DT)
setwd('/home/alex/Downloads/')
# schulen.nrw <- read.table('https://www.schulministerium.nrw.de/docs/bp/Ministerium/Open_MSB/Open_Data/FAQ-Eckdaten/OpenData_Eckdaten.csv'
#                   , header=TRUE
#                   , sep=';'
#                   , stringsAsFactors=FALSE
#                   , dec=','
#                   , fileEncoding= 'WINDOWS-1252')


schulen.nrw <- read.table('./OpenData_Eckdaten.csv'
                          , header=TRUE
                          , sep=';'
                          , stringsAsFactors=FALSE
                          , dec=','
                          , fileEncoding= 'WINDOWS-1252')


str(schulen.nrw)
#datatable(schulen.nrw)

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
summary(schulen.nrw$betreuung)
library(officer)
library(rvg)
## Auf einen Lehrkraft entfällt durchschnittlich 13 Schüler
## Ein blick auf die Verteilung visualisiert am besten die Streuung
histo <- function(x){hist(x
                          , col='#000000'
                          , border='white'
                          , freq=FALSE
                          , ylim=c(0,.3)
                          , xlab=NULL
                          , main=NULL
                          )
                      lines(density(x),
                            col='darkgrey')
                        }
histo(schulen.nrw$betreuung)


my_pres <- read_pptx()
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with_text(x = my_pres, type = "title", str = "Un title")
my_pres <- ph_with_vg(my_pres
                       , code = histo(schulen.nrw$betreuung)
                       , type = "body")
print(my_pres, target='./vg.pptx')

names(schulen.nrw)
schulen.nrw.schulform <- split(schulen.nrw, schulen.nrw$schulform_text)



hist(schulen.nrw$betreuung, col='#000000', border='white', freq=FALSE, ylim=c(0,.15))
lines(density(schulen.nrw$betreuung),  col='darkgrey')
View(schulen.nrw)
table(schulen.nrw$kreis_text)

schulen.nrw <- schulen.nrw[order(schulen.nrw$kreis
                                 , schulen.nrw$jahr
                                 , schulen.nrw$schueler_innen
                                 ), ]

## Datensatz wird in disjunkte Partitionen (Listen) nach Kreis getrennt
schulen.nrw.list <- split(schulen.nrw, schulen.nrw$kreis_text)
##

### Dateien
doc <- read_pptx()
### Schwundquoten nach Semester von Teilnehmern und nicht Teilnehmern EA
for(i in names(schulen.nrw.schulform) ){
    doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
    doc <- ph_with_text(doc,  paste(i, 'Lehrkräfte pro SuS in NRW', sep='-'))
    doc <- ph_with_vg(doc
               , code = histo(schulen.nrw.schulform[[i]]$betreuung)
               , type = "body")
    
}
print(doc, './AnzahlSchüler.pptx' )
