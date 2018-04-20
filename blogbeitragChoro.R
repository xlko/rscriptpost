library(DT)
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


str(schulen.nrw)
#datatable(schulen.nrw)

## einheitliche namen vergeben
names(schulen.nrw) <- otlower(names(schulen.nrw))
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

