library(stringi)

load("all_statements.rda")

str(all_statements)

regexp <- "uchodźcy"

all_statements$statement
  

positions <- stri_detect_regex(str = all_statements$statement, pattern = regexp)

stri_match_

pp <- stri_extract_all_regex(str = all_statements$statement, pattern = "(.{3}uchodź.{3})")
sapply(pp, function(x) is.na(all(x)))


pp <- stri_extract_all_regex(str = all_statements$statement, pattern = "[Ss]mole")
table(sapply(pp, function(x) is.na(all(x))))

pp <- stri_extract_all_regex(str = all_statements$statement, pattern = "[Ss]mole")
table(sapply(pp, function(x) all(is.na(x))))

czyWystepuje <- sapply(pp, function(x) all(!is.na(x)))

tabelka <- table(czyWystepuje, all_statements$surname_name)
ktoMowi <- head(sort(tabelka["TRUE",], decreasing = TRUE), 20)
ktoMowi <- data.frame(ile = ktoMowi, kto = names(ktoMowi))

library(ggplot2)
ktoMowi$kto <- reorder(ktoMowi$kto, ktoMowi$ile, mean)
ggplot(ktoMowi, aes(x=kto, y=ile)) +
  geom_bar(stat="identity") +
  coord_flip() + ggtitle("Smoleńsk")



all_statements$statement[czyWystepuje]

all_statements$date_statement[czyWystepuje]


library(lubridate)
daty <- table(all_statements$date_statement[czyWystepuje])
df <- data.frame(data=ymd(names(daty)), liczba = as.numeric(daty))

ggplot(df, aes(x=data, y=liczba)) +
  geom_bar(stat="identity") +
  geom_smooth(span=0.2, se=FALSE) + ggtitle("Smoleńsk")



wybraneWypowiedzi <- all_statements$statement[czyWystepuje]


# linki do wypowiedzi
# http://www.sejm.gov.pl/Sejm7.nsf/wypowiedz.xsp?posiedzenie=10&dzien=1&wyp=056


pp <- stri_extract_all_regex(str = wybraneWypowiedzi, pattern = "(.{0,100}[Ss]moleń.{0,100})")
unlist(pp)

sapply(pp, function(x) is.na(all(x)))





smoleńsk
lasy państwowe



getSpeakerCounts <- function(word) {
  # all_statements jest global
  positions <- stri_detect_regex(str = all_statements$statement, pattern = word)
  
  if (sum(positions)) {
    speakers <- all_statements$surname_name[positions]
    
  }
  
  pp <- stri_extract_all_regex(str = all_statements$statement, pattern = "[Ss]mole")
  table(sapply(pp, function(x) is.na(all(x))))
  
}



