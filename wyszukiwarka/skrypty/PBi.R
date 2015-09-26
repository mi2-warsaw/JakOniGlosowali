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



getSpeakerCounts <- function(word, N = 20) {
  # all_statements jest global
  positions <- stri_detect_regex(str = all_statements$statement, pattern = word)
  
  pl <- qplot(1,1) + theme_bw()
    
  if (sum(positions)) {
    speakers <- all_statements$surname_name[positions]
    speakers <- head(sort(table(speakers), decreasing = TRUE), N)
    df <- data.frame(name = names(speakers), counts = as.numeric(speakers))
    
    df$name <- reorder(df$name, df$counts, mean)
    
    pl <- ggplot(df, aes(x=name, y=counts)) +
      geom_bar(stat="identity") +
      coord_flip() + ggtitle(paste("Pattern:", word, "\n\n")) + 
      theme_bw() + xlab("") + ylab("")
  }
  pl
}

library(lubridate)

getDateCounts <- function(word) {
  # all_statements jest global
  positions <- stri_detect_regex(str = all_statements$statement, pattern = word)
  
  pl <- qplot(1,1) + theme_bw()
  
  if (sum(positions)) {
    dates <- all_statements$date_statement[positions]
    dates <- table(dates)
    df <- data.frame(name = ymd(names(dates)), counts = as.numeric(dates))
  
    pl <- ggplot(df, aes(x=name, y=counts)) +
      geom_bar(stat="identity") +
      geom_smooth(se=FALSE, span=0.2, color="red3", size=2) + 
      ggtitle(paste("Pattern:", word, "\n\n")) + 
      theme_bw() + xlab("") + ylab("")
  }
  pl
}


getBorders <- function(word, N=100) {
  positions <- stri_detect_regex(str = all_statements$statement, pattern = word)
  wybraneWypowiedzi <- all_statements[positions, ]
  
  allChunks <- stri_extract_all_regex(str = wybraneWypowiedzi$statement, 
                               pattern = paste0(".{0,",N,"}",word,".{0,",N,"}"))
  len <- sapply(allChunks, length)
  len2 <- rep(seq_along(len),len)
  link <- sapply(strsplit(wybraneWypowiedzi$id_statement[len2], split=".", fixed=TRUE),
         function(x) {
           paste0("http://www.sejm.gov.pl/Sejm7.nsf/wypowiedz.xsp?posiedzenie=",
                  x[1],"&dzien=",x[2],"&wyp=",x[3])
         })
  data.frame(id = wybraneWypowiedzi$id_statement[len2], 
             link = link, 
             text = unlist(allChunks)[len2])
}




