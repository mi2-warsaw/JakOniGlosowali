library(ggplot2)
library(stringi)
library(lubridate)

load("all_statements.rda")

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

