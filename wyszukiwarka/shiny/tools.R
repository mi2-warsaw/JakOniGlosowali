library(ggplot2)
library(stringi)
library(lubridate)

load("all_statements.rda")
all_statements$statement <- as.character(all_statements$statement)

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
  positions <- head(which(positions), 500)
  wybraneWypowiedzi <- all_statements[positions, ]
  
  allChunks <- sapply(1:nrow(wybraneWypowiedzi), function(i) {
    tmp <- stri_locate_all_regex(str = wybraneWypowiedzi$statement[i], pattern = word)[[1]]
    paste(sapply(1:nrow(tmp), function(j) {
      
      id <- all_statements$id_statement[positions[i]]
      x <- strsplit(id, split=".", fixed = TRUE)[[1]]
      
      adres <- paste0("http://www.sejm.gov.pl/Sejm7.nsf/wypowiedz.xsp?posiedzenie=",
             x[1],"&dzien=",x[2],"&wyp=",x[3])

      paste0(
        "<a href='",adres,"'>",
        substr(wybraneWypowiedzi$statement[i], 
             max(tmp[j,1] - N, 1),
             tmp[j,1]-1),
      "<b>",
      substr(wybraneWypowiedzi$statement[i], 
             tmp[j,1],
             tmp[j,2]),
      "</b>",
      substr(wybraneWypowiedzi$statement[i], 
             tmp[j,2]+1,
             min(tmp[j,2] + N, nchar(wybraneWypowiedzi$statement[i]))),
      "</a><br/>")
    }), collapse="</br>")
  })
  
  HTML(paste(allChunks, collapse = "<hr>"))
}

