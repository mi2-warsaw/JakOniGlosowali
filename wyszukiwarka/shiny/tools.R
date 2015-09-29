library(ggplot2)
library(stringi)
library(lubridate)

load("all_statements.rda")
all_statements$statement <- as.character(all_statements$statement)
all_statements$surname_name2 <- gsub(as.character(all_statements$surname_name),
                                     pattern="Sekretarz Poseł ", replacement="")
all_statements$surname_name2 <- gsub(as.character(all_statements$surname_name2),
                                     pattern=" Poseł ", replacement="")
all_statements$surname_name2 <- gsub(as.character(all_statements$surname_name2),
                                     pattern="Poseł ", replacement="")
all_statements$surname_name2 <- gsub(as.character(all_statements$surname_name2),
                                     pattern="Prezes Rady Ministrów ", replacement="")
all_statements$surname_name2 <- gsub(as.character(all_statements$surname_name2),
                                     pattern="Sprawozdawca ", replacement="")
all_statements$surname_name2 <- gsub(as.character(all_statements$surname_name2),
                                     pattern="Sprawozdawca ", replacement="")

getSpeakerCounts <- function(words, N = 20) {
  # all_statements jest global
  word <- words[1]
  word2 <- words[2]
  word <- words[1]
  word2 <- words[2]
  all_statementsSelected <- all_statements
  poslowie <- words[-(1:2)]
  if (length(poslowie)>0) {
    cat(poslowie)
    all_statementsSelected <- all_statementsSelected[all_statementsSelected$surname_name2 %in% poslowie,]
  }
  
  positions <- stri_detect_regex(str = all_statementsSelected$statement, pattern = word)
  if (nchar(word2)>1) {
    positionsNeg <- stri_detect_regex(str = all_statementsSelected$statement, pattern = word2)
    positions <- which(positions & !positionsNeg)
  } else {
    positions <- which(positions)
  }
  
  pl <- qplot(1,1) + theme_bw()
  
  if (length(positions)>1) {
    speakers <- all_statementsSelected$surname_name2[positions]
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

getDateCounts <- function(words) {
  word <- words[1]
  word2 <- words[2]
  # all_statements jest global
  word <- words[1]
  word2 <- words[2]
  all_statementsSelected <- all_statements
  poslowie <- words[-(1:2)]
  if (length(poslowie)>0) {
    all_statementsSelected <- all_statementsSelected[all_statementsSelected$surname_name2 %in% poslowie,]
  }
  
  positions <- stri_detect_regex(str = all_statementsSelected$statement, pattern = word)
  if (nchar(word2)>1) {
    positionsNeg <- stri_detect_regex(str = all_statementsSelected$statement, pattern = word2)
    positions <- which(positions & !positionsNeg)
  } else {
    positions <- which(positions)
  }
  
  pl <- qplot(1,1) + theme_bw()
  
  if (length(positions)>1) {
    dates <- all_statementsSelected$date_statement[positions]
    dates <- table(dates)
    df <- data.frame(name = ymd(names(dates)), counts = as.numeric(dates))
    
    pl <- ggplot(df, aes(x=name, y=counts)) +
      geom_bar(stat="identity") +
      geom_smooth(se=FALSE, span=0.2, color="red3", size=2) + 
      ggtitle(paste("Pattern:", word, "\n\n")) + 
      theme_bw() + xlab("") + ylab("") + 
      coord_cartesian(ylim=c(0,max(df$counts)))
  }
  pl
}


getBorders <- function(words, N=100) {
  word <- words[1]
  word2 <- words[2]
  all_statementsSelected <- all_statements
  poslowie <- words[-(1:2)]
  if (length(poslowie)>0) {
    all_statementsSelected <- all_statementsSelected[all_statementsSelected$surname_name2 %in% poslowie,]
  }
  
  positions <- stri_detect_regex(str = all_statementsSelected$statement, pattern = word)
  if (nchar(word2)>1) {
    positionsNeg <- stri_detect_regex(str = all_statementsSelected$statement, pattern = word2)
    positions <- head(which(positions & !positionsNeg), 500)
  } else {
    positions <- head(which(positions), 500)
  }
  wybraneWypowiedzi <- all_statementsSelected[positions, ]
  
  allChunks <- sapply(1:nrow(wybraneWypowiedzi), function(i) {
    tmp <- stri_locate_all_regex(str = wybraneWypowiedzi$statement[i], pattern = word)[[1]]
    dat1 <- all_statementsSelected$surname_name[positions[i]]
    dat2 <- all_statementsSelected$date_statement[positions[i]]

    tmpD <- paste(sapply(1:nrow(tmp), function(j) {
      
      id <- all_statementsSelected$id_statement[positions[i]]
      x <- strsplit(id, split=".", fixed = TRUE)[[1]]
      
      adres <- paste0("http://www.sejm.gov.pl/Sejm7.nsf/wypowiedz.xsp?posiedzenie=",
             x[1],"&dzien=",x[2],"&wyp=",x[3])

      tmp <- paste0(
        "<small><a href='",adres,"'>... ",
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
      "... </a></small><br/>")
      
      paste(tmp)
    }), collapse="</br>")
    
    paste(dat2, "<i>", dat1, "</i><br/>", tmpD)
  })
  
  HTML(paste(rev(allChunks), collapse = "<hr>"))
}

