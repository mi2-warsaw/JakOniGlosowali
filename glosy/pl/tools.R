library(cluster)
library(ggdendro)
library(ape)
library(RColorBrewer)
library(dplyr)
library(tidyr)

kol <- brewer.pal(9,"Set1")
kol2 <- c(PO = "orange3", PiS = "blue4", RP = "gold3", PSL="green4", SLD="red3", SP="blue1",
          `niez.` = "grey", ID="gold4", TR="gold3", KPSP="blue2", BiG="orange2",
          ZP="blue2", BC ="blue2" )
scores <- c(`Nieobecny` = 0, `Przeciw` = -2, `Wstrzymał się` = -1, `Za` = 2)


load("all_votes.rda")

getSpeakerDendro <- function(wzor) {
  cat(wzor)
  typ <- wzor[1]
  wzor <- wzor[-1]
  
  if (length(wzor) == 0 || wzor[1] == "") {
    tytulyGlos <- unique(all_votes$topic_voting)
  } else {
    tytulyGlos <- unique(unlist(sapply(wzor, function(w) {
      grep(unique(all_votes$topic_voting), pattern = w, value = TRUE, fixed = TRUE)
    })))
  }

  selVotes <- all_votes %>%
    filter(topic_voting %in% tytulyGlos)
  
  tabi <- table(selVotes$surname_name, selVotes$club)
  clubs <- apply(tabi, 1, function(x) paste(colnames(tabi)[x>0], collapse=","))
  clubs2 <- apply(tabi, 1, function(x) paste(colnames(tabi)[which.max(x)], collapse=","))
  
  selVotes$vote <- scores[as.character(selVotes$vote)]
  
  tVotes <- spread(selVotes[,c(1,3,4)], key = id_voting, value = vote)
  rownames(tVotes) <- paste(" ", tVotes[,1], " - ", clubs[as.character(tVotes[,1])], " ",sep="")
  tVotes <- tVotes[,-1]
  
  # tylko Ci w sejmie na ponad 90% glosowan
  cVotes <- clubs2[rowMeans(is.na(tVotes)) < 0.1]
  tVotes <- tVotes[rowMeans(is.na(tVotes)) < 0.1,]
  
  dVotes <- dist(tVotes)
  
  ag <- agnes(dVotes, method = "average")
  hc = as.hclust(ag)
  
  par(mar=c(1,1,2,1), xpd=NA, font=2, family="mono")
  
  plot(as.phylo(hc), type = typ, cex = 0.85,
       tip.color = kol2[cVotes],
       main=paste(paste(wzor, collapse = "\n"), "(głosowań:",length(unique(selVotes$id_voting)),")"),
       rotate.tree=-85)
}



getSpeakerDendro2 <- function(wzor) {
  typ <- wzor[1]
  wzor <- wzor[-1]
  cat(wzor)
  
  if (length(wzor) == 0 || wzor[1] == "") {
    tytulyGlos <- unique(all_votes$topic_voting)
  } else {
    tytulyGlos <- unique(unlist(sapply(wzor, function(w) {
      grep(unique(all_votes$topic_voting), pattern = w, value = TRUE, fixed = TRUE)
    })))
  }
  
  selVotes <- all_votes %>%
    filter(topic_voting %in% tytulyGlos)
  
  tabi2 <- table(selVotes$club,selVotes$vote)[,c(4,1,3,2)]
  par(mar=c(1,1,2,1), xpd=NA)
  mosaicplot(tabi2, off = c(0,0), border="white", 
             color=c("green3", "grey", "red4", "red2"), las=2,
             main="")
}
