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


wzor <- "o ochronie zwierząt"
wzor <- "szkolnict"
wzor <- ""

head(grep(unique(all_votes$topic_voting), pattern = "szkolnict", value = TRUE))

tytulyGlos <- grep(unique(all_votes$topic_voting), pattern = wzor, value = TRUE)

selVotes <- all_votes %>%
  filter(topic_voting %in% tytulyGlos)

tabi2 <- table(selVotes$club,selVotes$vote)[,c(4,1,3,2)]
mosaicplot(tabi2, off = c(0,0), border="white", 
           color=c("green3", "grey", "red4", "red2"), las=2,
           main="")



tabi <- table(selVotes$surname_name, selVotes$club)
clubs <- apply(tabi, 1, function(x) paste(colnames(tabi)[x>0], collapse=","))
clubs2 <- apply(tabi, 1, function(x) paste(colnames(tabi)[which.max(x)], collapse=","))

selVotes$vote <- scores[as.character(selVotes$vote)]

tVotes <- spread(selVotes[,c(1,3,4)], key = id_voting, value = vote)
rownames(tVotes) <- paste(tVotes[,1], " - ", clubs[as.character(tVotes[,1])], sep="")
tVotes <- tVotes[,-1]

# tylko Ci w sejmie na ponad 90% glosowan
cVotes <- clubs2[rowMeans(is.na(tVotes)) < 0.1]
tVotes <- tVotes[rowMeans(is.na(tVotes)) < 0.1,]



dVotes <- dist(tVotes)

ag <- agnes(dVotes, method = "average")
hc = as.hclust(ag)

par(mar=c(1,1,2,1), xpd=NA)

plot(as.phylo(hc), type = "fan", cex = 0.4,
     tip.color = kol2[cVotes],
     main=wzor,
     rotate.tree=-85)

plot(as.phylo(hc), type = "unrooted", cex = 0.4,
     tip.color = kol2[cVotes],
     main=wzor,
     rotate.tree=-85)

plot(as.phylo(hc), type = "radial", cex = 0.4,
     tip.color = kol2[cVotes],
     main=wzor,
     rotate.tree=-85)

plot(as.phylo(hc), type = "phylogram", cex = 0.4,
     tip.color = kol2[cVotes],
     main=wzor,
     rotate.tree=-85)

plot(as.phylo(hc), type = "cladogram", cex = 0.4,
     tip.color = kol2[cVotes],
     main=wzor,
     rotate.tree=-85)


# nazwy ustaw
ustawy <- grep(unique(all_votes$topic_voting), pattern = "ustawy o", value=TRUE)
ustawy2 <- sapply(ustawy, function(x) {
  paste(strsplit(x, split= "ustawy o")[[1]][-1], collapse= "ustawy o")
})
# ustawy2 <- (gsub(ustawy, pattern="^.*ustawy o *", replacement = ""))
ustawy2 <- (gsub(ustawy2, pattern="^ zmianie ustawy - *", replacement = ""))
ustawy2 <- (gsub(ustawy2, pattern="^ zmianie ustawy o *", replacement = ""))
ustawy2 <- (gsub(ustawy2, pattern="^ *", replacement = ""))
ustawy3 <- (gsub(ustawy2, pattern=" *[-,\\(].*$", replacement = ""))


ustawy3 <- names(which(table(ustawy3) > 5))








selPos <- levels(factor(selVotes$surname_name))

tabi <- table(selVotes$surname_name, selVotes$club)
clubs <- apply(tabi, 1, function(x) paste(colnames(tabi)[x>0], collapse=","))

clubs2 <- apply(tabi, 1, function(x) paste(colnames(tabi)[which.max(x)], collapse=","))

distMat <- matrix(NA, length(selPos),  length(selPos))
colnames(distMat) <- paste0(selPos, " (", clubs, ")")
rownames(distMat) <- paste0(selPos, " (", clubs, ")")

recalculateDistMat <- TRUE
if (recalculateDistMat) {
  system.time({
    for (i in 1:(length(selPos)-1)) {
      cat("\n",i," ", selPos[i]," ")
      for (j in i:length(selPos)) {
        selVotesI <- selVotes %>%
          filter(surname_name %in% selPos[i])
        selVotesJ <- selVotes %>%
          filter(surname_name %in% selPos[j])
        
        selIJ <- merge(selVotesI[,c(1,2,3,4)], selVotesJ[,c(1,2,3,4)], by="id_voting")
        
        distMat[i,j] = mean(selIJ$vote.x == selIJ$vote.y)
        distMat[j,i] = mean(selIJ$vote.x == selIJ$vote.y)
        cat(".")
      }
    }
    
  })
  save(distMat, file = "distMat.rda")
} else {
  load(file = "distMat.rda")
}

rem <- which(rowMeans(is.na(distMat)) > 0.01)
distMatR <- distMat[-rem, -rem]
rownames(distMatR) <- selPos[-rem]
colnames(distMatR) <- paste(selPos[-rem], clubs2[-rem])

library(MASS)

space <- isoMDS(as.dist(1.001-distMatR), k=2)
df <- data.frame(space$points, clubs=clubs2[-rem], name=selPos[-rem])

library(ggplot2)

ggplot(df, aes(X1, X2, color=clubs, label=name)) +
  geom_text(size=4) +
  theme_bw() + scale_shape_manual(values=LETTERS)


distMatR["Napieralski Grzegorz",]


library(cluster)

ag <- agnes(as.dist(1.001-t(distMatR)), method = "average")

library(ggdendro)
library(ape)
library(RColorBrewer)

kol <- brewer.pal(9,"Set1")

hc = as.hclust(ag)

par(mar=c(0,0,2,0))

plot(as.phylo(hc), type = "fan", cex = 0.4,
     tip.color = kol[as.numeric(factor(clubs2[-rem]))],
     main=wzor)


