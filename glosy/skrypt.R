library(cluster)
library(ggdendro)
library(ape)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(parallel)

# settings for plot exports
defaultWidth <- 2000
defaultHeight <- 2000
defaultPointSize <- 40

numCores <- detectCores() # get the number of cores available

source("parliament_voting_data.pl.R")

scores <- c(`Absent` = 0, `Against` = -2, `Abstained` = -1, `For` = 2)

votingTopicsThatMatchesPattern <- grep(unique(all_votes$topic_voting), pattern = pattern, value = TRUE)
selectionOfVotes <- all_votes %>%
  filter(topic_voting %in% votingTopicsThatMatchesPattern)

partiesAndTheirVotes <- table(selectionOfVotes$party,selectionOfVotes$vote)[,c("For","Absent","Abstained","Against")]
tt<-with(selectionOfVotes, partiesAndTheirVotes)
partiesAndTheirVotes_sortedByMostFrequent <- tt[order(tt[,2], decreasing=T),]

png("plotA_mosaicplot.png",
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
mosaicplot(partiesAndTheirVotes_sortedByMostFrequent, off = c(0,0), border="white", 
           color=c("green3", "grey", "red4", "red2"), las=2,
           main="")
dev.off()

membersVsPartiesOccurances <- table(selectionOfVotes$voter_id, selectionOfVotes$party)
membersAndAllTheirParties <- apply(membersVsPartiesOccurances, 1, function(x) paste(colnames(membersVsPartiesOccurances)[x>0], collapse=","))
membersAndTheirMostFrequentParty <- apply(membersVsPartiesOccurances, 1, function(x) paste(colnames(membersVsPartiesOccurances)[which.max(x)], collapse=","))

selectionOfVotes$vote <- scores[as.character(selectionOfVotes$vote)]

tVotes_ <- spread(selectionOfVotes[,c(1,3,4)], key = id_voting, value = vote)
rownames(tVotes_) <- paste(tVotes_[,1], " - ", membersAndAllTheirParties[as.character(tVotes_[,1])], sep="")
tVotes <- tVotes_[,-1]

# tylko Ci w sejmie na ponad 90% glosowan = Only those in the Diet on more than 90% of voting
cVotes <- membersAndTheirMostFrequentParty[rowMeans(is.na(tVotes)) < 0.1]
tVotes <- tVotes[rowMeans(is.na(tVotes)) < 0.1,]

dVotes <- dist(tVotes)

ag <- agnes(dVotes, method = "average")
hc = as.hclust(ag)

par(mar=c(1,1,2,1), xpd=NA)
png("plot5_fan.png",
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plot(as.phylo(hc), type = "fan", cex = 0.4,
     tip.color = partyColors[cVotes],
     edge.width = 2,
     no.margin = TRUE,
     main=pattern,
     rotate.tree=-85)
dev.off()

par(mar=c(1,1,2,1), xpd=NA)
png("plot4_unrooted.png",
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plot(as.phylo(hc), type = "unrooted", cex = 0.4,
     tip.color = partyColors[cVotes],
     no.margin = TRUE,
     main=pattern,
     rotate.tree=-85)
dev.off()

par(mar=c(1,1,2,1), xpd=NA)
png("plot5alt_radial.png",
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plot(as.phylo(hc), type = "radial", cex = 0.4,
     tip.color = partyColors[cVotes],
     edge.width = 2,
     no.margin = TRUE,
     main=pattern,
     rotate.tree=-85)
dev.off()

par(mar=c(0,0,0,0), xpd=NA)
png("plot3_phylogram.png",
    width=as.integer(defaultWidth*2.0),
    height=as.integer(defaultHeight*2.0),
    pointsize=15,
)
plot(as.phylo(hc), type = "phylogram", cex = 2.5,
     tip.color = partyColors[cVotes],
     edge.width = 2,
     no.margin = TRUE,
     main=pattern)
dev.off()

par(mar=c(0,0,0,0), xpd=NA)
png("plot3alt_cladogram.png",
    width=as.integer(defaultWidth*2.0),
    height=as.integer(defaultHeight*2.0),
    pointsize=15,
)
plot(as.phylo(hc), type = "cladogram", cex = 2.5,
     tip.color = partyColors[cVotes],
     edge.width = 2,
     no.margin = TRUE,
     main=pattern)
dev.off()

# plots not mentioned in the original article

uniqueVoterIds <- levels(factor(selectionOfVotes$voter_id))

membersVsPartiesOccurances <- table(selectionOfVotes$voter_id, selectionOfVotes$party)
membersAndAllTheirParties <- apply(membersVsPartiesOccurances, 1, function(x) paste(colnames(membersVsPartiesOccurances)[x>0], collapse=","))
membersAndTheirMostFrequentParty <- apply(membersVsPartiesOccurances, 1, function(x) paste(colnames(membersVsPartiesOccurances)[which.max(x)], collapse=","))

distMat <- matrix(NA, length(uniqueVoterIds),  length(uniqueVoterIds))
colnames(distMat) <- paste0(uniqueVoterIds, " (", membersAndAllTheirParties, ")")
rownames(distMat) <- paste0(uniqueVoterIds, " (", membersAndAllTheirParties, ")")

recalculateDistMat <- TRUE
if (recalculateDistMat) {
  system.time({

    iMax <- (length(uniqueVoterIds)-1)
    jMax <- length(uniqueVoterIds)

    message("Pre-calculating lists of personal votes")
    results_selectionOfVotes <- mclapply(1:jMax,
                                 FUN=function(i) {
                                   selectionOfVotesI <- selectionOfVotes %>%
                                     filter(voter_id %in% uniqueVoterIds[i])
                                 },
                                 mc.cores = numCores)
    
    message("Summarizing intersections between member votes")
    for (i in 1:iMax) {
      cat("\n",i," ", uniqueVoterIds[i]," ")
      for (j in i:jMax) {
        selectionOfVotesI <- results_selectionOfVotes[[i]]
        selectionOfVotesJ <- results_selectionOfVotes[[j]]

        selIJ <- merge(selectionOfVotesI[,c(1,2,3,4)], selectionOfVotesJ[,c(1,2,3,4)], by="id_voting")
        
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
rownames(distMatR) <- uniqueVoterIds[-rem]
colnames(distMatR) <- paste(uniqueVoterIds[-rem], membersAndTheirMostFrequentParty[-rem])

library(MASS)

space <- isoMDS(as.dist(1.001-distMatR), k=2)
df <- data.frame(space$points, parties=membersAndTheirMostFrequentParty[-rem], name=uniqueVoterIds[-rem])

library(ggplot2)

# a plot not mentioned in the original article
png("plotB_aes.png",
    width=as.integer(defaultWidth/3),
    height=as.integer(defaultHeight/3),
    pointsize=defaultPointSize,
)
ggplot(df, aes(X1, X2, color=parties, label=name)) +
  geom_text(size=4) +
  theme_bw() + scale_shape_manual(values=LETTERS)
dev.off()

# debug
# distMatR["Napieralski Grzegorz",]


library(cluster)

ag <- agnes(as.dist(1.001-t(distMatR)), method = "average")

library(ggdendro)
library(ape)
library(RColorBrewer)

colors <- brewer.pal(9,"Set1")

hc = as.hclust(ag)

par(mar=c(0,0,2,0))

# another plot not mentioned in the original article
png("plotC_phylo.png",
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plot(as.phylo(hc), type = "fan", cex = 0.4,
     tip.color = colors[as.numeric(factor(membersAndTheirMostFrequentParty[-rem]))],
     main=pattern)
dev.off()

