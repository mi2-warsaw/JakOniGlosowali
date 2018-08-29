library(cluster)
library(ggdendro)
library(ape)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(parallel)
library(dendextend)

# country to work with
country <- "pl"

countrySpecificPath <- function(path) {
  paste("./", country, "/", path, sep="")
}

# settings for plot exports
defaultWidth <- 2000
defaultHeight <- 2000
defaultPointSize <- 40

numCores <- detectCores() # get the number of cores available

source(countrySpecificPath("parliament_voting_data.R"))

scores <- c(`Absent` = 0, `Against` = -2, `Abstained` = -1, `For` = 2)

votingTopicsThatMatchesPattern <- grep(unique(all_votes$topic_voting), pattern = pattern, value = TRUE)
selectionOfVotes <- all_votes %>%
  filter(topic_voting %in% votingTopicsThatMatchesPattern)

partiesAndTheirVotes <- table(selectionOfVotes$party,selectionOfVotes$vote)[,c("For","Absent","Abstained","Against")]
tt<-with(selectionOfVotes, partiesAndTheirVotes)
partiesAndTheirVotes_sortedByMostFrequent <- tt[order(tt[,2], decreasing=T),]

png(countrySpecificPath("plotA_mosaicplot.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
mosaicplot(partiesAndTheirVotes_sortedByMostFrequent, off = c(0,0), border="white", 
           color=c("green3", "grey", "red4", "red2"), las=2,
           main="")
dev.off()

voterIdsVsPartiesOccurances <- table(selectionOfVotes$voter_id, selectionOfVotes$party)
voterIdsAndAllTheirParties <- apply(voterIdsVsPartiesOccurances, 1, function(x) paste(colnames(voterIdsVsPartiesOccurances)[x>0], collapse=","))
voterIdsAndTheirMostFrequentParty <- apply(voterIdsVsPartiesOccurances, 1, function(x) paste(colnames(voterIdsVsPartiesOccurances)[which.max(x)], collapse=","))

voterIdsVsVoterNameOccurances <- table(selectionOfVotes$voter_id, selectionOfVotes$voter_name)
voterIdsAndTheirVoterName <- apply(voterIdsVsVoterNameOccurances, 1, function(x) paste(colnames(voterIdsVsVoterNameOccurances)[x>0], collapse=","))

selectionOfVotes$vote <- scores[as.character(selectionOfVotes$vote)]

votersAndTheirVotes_ <- spread(selectionOfVotes[,c("voter_id","vote","id_voting")], key = id_voting, value = vote)
voterIds <- votersAndTheirVotes_[,1]
rownames(votersAndTheirVotes_) <- voterIds

votersAndTheirVotes <- votersAndTheirVotes_[,-1] # removes the voter_id column, leaving only the votes (columns) of each voter (rows)

# only include parliament members that have voted on at least 90% of the votings
voteFilter <- rowMeans(is.na(votersAndTheirVotes)) < 0.1
partyRepresentedByEachVote <- voterIdsAndTheirMostFrequentParty[voteFilter]
consistentVotersAndTheirVotes <- votersAndTheirVotes[voteFilter,]
consistentVotersAndTheirVoterName <- voterIdsAndTheirVoterName[voteFilter]
dVotes <- dist(consistentVotersAndTheirVotes)

ag <- agnes(dVotes, method = "average")
hc = as.hclust(ag)
labels(hc) <- paste(consistentVotersAndTheirVoterName[order.hclust(hc)], " - ", voterIdsAndAllTheirParties[voteFilter][order.hclust(hc)], sep="")

par(mar=c(1,1,2,1), xpd=NA)
png(countrySpecificPath("plot5_fan.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plot(as.phylo(hc), type = "fan", cex = 0.4,
     tip.color = partyColors[partyRepresentedByEachVote],
     edge.width = 2,
     no.margin = TRUE,
     main=pattern,
     rotate.tree=-85)
dev.off()

par(mar=c(1,1,2,1), xpd=NA)
png(countrySpecificPath("plot4_unrooted.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plot(as.phylo(hc), type = "unrooted", cex = 0.4,
     tip.color = partyColors[partyRepresentedByEachVote],
     no.margin = TRUE,
     main=pattern,
     rotate.tree=-85)
dev.off()

par(mar=c(1,1,2,1), xpd=NA)
png(countrySpecificPath("plot5alt_radial.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plot(as.phylo(hc), type = "radial", cex = 0.4,
     tip.color = partyColors[partyRepresentedByEachVote],
     edge.width = 2,
     no.margin = TRUE,
     main=pattern,
     rotate.tree=-85)
dev.off()

par(mar=c(0,0,0,0), xpd=NA)
png(countrySpecificPath("plot3_phylogram.png"),
    width=as.integer(defaultWidth*2.0),
    height=as.integer(defaultHeight*2.0),
    pointsize=15,
)
plot(as.phylo(hc), type = "phylogram", cex = 2.5,
     tip.color = partyColors[partyRepresentedByEachVote],
     edge.width = 2,
     no.margin = TRUE,
     main=pattern)
dev.off()

par(mar=c(0,0,0,0), xpd=NA)
png(countrySpecificPath("plot3alt_cladogram.png"),
    width=as.integer(defaultWidth*2.0),
    height=as.integer(defaultHeight*2.0),
    pointsize=15,
)
plot(as.phylo(hc), type = "cladogram", cex = 2.5,
     tip.color = partyColors[partyRepresentedByEachVote],
     edge.width = 2,
     no.margin = TRUE,
     main=pattern)
dev.off()

# plots not mentioned in the original article

uniqueVoterIds <- levels(factor(selectionOfVotes$voter_id))

distMat <- matrix(NA, length(uniqueVoterIds),  length(uniqueVoterIds))
colnames(distMat) <- paste0(uniqueVoterIds, " (", voterIdsAndAllTheirParties, ")")
rownames(distMat) <- paste0(uniqueVoterIds, " (", voterIdsAndAllTheirParties, ")")

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
  save(distMat, file = countrySpecificPath("distMat.rda"))
} else {
  load(file = countrySpecificPath("distMat.rda"))
}

rem <- which(rowMeans(is.na(distMat)) > 0.01)
distMatR <- distMat[-rem, -rem]
rownames(distMatR) <- uniqueVoterIds[-rem]
colnames(distMatR) <- paste(uniqueVoterIds[-rem], voterIdsAndTheirMostFrequentParty[-rem])

library(MASS)

space <- isoMDS(as.dist(1.001-distMatR), k=2)
df <- data.frame(space$points, parties=voterIdsAndTheirMostFrequentParty[-rem], name=uniqueVoterIds[-rem])

library(ggplot2)

# a plot not mentioned in the original article
png(countrySpecificPath("plotB_aes.png"),
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
png(countrySpecificPath("plotC_phylo.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plot(as.phylo(hc), type = "fan", cex = 0.4,
     tip.color = colors[as.numeric(factor(voterIdsAndTheirMostFrequentParty[-rem]))],
     main=pattern)
dev.off()

