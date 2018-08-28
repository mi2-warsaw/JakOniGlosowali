library(cluster)
library(ggdendro)
library(ape)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(parallel)

numCores <- detectCores() # get the number of cores available

partyColors <- c(PO = "orange3", PiS = "blue4", RP = "gold3", PSL="green4", SLD="red3", SP="blue1",
          `niez.` = "grey", ID="gold4", TR="gold3", KPSP="blue2", BiG="orange2",
          ZP="blue2", BC ="blue2" )
scores <- c(`Nieobecny` = 0, `Przeciw` = -2, `Wstrzymał się` = -1, `Za` = 2)
# Nieobecny = Absent, Przeciw = Against, Wstrzymał się = Abstained, Za = For


load("all_votes.rda")

# the data column "club" stands for which party/parties the member was a member of
# translating here to "party" for clarity
all_votes$party <- all_votes$club

# pattern = pattern
pattern <- "o ochronie zwierząt" # about animal protection
pattern <- "szkolnict" # school
pattern <- ""

# debug
# head(grep(unique(all_votes$topic_voting), pattern = "szkolnict", value = TRUE))

votingTopicsThatMatchesPattern <- grep(unique(all_votes$topic_voting), pattern = pattern, value = TRUE)

selectionOfVotes <- all_votes %>%
  filter(topic_voting %in% votingTopicsThatMatchesPattern)

partiesAndTheirVotes <- table(selectionOfVotes$party,selectionOfVotes$vote)[,c(4,1,3,2)]
mosaicplot(partiesAndTheirVotes, off = c(0,0), border="white", 
           color=c("green3", "grey", "red4", "red2"), las=2,
           main="")



membersAndTheirParties <- table(selectionOfVotes$surname_name, selectionOfVotes$party)
partiesIncludingHistory <- apply(membersAndTheirParties, 1, function(x) paste(colnames(membersAndTheirParties)[x>0], collapse=","))
partiesOnlyLatest <- apply(membersAndTheirParties, 1, function(x) paste(colnames(membersAndTheirParties)[which.max(x)], collapse=","))

selectionOfVotes$vote <- scores[as.character(selectionOfVotes$vote)]

tVotes <- spread(selectionOfVotes[,c(1,3,4)], key = id_voting, value = vote)
rownames(tVotes) <- paste(tVotes[,1], " - ", partiesIncludingHistory[as.character(tVotes[,1])], sep="")
tVotes <- tVotes[,-1]

# tylko Ci w sejmie na ponad 90% glosowan = only those in the Seym over 90% of the votes
cVotes <- partiesOnlyLatest[rowMeans(is.na(tVotes)) < 0.1]
tVotes <- tVotes[rowMeans(is.na(tVotes)) < 0.1,]



dVotes <- dist(tVotes)

ag <- agnes(dVotes, method = "average")
hc = as.hclust(ag)

par(mar=c(1,1,2,1), xpd=NA)

plot(as.phylo(hc), type = "fan", cex = 0.4,
     tip.color = partyColors[cVotes],
     main=pattern,
     rotate.tree=-85)

plot(as.phylo(hc), type = "unrooted", cex = 0.4,
     tip.color = partyColors[cVotes],
     main=pattern,
     rotate.tree=-85)

plot(as.phylo(hc), type = "radial", cex = 0.4,
     tip.color = partyColors[cVotes],
     main=pattern,
     rotate.tree=-85)

plot(as.phylo(hc), type = "phylogram", cex = 0.4,
     tip.color = partyColors[cVotes],
     main=pattern,
     rotate.tree=-85)

plot(as.phylo(hc), type = "cladogram", cex = 0.4,
     tip.color = partyColors[cVotes],
     main=pattern,
     rotate.tree=-85)


# nazwy ustaw = names of laws
# ustawy = bill
# zmianie ustawy = change of the bill
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







uniqueMemberNames <- levels(factor(selectionOfVotes$surname_name))

membersAndTheirParties <- table(selectionOfVotes$surname_name, selectionOfVotes$party)
partiesIncludingHistory <- apply(membersAndTheirParties, 1, function(x) paste(colnames(membersAndTheirParties)[x>0], collapse=","))

partiesOnlyLatest <- apply(membersAndTheirParties, 1, function(x) paste(colnames(membersAndTheirParties)[which.max(x)], collapse=","))

distMat <- matrix(NA, length(uniqueMemberNames),  length(uniqueMemberNames))
colnames(distMat) <- paste0(uniqueMemberNames, " (", partiesIncludingHistory, ")")
rownames(distMat) <- paste0(uniqueMemberNames, " (", partiesIncludingHistory, ")")

recalculateDistMat <- TRUE
if (recalculateDistMat) {
  system.time({
    
    iMax <- (length(uniqueMemberNames)-1)
    jMax <- length(uniqueMemberNames)

    message("Pre-calculating lists of personal votes")
    results_selectionOfVotes <- mclapply(1:jMax,
                                 FUN=function(i) {
                                   selectionOfVotesI <- selectionOfVotes %>%
                                     filter(surname_name %in% uniqueMemberNames[i])
                                 },
                                 mc.cores = numCores)
    
    message("Summarizing intersections between member votes")
    for (i in 1:iMax) {
      cat("\n",i," ", uniqueMemberNames[i]," ")
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
rownames(distMatR) <- uniqueMemberNames[-rem]
colnames(distMatR) <- paste(uniqueMemberNames[-rem], partiesOnlyLatest[-rem])

library(MASS)

space <- isoMDS(as.dist(1.001-distMatR), k=2)
df <- data.frame(space$points, parties=partiesOnlyLatest[-rem], name=uniqueMemberNames[-rem])

library(ggplot2)

ggplot(df, aes(X1, X2, color=parties, label=name)) +
  geom_text(size=4) +
  theme_bw() + scale_shape_manual(values=LETTERS)

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

plot(as.phylo(hc), type = "fan", cex = 0.4,
     tip.color = colors[as.numeric(factor(partiesOnlyLatest[-rem]))],
     main=pattern)


