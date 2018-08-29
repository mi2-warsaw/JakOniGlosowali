partyColors <- c(PO = "orange3", PiS = "blue4", RP = "gold3", PSL="green4", SLD="red3", SP="blue1",
                 `niez.` = "grey", ID="gold4", TR="gold3", KPSP="blue2", BiG="orange2",
                 ZP="blue2", BC ="blue2" )
# Nieobecny = Absent, Przeciw = Against, Wstrzymał się = Abstained, Za = For

load(countrySpecificPath("all_votes.rda"))

# translate the vote column
all_votes$vote[all_votes$vote == "Nieobecny"] <- "Absent"
all_votes$vote[all_votes$vote == "Przeciw"] <- "Against"
all_votes$vote[all_votes$vote == "Wstrzymał się"] <- "Abstained"
all_votes$vote[all_votes$vote == "Za"] <- "For"

# the data column "club" stands for which party/parties the member was a member of
# translating here to "party" for clarity
colnames(all_votes)[colnames(all_votes)=="club"] <- "party"

# renaming column "surname_name" to "voter_name" for clarity
colnames(all_votes)[colnames(all_votes)=="surname_name"] <- "voter_name"

# using column "voter_name" as a surrogate for "voter_id" (works as long as members have unique names)
all_votes$voter_id <- all_votes$voter_name

# debug
# head(grep(unique(all_votes$topic_voting), pattern = "szkolnict", value = TRUE))

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

# optionally filter votes on specific topics (an empty pattern = no filter)
pattern <- "o ochronie zwierząt" # about animal protection
pattern <- "szkolnict" # school
pattern <- ""
