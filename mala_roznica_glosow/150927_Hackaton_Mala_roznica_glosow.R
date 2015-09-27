library(dplyr)
library(tidyr)
library(rvest)

#tylko plik all_votes
setwd("/Users/Alicja/Desktop")
load("all_votes.rda")

#scrapowanie nazw legislacji ze strony Mam Prawo Wiedziec
title_scrap <- function(page){
  page <- html(page)
  title <- html_nodes(page, ".naglowek-1")
  title <- html_text(title)
  return(title)
}

ids <- all_votes[,c(5,6)]
ids <- unique(ids)

n <- nrow(ids)
title <- character(n)

for(i in seq_len(n)){
  link <- paste0("http://mamprawowiedziec.pl/strona/glosowanie/7,",
                 ids[i, 1] , "," , ids[i, 2])
  title_enc <- title_scrap(link)
  #windows
  #title[i] <- iconv(title_enc, from = "UTF-8", to = "Windows-1250")
  #mac
  title[i] <- title_enc
}

idiki <- cbind(ids, title=title)
idiki[,3] <- as.character(idiki[,3])
idiki <- filter(idiki, title>0)

#filtrowanie po scrapowanych danych
all_votes %>% 
  filter(nr_meeting %in% idiki[,1], nr_voting %in% idiki[,2]) -> votes

votes <- merge(votes, idiki, by.x=c("nr_meeting", "nr_voting"), by.y=c("nr_meeting", "nr_voting"))

#glosowania dla legislacji wedlug partii
votes %>%  
  group_by(title, id_voting, club, vote) %>% 
  summarise(liczba=n()) %>%
  spread(vote, liczba) %>%
  replace(is.na(.), 0) %>%
  mutate(roznica = abs(Za - Przeciw)) %>%
  arrange(title, id_voting, club) -> glosowania_partie

#glosowania dla legislacji ogolem
votes %>% 
  group_by(title, id_voting, vote) %>% 
  summarise(liczba=n()) %>%
  mutate(liczba = ifelse(is.na(liczba),0,liczba)) %>%
  spread(vote, liczba) %>%
  replace(is.na(.), 0) %>%
  mutate(roznica = abs(Za - Przeciw)) %>%
  arrange(roznica) -> glosowania_ogolem

#wybor glosowan, w ktorych roznica glosow byla mniejsza od 21
glos <- as.numeric(unlist(unique(subset(glosowania_ogolem, glosowania_ogolem$roznica < 21)[,2])))
glosowania_ogolem_20 <- subset(glosowania_ogolem, glosowania_ogolem$id_voting %in% glos)
glosowania_partie_20 <- subset(glosowania_partie, glosowania_partie$id_voting %in% glos)

#wybor poslow zmieniajacych partie
#ile glosowan w kazdym klubie
migranci <- all_votes %>%
  group_by(surname_name, club) %>%
  summarise(ile_glosowal = n()) %>%
  spread(club, ile_glosowal) %>%
  replace(is.na(.), 0)

#ile razy zmienial klub
migranci2 <- all_votes %>%
  group_by(surname_name, club) %>%
  summarise(ile_glosowal = n()) %>%
  group_by(surname_name) %>%
  summarise(ile_klubow = n()) 

#zbiorowa informacja o migrantach
mig <- merge(migranci, migranci2)
mig %>% 
  arrange(desc(ile_klubow)) %>%
  filter(ile_klubow > 1) -> mig

glosowania_mig <- subset(votes, votes$surname_name %in% mig$surname_name)
