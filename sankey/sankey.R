library(dplyr)
library(tidyr)
library(networkD3)

#wybieramy kluczowe głosowania
kluczowe <- c("45", "974", "2474","3773","3980","4865","6029")

#możemy też wybrać kluby do pokazania
#kluby <- c( "PiS", "PO", "PSL", "SLD", "TR", "BiG", "KPSP", "SP", "ID",  "ZP", "RP", "BC", "niez.", "PSL", "SLD")

#dane do wizualizacji
liczby <- all_votes %>%
  filter(id_voting %in% kluczowe) %>%
  #filter(club %in% kluby) %>% # jeślichcemy pokazać wybrane kluby
  select(surname_name, id_voting, club) %>%
  group_by(surname_name, id_voting, club) %>%
  summarise(ilu = n()) %>%
  select(-ilu) %>%
  replace(is.na(.), "") %>%
  group_by(surname_name) 

#transpozycja głosowań do zmiennych
liczby_spr <- liczby %>% 
  spread(id_voting, club) %>%
   replace(is.na(.), "_N.O.")

#funkcja zamieniająca wartości dla kandydatów na mandaty partyjne (klubowe)
fixtable <- function(...) {
  tab <- table(...)
  if (substr(colnames(tab)[1],1,1) == "_" &
     substr(rownames(tab)[1],1,1) == "_") {
      tab2 <- tab
      colnames(tab2) <- sapply(strsplit(colnames(tab2), split=" "), `[`, 1)
      rownames(tab2) <- sapply(strsplit(rownames(tab2), split=" "), `[`, 1)
      tab2[1,1] <- 0
      # mandat w klubie
      for (par in names(which(tab2[1,] > 0))) {
        delta = min(tab2[par, 1], tab2[1, par])
        tab2[par, par] = tab2[par, par] + delta
        tab2[1, par] = tab2[1, par] - delta
        tab2[par, 1] = tab2[par, 1] - delta
      }
      # przechodzi przez niezalezy
      for (par in names(which(tab2[1,] > 0))) {
        tab2["niez.", par] = tab2["niez.", par] + tab2[1, par]
        tab2[1, par] = 0
      }
      for (par in names(which(tab2[,1] > 0))) {
        tab2[par, "niez."] = tab2[par, "niez."] + tab2[par, 1]
        tab2[par, 1] = 0
      }
      
     tab[] <- tab2[] 
  }
  tab
}

#matryca przepływów między posiedzeniami
flow <- rbind(
  data.frame(fixtable(z = paste0(liczby_spr$"45",  " (45)"), do = paste0(liczby_spr$"974", " (974)"))),
  data.frame(fixtable(z = paste0(liczby_spr$"974",  " (974)"), do = paste0(liczby_spr$"2474", " (2474)"))),
  data.frame(fixtable(z = paste0(liczby_spr$"2474", " (2474)"), do = paste0(liczby_spr$"3773", " (3773)"))),
  data.frame(fixtable(z = paste0(liczby_spr$"3773", " (3773)"), do = paste0(liczby_spr$"3980", " (3980)"))),
  data.frame(fixtable(z = paste0(liczby_spr$"3980", " (3980)"), do = paste0(liczby_spr$"4865", " (4865)"))),
  data.frame(fixtable(z = paste0(liczby_spr$"4865", " (4865)"), do = paste0(liczby_spr$"6029", " (6029)"))))

#wykluczenie wartości zerowych z matrycy
flow <- flow[flow[,3] > 0,]

#lista węzłów
nodes <- data.frame(name=unique(c(levels(factor(flow[,1])), levels(factor(flow[,2])))))
nam <- seq_along(nodes[,1])-1
names(nam) <- nodes[,1]

#dane do diagramu
links <- data.frame(source = nam[as.character(flow[,1])],
                    target = nam[as.character(flow[,2])],
                    value = flow[,3])

#diagram 
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontFamily = "Arial", fontSize = 12, nodeWidth = 40,
              colourScale = "d3.scale.category20()")
