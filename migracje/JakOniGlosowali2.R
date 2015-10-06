library(dplyr)
library(ggplot2)

#dane do wizualizacji - wszyscy poza PO i PiS
df <- all_votes %>%
#  filter(club != "PO" & club != "PiS") %>%
  group_by(nr_meeting, club, surname_name) %>%
  summarize(ile = n()) %>%
  arrange(nr_meeting, club, surname_name)

#zmiana zmiennej na jakościową - pokazanie na osi X tylko posiedzeń z głosowaniami
df$nr_meeting <- as.factor(df$nr_meeting)
df$date_meeting <- as.factor(paste(df$date_meeting))

#wykres rastrowy
ggplot(df, aes(x = nr_meeting, y = surname_name, fill = club)) +
  geom_raster() +
  scale_fill_brewer(palette = "Paired", guide = guide_legend(title = "Klub / koło")) +
  scale_x_discrete(breaks = c(15,30,45,60,75,90)) +
  scale_y_discrete(breaks = NULL) +
  xlab("Numer posiedzenia") +
  ylab("Posłowie")

#wykres słupkowy
df2 <- df
vec <- c("SLD","PSL","RP","TR","BiG","niez.","ZP","ID","KPSP","SP","BC","PO","PiS")
df2$club <- factor(df$club, 
                   levels=vec)
df3 <- do.call(rbind, lapply(vec, function(l) df2[df2$club==l,]))

library(ggthemes)

ggplot(df3, aes(x = nr_meeting, y = surname_name, fill = club, color = club)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired", guide = guide_legend(title = "Klub / koło")) +
  scale_color_brewer(palette = "Paired", guide = guide_legend(title = "Klub / koło")) +
  scale_x_discrete(breaks = c(15,30,45,60,75,90)) +
  scale_y_discrete(breaks = NULL) +
  xlab("Numer posiedzenia") +
  ylab("Liczba posłów w danym klubie (bez PO/PiS)") + theme_tufte()
