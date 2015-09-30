library(dplyr)
library(ggplot2)

#dane do wizualizacji - wszyscy poza PO i PiS
df <- all_votes %>%
  filter(club != "PO" & club != "PiS") %>%
  group_by(nr_meeting, club, surname_name) %>%
  summarize(ile = n()) %>%
  arrange(nr_meeting, club, surname_name)

#zmiana zmiennej na jakościową - pokazanie na osi X tylko posiedzeń z głosowaniami
df$nr_meeting <- as.factor(df$nr_meeting)

#wykres rastrowy
ggplot(df, aes(x = nr_meeting, y = surname_name, fill = club)) +
  geom_raster() +
  scale_fill_brewer(palette = "Paired", guide = guide_legend(title = "Klub / koło")) +
  scale_x_discrete(breaks = c(15,30,45,60,75,90)) +
  scale_y_discrete(breaks = NULL) +
  xlab("Numer posiedzenia") +
  ylab("Posłowie")

#wykres słupkowy
ggplot(df, aes(x = nr_meeting, y = surname_name, fill = club)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired", guide = guide_legend(title = "Klub / koło")) +
  scale_x_discrete(breaks = c(15,30,45,60,75,90)) +
  scale_y_discrete(breaks = NULL) +
  xlab("Numer posiedzenia") +
  ylab("Posłowie")
