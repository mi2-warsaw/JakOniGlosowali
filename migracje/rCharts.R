library(rCharts)

head(all_votes)

selUniq <- all_votes %>%
  select(date_meeting, surname_name, club) %>%
  unique()

tab  <- table(selUniq$date_meeting, selUniq$club)

tab <- tab[seq(1,161,10),6:11]

dff <- as.data.frame(as.table(tab))
dff <- dff[dff$Freq != 0,]

n2 <- nPlot(Freq ~ Var1, group = 'Var2', data = dff, type = 'multiBarChart')
n2$chart(stacked = TRUE)
n2
