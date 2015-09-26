migranci <- all_votes %>%
  select(surname_name, club) %>%
  group_by(surname_name, club) %>%
  summarise(ile_glosowal = n()) %>%
  group_by(surname_name) %>%
  summarise(ile_klubow = n(),
            maksi = head(club, 1)) %>%
  filter(ile_klubow>1) %>%
  arrange(desc(ile_klubow))