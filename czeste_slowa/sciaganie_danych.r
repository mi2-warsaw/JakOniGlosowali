if (!require(devtools)) {
  install.packages("devtools")
  require(devtools)
}
install_github("mi2-warsaw/sejmRP/sejmRP")

library(sejmRP)

st <- get_statements_table(dbname = 'sejmrp', user = 'reader', password = 'qux94874',
                           host = 'services.mini.pw.edu.pl', sorted_by_id = TRUE)

write.csv(st, "sejm_statements.csv")

vs <- get_votes_table(dbname='sejmrp', user='reader', password='qux94874', 
                      host='services.mini.pw.edu.pl', sorted_by_id=TRUE, windows=FALSE)

load("./all_votes.rda")

write.csv(all_votes, "all_votes.csv")
