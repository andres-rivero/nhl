library(rvest)
library(dplyr)

clubs <- cbind(
  "club" = c("ANA", "ARI", "BOS", "BUF", "CGY",
             "CAR", "CHI", "COL", "CBJ", "DAL",
             "DET", "EDM", "FLA", "LAK", "MIN",
             "MTL", "NSH", "NJD", "NYI", "NYR",
             "OTT", "PHI", "PIT", "SJS", "STL",
             "TBL", "TOR", "VAN", "VEG", "WSH", "WPG"),
  "club_name" = c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", 
                  "Buffalo Sabres", "Calgary Flames", "Carolina Hurricanes", 
                  "Chicago Blackhawks", "Colorado Avalanche", "Columbus Blue Jackets", 
                  "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers", 
                  "Florida Panthers", "Los Angeles Kings", "Minnesota Wild",
                  "Montreal Canadiens", "Nashville Predators", "New Jersey Devils", 
                  "New York Islanders", "New York Rangers", "Ottawa Senators", 
                  "Philadelphia Flyers", "Pittsburgh Penguins", "San Jose Sharks", 
                  "St. Louis Blues","Tampa Bay Lightning", "Toronto Maple Leafs", 
                  "Vancouver Canucks", "Vegas Golden Knights", "Washington Capitals", "Winnipeg Jets")
) %>%
  as.data.frame()


column <- c("gp","date","home_away","opponent","gf","ga",
            "win_loss","reg", "blank1",
            "team_shots", "team_pim", "team_ppg", "team_ppo", "team_shg", "blank2",
            "opp_shots", "opp_pim", "opp_ppg", "opp_ppo", "opp_shg", "blank3",
            "cf", "ca", "cf_pct", "ff", "fa", "ff_pct", "fow", "fol", "fo_pct", "ozs_pct", "pdo")



scrape <- function(x) {
  read_html(paste(
                  "https://www.hockey-reference.com/teams/",x,"/2019_gamelog.html",
                  sep = "")) %>%
  html_nodes('#tm_gamelog_rs') %>%
  html_table() %>%
  data.frame() %>%
  `colnames<-`(column) %>%
  filter(!gp == "GP") %>%
  filter(!win_loss == "") %>%
  select(-c("blank1", "blank2", "blank3")) %>%
  mutate(team = x)}

games <- c()
for (i in clubs[,1]) games <- rbind(games,{
  scrape(i)
})
rm(i)

asas <- inner_join(games, clubs, by = c("opponent" = "club_name"))
asas <- asas[,30:31] %>%
  `colnames<-`(c("team", "opponent"))
games <- cbind(asas, games[,-c(1, 4, 31)])
rm(asas)

