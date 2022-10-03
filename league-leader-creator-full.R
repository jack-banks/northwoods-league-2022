
library(tidyverse)
library(readxl)
library(writexl)

team_games <- {
  
  Tables <- read_excel("advanced-standings.xlsx", 
                       sheet = "StandingsRAW")
  names(Tables)[1:3] = c("Long", "Wins", "Losses")
  
  Tables %>% 
    dplyr::select(Long, Team, Wins, Losses) %>% 
    filter(Long != "Great Lakes West" &
             Long != "Great Plains East" &
             Long != "Great Plains West")
}
team_games$Wins = as.numeric(team_games$Wins)  
team_games$Losses = as.numeric(team_games$Losses)

team_games <- {
  team_games %>% 
    mutate(Games = Wins + Losses) %>% 
    mutate(PAQ = Games * 2.7) %>% 
    mutate(IPQ = Games * 0.8)
}


batting <- read_excel("master.xlsx", 
                      sheet = "Batting")

a <- merge(batting, team_games, by = "Team")

battingQ <- {
  a %>% 
    filter(PA >= PAQ) %>% 
    dplyr::select(-Long, -Wins, -Losses, -Games, -PAQ, -IPQ)
}

pitching <- read_excel("master.xlsx", 
                       sheet = "Pitching")

a <- merge(pitching, team_games, by = "Team")

pitchingQ <- {
  a %>% 
    filter(IPx >= IPQ) %>% 
    dplyr::select(-Long, -Wins, -Losses, -Games, -PAQ, -IPQ)
}

fielding <- read_excel("master.xlsx", 
                       sheet = "Fielding")


#Batting

#Hits
h <- {
  batting %>% 
    arrange(desc(H), PA) %>% 
    dplyr::select(PLAYER, Team, H) %>% 
    slice_head(n = 10)
}
names(h)[3] = "x"

#Doubles
`2b` <- {
  batting %>% 
    arrange(desc(`2B`), PA) %>% 
    dplyr::select(PLAYER, Team, `2B`) %>% 
    slice_head(n = 10)
}
names(`2b`)[3] = "x"

#Triples
`3b` <- {
  batting %>% 
    arrange(desc(`3B`), PA) %>% 
    dplyr::select(PLAYER, Team, `3B`) %>% 
    slice_head(n = 10)
}
names(`3b`)[3] = "x"

#Home Runs
hr <- {
  batting %>% 
    arrange(desc(HR), PA) %>% 
    dplyr::select(PLAYER, Team, HR) %>% 
    slice_head(n = 10)
}
names(hr)[3] = "x"

#Runs
r <- {
  batting %>% 
    arrange(desc(R), PA) %>% 
    dplyr::select(PLAYER, Team, R) %>% 
    slice_head(n = 10)
}
names(r)[3] = "x"

#RBI
rbi <- {
  batting %>% 
    arrange(desc(RBI), PA) %>% 
    dplyr::select(PLAYER, Team, RBI) %>% 
    slice_head(n = 10)
}
names(rbi)[3] = "x"

#BB
bb <- {
  batting %>% 
    arrange(desc(BB), PA) %>% 
    dplyr::select(PLAYER, Team, BB) %>% 
    slice_head(n = 10)
}
names(bb)[3] = "x"

#Strikeouts
kb <- {
  batting %>% 
    arrange(desc(K), PA) %>% 
    dplyr::select(PLAYER, Team, K) %>% 
    slice_head(n = 10)
}
names(kb)[3] = "x"

#HBP
hbp <- {
  batting %>% 
    arrange(desc(HBP), PA) %>% 
    dplyr::select(PLAYER, Team, HBP) %>% 
    slice_head(n = 10)
}
names(hbp)[3] = "x"

#SB
sb <- {
  batting %>% 
    arrange(desc(SB), PA) %>% 
    dplyr::select(PLAYER, Team, SB) %>% 
    slice_head(n = 10)
}
names(sb)[3] = "x"

#Runs Created
rc <- {
  batting %>% 
    arrange(desc(RC), PA) %>% 
    dplyr::select(PLAYER, Team, RC) %>% 
    slice_head(n = 10)
}
names(rc)[3] = "x"

#Errors
e <- {
  fielding %>% 
    arrange(desc(E), TC, INN) %>% 
    dplyr::select(PLAYER, Team, E) %>% 
    slice_head(n = 10)
}
names(e)[3] = "x"

#Batting Average
avg <- {
  battingQ %>% 
    arrange(desc(AVG), desc(AB)) %>%
    dplyr::select(PLAYER, Team, AVG) %>% 
    slice_head(n = 10)
}
names(avg)[3] = "x"

#On Base
obp <- {
  battingQ %>% 
    arrange(desc(OBP), desc(PA)) %>%
    dplyr::select(PLAYER, Team, OBP) %>% 
    slice_head(n = 10)
}
names(obp)[3] = "x"

#Slugging
slg <- {
  battingQ %>% 
    arrange(desc(SLG), desc(AB)) %>%
    dplyr::select(PLAYER, Team, SLG) %>% 
    slice_head(n = 10)
}
names(slg)[3] = "x"

#OPS
ops <- {
  battingQ %>% 
    arrange(desc(OPS), desc(PA)) %>% 
    dplyr::select(PLAYER, Team, OPS) %>% 
    slice_head(n = 10)
}
names(ops)[3] = "x"


#wOBA
woba <- {
  battingQ %>% 
    arrange(desc(wOBA), desc(PA)) %>% 
    dplyr::select(PLAYER, Team, wOBA) %>% 
    slice_head(n = 10)
}
names(woba)[3] = "x"

#SECA
seca <- {
  battingQ %>% 
    arrange(desc(SECA),desc(PA)) %>% 
    dplyr::select(PLAYER, Team, SECA) %>% 
    slice_head(n = 10)
}
names(seca)[3] = "x"

#ISO
iso <- {
  battingQ %>% 
    arrange(desc(ISOP), desc(PA)) %>% 
    dplyr::select(PLAYER, Team, ISOP) %>% 
    slice_head(n = 10)
}
names(iso)[3] = "x"

#wSB
wsb <- {
  batting %>% 
    arrange(desc(wSB), PA) %>% 
    dplyr::select(PLAYER, Team, wSB) %>% 
    slice_head(n = 10)
}
names(wsb)[3] = "x"

#singles
`1b` <- {
  batting %>% 
    arrange(desc(`1B`), PA) %>% 
    dplyr::select(PLAYER, Team, `1B`) %>% 
    slice_head(n = 10)
}
names(`1b`)[3] = "x"

#tb
tb <- {
  batting %>% 
    arrange(desc(TB),PA) %>% 
    dplyr::select(PLAYER, Team, TB) %>% 
    slice_head(n = 10)
}
names(tb)[3] = "x"

#XBH
xbh <- {
  batting %>% 
    arrange(desc(XBH), PA) %>% 
    dplyr::select(PLAYER, Team, XBH) %>% 
    slice_head(n = 10)
}
names(xbh)[3] = "x"

#Double Plays
dp <- {
  batting %>% 
    arrange(desc(DP), PA) %>% 
    dplyr::select(PLAYER, Team, DP) %>% 
    slice_head(n = 10)
}
names(dp)[3] = "x"

#BB%
`bb%` <- {
  battingQ %>% 
    arrange(desc(`BB%`), desc(PA)) %>% 
    dplyr::select(PLAYER, Team, `BB%`) %>% 
    slice_head(n = 10)
}
names(`bb%`)[3] = "x"

#K%
`k%` <- {
  battingQ %>% 
    arrange(desc(`K%`), desc(PA)) %>% 
    dplyr::select(PLAYER, Team, `K%`) %>% 
    slice_head(n = 10)
}
names(`k%`)[3] = "x"

#HR%
`hr%` <- {
  battingQ %>% 
    arrange(desc(`HR%`), desc(PA)) %>% 
    dplyr::select(PLAYER, Team, `HR%`) %>% 
    slice_head(n = 10)
}
names(`hr%`)[3] = "x"

#WAR
war <- {
  batting %>% 
    arrange(desc(WAR), PA) %>% 
    dplyr::select(PLAYER, Team, WAR) %>% 
    slice_head(n = 10)
}
names(war)[3] = "x"


#Combine

#Exportable Leaders Dataset

leadersFULL <- dplyr::bind_rows(h, `2b`, `3b`, hr, r, rbi, bb, kb,
                                hbp, sb, rc, e, avg, obp, slg, ops,
                                woba, seca, iso, wsb, `1b`, tb, xbh, dp,
                                `bb%`, `k%`, `hr%`, war)


# write_xlsx(leadersFULL, ".xlsx")

