# Insert Datasets

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

# Batting

#Batting Average
avg <- {
  battingQ %>% 
    arrange(desc(AVG), desc(AB)) %>%
    dplyr::select(PLAYER, Team, AVG) %>% 
    slice_head(n = 5)
}
names(avg)[3] = "x"

#Home Runs
hr <- {
  batting %>% 
    arrange(desc(HR), PA) %>% 
    dplyr::select(PLAYER, Team, HR) %>% 
    slice_head(n = 5)
}
names(hr)[3] = "x"

#RBI
rbi <- {
  batting %>% 
    arrange(desc(RBI), PA) %>% 
    dplyr::select(PLAYER, Team, RBI) %>% 
    slice_head(n = 5)
}
names(rbi)[3] = "x"

#OPS
ops <- {
  battingQ %>% 
    arrange(desc(OPS), desc(PA)) %>% 
    dplyr::select(PLAYER, Team, OPS) %>% 
    slice_head(n = 5)
}
names(ops)[3] = "x"

# Pitching

#ERA
era <- {
  pitchingQ %>% 
    arrange(ERA, desc(IPx), BF) %>% 
    dplyr::select(PLAYER, Team, ERA) %>% 
    slice_head(n = 5)
}
names(era)[3] = "x"


#Strikeouts
k <- {
  pitching %>% 
    arrange(desc(K), IPx, BF) %>% 
    dplyr::select(PLAYER, Team, K) %>% 
    slice_head(n = 5)
}
names(k)[3] = "x"


#Wins
w <- {
  pitching %>% 
    arrange(desc(W), ERA, desc(GS), desc(IPx)) %>% 
    dplyr::select(PLAYER, Team, W) %>% 
    slice_head(n = 5)
}
names(w)[3] = "x"

#Saves
sv <- {
  pitching %>% 
    arrange(desc(SV), ERA, desc(G), desc(IPx)) %>% 
    dplyr::select(PLAYER, Team, SV) %>% 
    slice_head(n = 5)
}
names(sv)[3] = "x"


#Exportable Leaders Dataset

leaders <- dplyr::bind_rows(avg, hr, rbi, ops,
                            era, k, w, sv)


write_xlsx(leaders, "C:/Users/My Laptop/OneDrive/Desktop/Battle Creek/League Leaders.xlsx")


#Advanced

# Batting

#woba
woba <- {
  battingQ %>% 
    arrange(desc(wOBA), desc(PA)) %>%
    dplyr::select(PLAYER, Team, wOBA) %>% 
    slice_head(n = 5)
}
names(woba)[3] = "x"

#iso
iso <- {
  battingQ %>% 
    arrange(desc(ISOP), desc(PA)) %>% 
    dplyr::select(PLAYER, Team, ISOP) %>% 
    slice_head(n = 5)
}
names(iso)[3] = "x"

#wsb
wsb <- {
  batting %>% 
    arrange(desc(wSB), desc(SB)) %>% 
    dplyr::select(PLAYER, Team, wSB) %>% 
    slice_head(n = 5)
}
names(wsb)[3] = "x"

#war
warb <- {
  batting %>% 
    arrange(desc(WAR), PA) %>% 
    dplyr::select(PLAYER, Team, WAR) %>% 
    slice_head(n = 5)
}
names(warb)[3] = "x"

# Pitching

#FIP
fip <- {
  pitchingQ %>% 
    arrange(FIP, desc(IPx), BF) %>% 
    dplyr::select(PLAYER, Team, FIP) %>% 
    slice_head(n = 5)
}
names(fip)[3] = "x"


#BB%
bbper <- {
  pitchingQ %>% 
    arrange(`BB%`, desc(IPx), desc(BF)) %>% 
    dplyr::select(PLAYER, Team, `BB%`) %>% 
    slice_head(n = 5)
}
names(bbper)[3] = "x"


#K%
kper <- {
  pitchingQ %>% 
    arrange(desc(`K%`), desc(IPx), desc(BF)) %>% 
    dplyr::select(PLAYER, Team, `K%`) %>% 
    slice_head(n = 5)
}
names(kper)[3] = "x"

warp <- {
  pitching %>% 
    arrange(desc(WAR), desc(IPx)) %>% 
    dplyr::select(PLAYER, Team, WAR) %>% 
    slice_head(n = 5)
}
names(warp)[3] = "x"


#Exportable Leaders Dataset

leadersADV <- dplyr::bind_rows(woba, iso, wsb, warb,
                            fip, bbper, kper, warp)

# write_xlsx(leadersADV, ".xlsx")

