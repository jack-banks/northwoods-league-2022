#libraries
library(tidyverse)
library(readxl)
library(writexl)

#import stat sheets

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

b <- merge(batting, team_games, by = "Team")


pitching <- read_excel("master.xlsx", 
                       sheet = "Pitching")

p <- merge(pitching, team_games, by = "Team")



#Duplicate Players

batting$PLAYER[which(batting$PLAYER == "Allen, D" & batting$Team == "RFD")] = "Allen, Du"
batting$PLAYER[which(batting$PLAYER == "Allen, D" & batting$Team == "WAU" & batting$G == 12)] = "Allen, Dw-Stint1"
batting$PLAYER[which(batting$PLAYER == "Allen, D" & batting$Team == "RFD" & batting$G > 12)] = "Allen, Dw-Stint2"

batting$PLAYER[which(batting$PLAYER == "Atkinson, B" & batting$Team == "KEN")] = "Atkinson, Bo"
batting$PLAYER[which(batting$PLAYER == "Atkinson, B" & batting$Team == "LAK")] = "Atkinson, Br"

batting$PLAYER[which(batting$PLAYER == "Bushnell, K" & batting$G == 13)] = "Bushnell, K-Stint1"
batting$PLAYER[which(batting$PLAYER == "Bushnell, K" & batting$G != 13)] = "Bushnell, K-Stint2"

batting$PLAYER[which(batting$PLAYER == "Campbell, K" & batting$Team == "DUL")] = "Campbell, Kr"
batting$PLAYER[which(batting$PLAYER == "Campbell, K" & batting$Team == "FDL")] = "Campbell, Ke"

batting$PLAYER[which(batting$PLAYER == "Conn, C" & batting$P == "2B")] = "Conn, Cl"
batting$PLAYER[which(batting$PLAYER == "Conn, C" & batting$P == "C")] = "Conn, Co"

batting$PLAYER[which(batting$PLAYER == "Iannantone, N" & batting$G == 20)] = "Iannantone, N-Stint1"
batting$PLAYER[which(batting$PLAYER == "Iannantone, N" & batting$G != 20)] = "Iannantone, N-Stint2"

batting$PLAYER[which(batting$PLAYER == "Kligman, E" & batting$Team == "DUL")] = "Kligman, E-DUL"
batting$PLAYER[which(batting$PLAYER == "Kligman, E" & batting$Team == "WAU")] = "Kligman, E-WAU"

batting$PLAYER[which(batting$PLAYER == "Mezzenga, I" & batting$Team == "WAU")] = "Mezzenga, I-WAU"
batting$PLAYER[which(batting$PLAYER == "Mezzenga, I" & batting$Team == "STC")] = "Mezzenga, I-STC"

batting$PLAYER[which(batting$PLAYER == "Miller, G" & batting$Team == "TVC")] = "Miller, G-TVC"
batting$PLAYER[which(batting$PLAYER == "Miller, G" & batting$Team == "GB")] = "Miller, G-GB"

batting$PLAYER[which(batting$PLAYER == "Rice, Z" & batting$Team == "KEN")] = "Rice, Z-KEN"
batting$PLAYER[which(batting$PLAYER == "Rice, Z" & batting$Team == "LAC")] = "Rice, Z-LAC"

batting$PLAYER[which(batting$PLAYER == "Rogers, J" & batting$Team == "WIR")] = "Rogers, Ja"
batting$PLAYER[which(batting$PLAYER == "Rogers, J" & batting$Team == "DUL")] = "Rogers, JD"

batting$PLAYER[which(batting$PLAYER == "Rosengard, B" & batting$Team == "MAD")] = "Rosengard, B-MAD"
batting$PLAYER[which(batting$PLAYER == "Rosengard, B" & batting$Team == "EC")] = "Rosengard, B-EC"

batting$PLAYER[which(batting$PLAYER == "Sagedahl, J" & batting$Team == "BIS")] = "Sagedahl, J-BIS"
batting$PLAYER[which(batting$PLAYER == "Sagedahl, J" & batting$Team == "WIL")] = "Sagedahl, J-WIL"

batting$PLAYER[which(batting$PLAYER == "Slack, C" & batting$Team == "LAC")] = "Slack, C-LAC"
batting$PLAYER[which(batting$PLAYER == "Slack, C" & batting$Team == "GB")] = "Slack, C-GB"

batting$PLAYER[which(batting$PLAYER == "Stevens, E" & batting$Team == "WIL")] = "Stevens, E-WIL"
batting$PLAYER[which(batting$PLAYER == "Stevens, E" & batting$Team == "MAD")] = "Stevens, E-MAD"

batting$PLAYER[which(batting$PLAYER == "Thompson, B" & batting$Team == "LAK")] = "Thompson, Bu"
batting$PLAYER[which(batting$PLAYER == "Thompson, B" & batting$Team == "FDL")] = "Thompson, Be"

batting$PLAYER[which(batting$PLAYER == "Williams, G" & batting$Team == "MAN")] = "Williams, Gar"
batting$PLAYER[which(batting$PLAYER == "Williams, G" & batting$Team == "KEN")] = "Williams, Gag"

pitching$PLAYER[which(pitching$PLAYER == "Combs, J" & pitching$Team == "BIS")] = "Combs, Jo"
pitching$PLAYER[which(pitching$PLAYER == "Combs, J" & pitching$Team == "DUL")] = "Combs, Ja"

pitching$PLAYER[which(pitching$PLAYER == "Gustafson, A" & pitching$Team == "TVC")] = "Gustafson, Ar"
pitching$PLAYER[which(pitching$PLAYER == "Gustafson, A" & pitching$Team == "WIL")] = "Gustafson, An"

pitching$PLAYER[which(pitching$PLAYER == "Horvath, T" & pitching$Team == "KMO")] = "Horvath, T-KMO"
pitching$PLAYER[which(pitching$PLAYER == "Horvath, T" & pitching$Team == "RFD")] = "Horvath, T-RFD"

pitching$PLAYER[which(pitching$PLAYER == "Martin, T" & pitching$Team == "MAN")] = "Martin, TJ"
pitching$PLAYER[which(pitching$PLAYER == "Martin, T" & pitching$Team == "DUL")] = "Martin, Tr"

pitching$PLAYER[which(pitching$PLAYER == "Martinez, J" & pitching$Team == "WAT")] = "Martin, Ja"
pitching$PLAYER[which(pitching$PLAYER == "Martinez, J" & pitching$Team == "STC")] = "Martin, Jo"

pitching$PLAYER[which(pitching$PLAYER == "Meyer, J" & pitching$Team == "GB")] = "Meyer, Jax"
pitching$PLAYER[which(pitching$PLAYER == "Meyer, J" & pitching$Team == "MAN")] = "Meyer, Jak"

pitching$PLAYER[which(pitching$PLAYER == "Mueller, M" & pitching$G >= 14)] = "Mueller, Mi"
pitching$PLAYER[which(pitching$PLAYER == "Mueller, M" & pitching$G < 14)] = "Mueller, Ma"

pitching$PLAYER[which(pitching$PLAYER == "Newman, J" & pitching$Team == "BC")] = "Newman, Je"
pitching$PLAYER[which(pitching$PLAYER == "Newman, J" & pitching$Team == "RFD")] = "Newman, Ja"

pitching$PLAYER[which(pitching$PLAYER == "Rybarczyk, T" & pitching$IPx > 50)] = "Rybarczyk, Ty"
pitching$PLAYER[which(pitching$PLAYER == "Rybarczyk, T" & pitching$IPx < 50)] = "Rybarczyk, Tr"

pitching$PLAYER[which(pitching$PLAYER == "Hoskins, L" & pitching$IP == 1.2 & pitching$R == 0 & pitching$BF == 3)] = "Hoskins, L-Stint1"
pitching$PLAYER[which(pitching$PLAYER == "Hoskins, L" & (pitching$IP != 1.2 | pitching$R != 0 | pitching$BF != 3))] = "Hoskins, L-Stint2"

#Qualifiers

battingQ <- {
  b %>% 
    filter(PA >= PAQ) %>% 
    dplyr::select(-Long, -Wins, -Losses, -Games, -PAQ, -IPQ)
}

pitchingQ <- {
  p %>% 
    filter(IPx >= IPQ) %>% 
    dplyr::select(-Long, -Wins, -Losses, -Games, -PAQ, -IPQ)
}


#Desired Order

cleanB <- {
  batting %>% 
    filter(PA >= 1) %>% 
    select(PLAYER, Team, WAR, AVG, OBP, SLG, OPS, G, AB, R, H, `2B`, `3B`, HR,
           RBI, BB, HBP, K, SF, SH, DP, SB, CS, PA, `1B`, XBH, TB, `BB%`, `K%`,
           `HR%`, wOBA, `OPS+`, `wRC+`, SECA, ISOP, `Bat Runs`, wSB, WAA) %>% 
    arrange(desc(WAR))
}

cleanBQ <- {
  battingQ %>% 
    select(PLAYER, Team, WAR, AVG, OBP, SLG, OPS, G, AB, R, H, `2B`, `3B`, HR,
           RBI, BB, HBP, K, SF, SH, DP, SB, CS, PA, `1B`, XBH, TB, `BB%`, `K%`,
           `HR%`, wOBA, `OPS+`, `wRC+`, SECA, ISOP, `Bat Runs`, wSB, WAA) %>% 
    arrange(desc(WAR))
}

cleanB100 <- {
  batting %>% 
    filter(PA >= 100) %>% 
    select(PLAYER, Team, WAR, AVG, OBP, SLG, OPS, G, AB, R, H, `2B`, `3B`, HR,
           RBI, BB, HBP, K, SF, SH, DP, SB, CS, PA, `1B`, XBH, TB, `BB%`, `K%`,
           `HR%`, wOBA, `OPS+`, `wRC+`, SECA, ISOP, `Bat Runs`, wSB, WAA) %>% 
    arrange(desc(WAR))
}


cleanP <- {
  pitching %>% 
    filter(BF >= 1) %>% 
    select(PLAYER, Team, WAR, ERA, FIP, WHIP, G, GS, W, L, SV, IP, H, R, ER, BB,
           K, `2B`, `3B`, HR, BK, WP, HB, IBB, BF, BAA, BABIP, `BB%`, `K%`, `HR%`,
           `K/9`, `FPS%`, `LOB%`, `ERA-`, `FIP-`, WAA) %>% 
    arrange(desc(WAR))
}

cleanPQ <- {
  pitchingQ %>% 
    select(PLAYER, Team, WAR, ERA, FIP, WHIP, G, GS, W, L, SV, IP, H, R, ER, BB,
           K, `2B`, `3B`, HR, BK, WP, HB, IBB, BF, BAA, BABIP, `BB%`, `K%`, `HR%`,
           `K/9`, `FPS%`, `LOB%`, `ERA-`, `FIP-`, WAA) %>% 
    arrange(desc(WAR))
}

cleanP36 <- {
  pitching %>% 
    filter(IPx >= 36) %>% 
    select(PLAYER, Team, WAR, ERA, FIP, WHIP, G, GS, W, L, SV, IP, H, R, ER, BB,
           K, `2B`, `3B`, HR, BK, WP, HB, IBB,BF, BAA, BABIP, `BB%`, `K%`, `HR%`,
           `K/9`, `FPS%`, `LOB%`, `ERA-`, `FIP-`, WAA) %>% 
    arrange(desc(WAR))
}

# write

# write_xlsx(cleanB, "B.xlsx")
# write_xlsx(cleanBQ, "BQ.xlsx")
# write_xlsx(cleanB100, "B100.xlsx")
# write_xlsx(cleanP, "P.xlsx")
# write_xlsx(cleanPQ, "PQ.xlsx")
# write_xlsx(cleanP36, "P36.xlsx")

