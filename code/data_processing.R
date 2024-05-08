######################################################################################
######################################################################################
######################################################################################
# Data processing scripts to produce the data used in the manuscript:
# 
#   "A New Framework to Estimate Return on Investment for
#     Player Salaries in the National Basketball Association"
#
# LAUTIER
# 2024
#
#R version 4.3.2 (2023-10-31 ucrt)
#RStudio 2023.03.0+386 "Cherry Blossom" Release
#(3c53477afb13ab959aeb5b34df1f10c237b256c3, 2023-03-09) for Windows
######################################################################################
######################################################################################
######################################################################################
######################################################################################
# INSTRUCTIONS
#
#supporting files:
#   2023regseason.csv   #raw regular season data    
#
#The code must be run sequentially downwards.
#As the new, cleaned files are prepared, they will be saved in a new
#folder 'processed_data' in the wd.
#For data analysis, proceed directly to 'data_analysis.R'.
#
#The figure and table references correspond to the May 2024 version
#of the working manuscript.  While the order of figures are subject to
#change, the figures themselves are likely to remain stable.
#
######################################################################################
######################################################################################
######################################################################################
######################################################################################
require(stringr) #convert minutes to usable formats
######################################################################################
######################################################################################
######################################################################################
######################################################################################

#where processed data will be stored
dir.create('./processed_data/')

#imported functions
source('./code/min_convert.R')

#WinLogit data cleaning

#bring in 38 fields from scraped 2023 reg season
path = "./raw_data/"
seas_dat = read.csv(paste(path,"2023regseason.csv",sep="/"))

seas_dat$MIN = as.numeric(sapply(seas_dat$MIN, min_convert))
seas_dat = subset(seas_dat, MIN > 0)

#cleaned season data; for example,
#minutes removed (no double counting)
#touches adjusted for passes received (no double counting)
#adjust PFD by charges drawn (no double counting)
seas_dat_clean = data.frame(
  "GAME_ID" = seas_dat$GAME_ID,
  "TEAM_ID" = seas_dat$TEAM_ID,
  "TEAM_ABBREVIATION" = seas_dat$TEAM_ABBREVIATION,
  "PLAYER_ID" = seas_dat$PLAYER_ID,
  "PLAYER_NAME" = seas_dat$PLAYER_NAME,
  "FG2O" = seas_dat$FGM - seas_dat$FG3M,
  "FG2X" = (seas_dat$FGA - seas_dat$FG3A) - (seas_dat$FGM - seas_dat$FG3M),
  "FG3O" = seas_dat$FG3M,
  "FG3X" = seas_dat$FG3A - seas_dat$FG3M,
  "FTMO" = seas_dat$FTM,
  "FTMX" = seas_dat$FTA - seas_dat$FTM,
  "PF" = seas_dat$PF,
  "PTS" = seas_dat$PTS,
  "STL" = seas_dat$STL,
  "AORB" = seas_dat$OREB - seas_dat$OREB_CONTEST,
  "ADRB" = seas_dat$DREB - seas_dat$DREB_CONTEST,
  "AST" = seas_dat$AST,
  "BLK" = seas_dat$BLK,
  "TOV" = seas_dat$TO,
  "BLKA" = seas_dat$BLKA,
  "PFD" = seas_dat$PFD - seas_dat$CHARGES_DRAWN,
  #"POSS" = seas_dat$POSS,
  "SAST" = seas_dat$SCREEN_ASSISTS,
  "DEFL" = seas_dat$DEFLECTIONS,
  "CHGD" = seas_dat$CHARGES_DRAWN,
  "AC2P" = seas_dat$CONTESTED_SHOTS_2PT - seas_dat$BLK,
  "C3P" = seas_dat$CONTESTED_SHOTS_3PT,
  "OBOX" = seas_dat$OFF_BOXOUTS,
  "DBOX" = seas_dat$DEF_BOXOUTS,
  "OLBR" = seas_dat$OFF_LOOSE_BALLS_RECOVERED,
  "DLBR" = seas_dat$DEF_LOOSE_BALLS_RECOVERED,
  "DFGO" = seas_dat$D_FGM,
  "DFGX" = seas_dat$D_FGA - seas_dat$D_FGM,
  "DRV" = seas_dat$DRIVES,
  "ODIS" = seas_dat$DIST_MILES_OFF,
  "DDIS" = seas_dat$DIST_MILES_DEF,
  #"TCH" = seas_dat$TOUCHES - seas_dat$PASSES_RECEIVED,
  "APM" = seas_dat$PASSES_MADE - seas_dat$AST - seas_dat$SECONDARY_AST - seas_dat$FT_AST,
  #seas_dat$POTENTIAL_AST - seas_dat$FT_AST,
  "AST2" = seas_dat$SECONDARY_AST,
  #"PAST" = seas_dat$POTENTIAL_AST - seas_dat$AST - seas_dat$FT_AST,
  "FAST" = seas_dat$FT_AST,
  "OCRB" = seas_dat$OREB_CONTEST,
  "AORC" = seas_dat$OREB_CHANCES - seas_dat$OREB,
  "DCRB" = seas_dat$DREB_CONTEST,
  "ADRC" = seas_dat$DREB_CHANCES - seas_dat$DREB
)

#remove games no tracking data available
no_track = c(22200635, 22200678, 22201199, 22201214)
seas_dat_clean = subset(seas_dat_clean, !(GAME_ID %in% no_track))

#cleaning negative values
sum(seas_dat_clean$AORB < 0) / length(seas_dat_clean$AORB)
seas_dat_clean$AORB = ifelse(seas_dat_clean$AORB < 0, 0, seas_dat_clean$AORB)

sum(seas_dat_clean$ADRB < 0) / length(seas_dat_clean$ADRB)
seas_dat_clean$ADRB = ifelse(seas_dat_clean$ADRB < 0, 0, seas_dat_clean$ADRB)

sum(seas_dat_clean$PFD < 0) / length(seas_dat_clean$PFD)
seas_dat_clean$PFD = ifelse(seas_dat_clean$PFD < 0, 0, seas_dat_clean$PFD)

sum(seas_dat_clean$AC2P < 0) / length(seas_dat_clean$AC2P)
seas_dat_clean$AC2P = ifelse(seas_dat_clean$AC2P < 0, 0, seas_dat_clean$AC2P)

#sum(seas_dat_clean$TCH < 0) / length(seas_dat_clean$TCH)
#seas_dat_clean$TCH = ifelse(seas_dat_clean$TCH < 0, 0, seas_dat_clean$TCH)

sum(seas_dat_clean$APM < 0) / length(seas_dat_clean$APM)
seas_dat_clean$APM = ifelse(seas_dat_clean$APM < 0, 0, seas_dat_clean$APM)

sum(seas_dat_clean$AORC < 0) / length(seas_dat_clean$AORC)
seas_dat_clean$AORC = ifelse(seas_dat_clean$AORC < 0, 0, seas_dat_clean$AORC)

sum(seas_dat_clean$ADRC < 0) / length(seas_dat_clean$ADRC)
seas_dat_clean$ADRC = ifelse(seas_dat_clean$ADRC < 0, 0, seas_dat_clean$ADRC)

write.csv(seas_dat_clean, "./processed_data/seas_dat_clean.csv")

#Win Score, Game Score data cleaning

path = "./raw_data"
seas_dat = read.csv(paste(path,"2023regseason.csv",sep="/"))

seas_dat = seas_dat[,-c(1,2)]

no_track = c(22200635, 22200678, 22201199, 22201214)
seas_dat = subset(seas_dat, !(GAME_ID %in% no_track))

seas_dat$MIN = as.numeric(sapply(seas_dat$MIN, min_convert))
seas_dat = subset(seas_dat, MIN > 0)
write.csv(seas_dat, './processed_data/seas_dat.csv')



