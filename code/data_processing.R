######################################################################################
######################################################################################
######################################################################################
# Data processing scripts to produce the data used in the manuscript:
# 
#   "A New Framework to Estimate Return on Investment for
#     Player Salaries in the National Basketball Association"
#
# LAUTIER
# 2025
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
#   2023regseason.csv   #raw regular season data (nba.com)
#   full_stats_box_score_pm.csv box-score pm data (basketball-ref.com)
#
#The code must be run sequentially downwards.
#As the new, cleaned files are prepared, they will be saved in a new
#folder 'processed_data' in the wd.
#For data analysis, proceed directly to 'data_analysis.R'.
#
#The figure and table references correspond to the Feb 2025 version
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
source('./code/min_convert_nba.R')

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

######################################################################################
######################################################################################
######################################################################################

#clean basketball reference data

#imported functions
source('./code/min_convert_bref.R')

#load compiled data
path = "./raw_data/"
raw_dat = read.csv(paste(path,"full_stats_box_score_pm.csv",sep="/"))

#clean first column, first row, remove team totals
raw_dat <- raw_dat[-1,-1]
raw_dat = raw_dat[!(raw_dat$PLAYER == "Team Totals"),]

#remove players with no PT
no_pt_key = c("Player Suspended", "Not With Team",
              "Did Not Play", "Did Not Dress")

raw_dat = raw_dat[!(raw_dat$MP %in% no_pt_key),]

#check for duplicates
raw_dat$key = paste(raw_dat$GAME_ID,
                    raw_dat$TEAM,
                    raw_dat$PLAYER,
                    sep=".")

n_occur <- data.frame(table(raw_dat$key))
dups = n_occur[n_occur$Freq > 1,]
total = nrow(dups)

player_list = c()
for(k in c(1:total)){
  
  player_list = append(
    player_list,
    str_split_1(as.character(dups$Var1[k]), "\\.")[3])
  
}

#start with most common entry error and work downwards
table(player_list)

################################################################################
#Seth Curry
################################################################################
player_name = "Seth Curry"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]

bad_data_phi = bad_data[bad_data$TEAM == "PHI",]
bad_data_phi[order(bad_data_phi$DATE),]

#https://www.basketball-reference.com/players/t/tuckepj01/gamelog/2023/
pt_min = c("33:01", "39:11", "28:36", "20:27", "31:27", "22:33", "22:35",
           "31:59", "24:51", "29:51", "24:03", "29:45", "32:21", "33:29",
           "37:04", "33:41", "25:14", "15:48", "28:13", "20:29", "21:22",
           "42:05", "33:13", "32:08", "26:35", "39:24", "21:07", "20:17",
           "18:57", "23:55")

pt_row = rownames(bad_data_phi)[(bad_data_phi$MP %in% pt_min)]
dm_row = rownames(bad_data_phi)[!(bad_data_phi$MP %in% pt_min)]
raw_dat[pt_row, c("PLAYER")] = "P.J. Tucker"
raw_dat[dm_row, c("PLAYER")] = "De'Anthony Melton"
raw_dat["20793", c("PLAYER")] = "Danuel House Jr."

bad_data_bkn = bad_data[bad_data$TEAM == "BRK",]
bad_data_bkn[order(bad_data_bkn$DATE),]

#https://www.basketball-reference.com/players/c/curryse01/gamelog/2023/
sc_min = c("20:27", "14:20", "28:17", "32:43", "21:51", "21:51", "6:36",
           "9:27", "18:33", "24:59", "6:31", "20:10")

tw_row = rownames(bad_data_bkn)[!(bad_data_bkn$MP %in% sc_min)]
raw_dat[tw_row, c("PLAYER")] = "T.J. Warren"

################################################################################
#DeMar DeRozan
################################################################################
player_name = "DeMar DeRozan"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#more minutes assumed to be DeMar DeRozan
#e.g., https://www.basketball-reference.com/boxscores/202211210CHI.html
gd_row = rownames(bad_data)[c(1:length(rownames(bad_data))) %% 2 == 0]

raw_dat[gd_row, c("PLAYER")] = "Goran Dragić"

################################################################################
#Malachi Flynn
################################################################################
player_name = "Malachi Flynn"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/players/f/flynnma01/gamelog/2023/
mf_min = c("5:07", "9:23", "9:52", "6:02", "11:57", "22:48", "15:16",
           "28:13", "30:04", "6:04", "1:52", "2:37", "6:15", "28:17",
           "37:10", "18:55", "25:51", "22:48", "25:53", "30:12",
           "18:14")

jh_row = rownames(bad_data)[!(bad_data$MP %in% mf_min)]
raw_dat[jh_row, c("PLAYER")] = "Juancho Hernangómez"

################################################################################
#Wenyen Gabriel
################################################################################
player_name = "Wenyen Gabriel"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/players/g/gabriwe01/gamelog/2023/
#always less MIN than Dennis Schröder except 11-18-2022
wg_row = rownames(bad_data)[c(1:length(rownames(bad_data))) %% 2 == 0]
wg_row[14] = "32635"

ds_row = rownames(bad_data)[!(rownames(bad_data) %in% wg_row)]
raw_dat[ds_row, c("PLAYER")] = "Dennis Schröder"

################################################################################
#Duncan Robinson
################################################################################
player_name = "Duncan Robinson"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/players/r/robindu01/gamelog/2023/
dr_min = c("19:06", "13:09", "4:38", "23:09", "14:32", "20:55", "15:18",
           "25:34", "36:15", "14:03", "15:50")

dr_row = rownames(bad_data)[!(bad_data$MP %in% dr_min)]
raw_dat[dr_row, c("PLAYER")] = "Nikola Jović"

################################################################################
#Eric Gordon
################################################################################
player_name = "Eric Gordon"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#more minutes assumed to be Eric Gordon
#e.g., https://www.basketball-reference.com/players/g/gordoer01/gamelog/2023/
tw_row = rownames(bad_data)[c(1:length(rownames(bad_data))) %% 2 == 0]

raw_dat[tw_row, c("PLAYER")] = "TyTy Washington Jr."

################################################################################
#Xavier Tillman Sr.
################################################################################
player_name = "Xavier Tillman Sr."
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/players/r/robindu01/gamelog/2023/
xt_min = c("8:54", "17:07", "10:21", "2:39", "14:23", "20:17")
vw_min = c("6:16", "4:40", "7:23", "2:39", "4:43", "13:55")
kl_min = c("8:54", "4:40", "4:43", "4:07")

xt_row = rownames(bad_data)[(bad_data$MP %in% xt_min)]
xt_row = xt_row[-c(5, 6)]
vw_row = rownames(bad_data)[(bad_data$MP %in% vw_min)]
vw_row = vw_row[-c(2, 4, 6)]
kl_row = rownames(bad_data)[(bad_data$MP %in% kl_min)]
kl_row = kl_row[-c(2, 5, 7)]
#check
rownames(bad_data) %in% c(xt_row, vw_row, kl_row)

raw_dat[vw_row, c("PLAYER")] = "Vince Williams Jr."
raw_dat[kl_row, c("PLAYER")] = "Kenneth Lofton Jr."

################################################################################
#Bruno Fernando
################################################################################
player_name = "Bruno Fernando"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#more minutes assumed to be Bruno Fernando
#https://www.basketball-reference.com/players/f/fernabr01/gamelog/2023/
bm_row = rownames(bad_data)[c(1:length(rownames(bad_data))) %% 2 == 0]

raw_dat[bm_row, c("PLAYER")] = "Boban Marjanović"

################################################################################
#Khem Birch, Gary Trent Jr., Austin Reaves, Cody Martin
################################################################################
player_name = "Khem Birch"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/players/b/birchkh01/gamelog/2023/
op_row = c("27292", "28066")
raw_dat[op_row, c("PLAYER")] = "Otto Porter Jr."

player_name = "Gary Trent Jr."
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/players/t/trentga02.html
rh_row = c("1092", "16558")
raw_dat[rh_row, c("PLAYER")] = "Ron Harper Jr."

player_name = "Austin Reaves"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/players/r/reaveau01/gamelog/2023/
sp_row = c("12141", "12170")
raw_dat[sp_row, c("PLAYER")] = "Scotty Pippen Jr."

player_name = "Cody Martin"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/boxscores/202210190SAS.html
tm_row = c("33358")
raw_dat[tm_row, c("PLAYER")] = "Théo Maledon"

player_name = "Theo Pinson"
bad_data =
  raw_dat[(raw_dat$PLAYER == player_name) & 
            (raw_dat$key %in% dups$Var1),c("PLAYER", "MP", "DATE", "TEAM")]
bad_data[order(bad_data$DATE),]

#https://www.basketball-reference.com/boxscores/202212290DAL.html
al_row = c("10002")
raw_dat[al_row, c("PLAYER")] = "A.J. Lawson"

################################################################################
#check all duplicate records corrected
################################################################################
raw_dat$key = paste(raw_dat$GAME_ID,
                    raw_dat$TEAM,
                    raw_dat$PLAYER,
                    sep=".")

n_occur <- data.frame(table(raw_dat$key))
dups = n_occur[n_occur$Freq > 1,]
dups

################################################################################
################################################################################
################################################################################

#https://www.basketball-reference.com/boxscores/202211220PHI.html
raw_dat["16391", c("PLAYER")] = "Michael Foster Jr."

#get player names to match nba spelling
path = "./raw_data/"
nba_names = read.csv(paste(path,"2023regseason.csv",sep="/"))
nba_names = unique(nba_names$PLAYER_NAME)

bpm_names = unique(raw_dat$PLAYER)

#missing players
nba_spell = sort(nba_names[!(nba_names %in% bpm_names)])
bpm_spell = sort(bpm_names[!(bpm_names %in% nba_names)])

#name key
spell_df = data.frame("nba_spell" = nba_spell,
                      "bpm_spell" = NA)

spell_df$bpm_spell[1:4] = bpm_spell[1:4]
spell_df$bpm_spell[5:6] = c("D.J. Augustin", "DaQuan Jeffries")
spell_df$bpm_spell[7:9] = bpm_spell[5:7]
spell_df$bpm_spell[10:12] = c("Derrick Favors", "E.J. Liddell", "Feron Hunt")
spell_df$bpm_spell[13] = bpm_spell[8]
spell_df$bpm_spell[14] = c("Ibou Badji")
spell_df$bpm_spell[15:20] = bpm_spell[11:16]
spell_df$bpm_spell[21] = c("Kostas Antetokounmpo")
spell_df$bpm_spell[22:26] = bpm_spell[17:21]
spell_df$bpm_spell[27] = bpm_spell[23]
spell_df$bpm_spell[28] = bpm_spell[10]
spell_df$bpm_spell[29:31] = bpm_spell[24:26]
spell_df$bpm_spell[32:33] = bpm_spell[28:29]
spell_df$bpm_spell[34:37] = bpm_spell[c(31, 33, 35, 36)]

#change bball ref names to nba names
for(k in c(1:nrow(spell_df))){
  
  raw_dat$PLAYER[raw_dat$PLAYER == spell_df$bpm_spell[k]] =
    spell_df$nba_spell[k]
  
}

#check
bpm_names = unique(raw_dat$PLAYER)
sort(nba_names[!(nba_names %in% bpm_names)])

################################################################################
################################################################################
################################################################################

#fix team abbreviations to match NBA
path = "./raw_data/"
nba_teams = read.csv(paste(path,"2023regseason.csv",sep="/"))
nba_teams = unique(nba_teams$TEAM_ABBREVIATION)

nba_teams[!(nba_teams %in% unique(raw_dat$TEAM))]

raw_dat$TEAM = ifelse(raw_dat$TEAM == "BRK", "BKN",raw_dat$TEAM)
raw_dat$TEAM = ifelse(raw_dat$TEAM == "CHO", "CHA",raw_dat$TEAM)
raw_dat$TEAM = ifelse(raw_dat$TEAM == "PHO", "PHX",raw_dat$TEAM)

nba_teams[!(nba_teams %in% unique(raw_dat$TEAM))]

################################################################################
################################################################################
################################################################################

#data check via MPG
path = "./processed_data/"
nba_dat = read.csv(paste(path,"seas_dat.csv",sep="/"))
nba_dat = nba_dat[,c("GAME_ID", "TEAM_ABBREVIATION", "PLAYER_NAME", "MIN")]

bpm_dat = raw_dat[,c("GAME_ID", "TEAM", "PLAYER", "MP")]
colnames(bpm_dat) = c("GAME_ID", "TEAM_ABBREVIATION", "PLAYER_NAME", "MIN")

bpm_dat$MIN = as.numeric(sapply(bpm_dat$MIN, min_convert))

bpm_dat$KEY = paste(bpm_dat$GAME_ID,
                    bpm_dat$TEAM_ABBREVIATION,
                    bpm_dat$PLAYER_NAME,
                    sep=".")

nba_dat$KEY = paste(nba_dat$GAME_ID,
                    nba_dat$TEAM_ABBREVIATION,
                    nba_dat$PLAYER_NAME,
                    sep=".")

bpm_dat = bpm_dat[,c("MIN","KEY")]
colnames(bpm_dat) = c("BPM_MIN", "KEY")

#find unmatched keys
nba_missing = nba_dat[!(nba_dat$KEY %in% bpm_dat$KEY),]

#correct additional data errors
raw_dat$PLAYER[raw_dat$PLAYER == "Willie Cauley-Stein"] = "Danuel House Jr."
raw_dat$PLAYER[raw_dat$PLAYER == "Wayne Ellington"] = "Juan Toscano-Anderson"
raw_dat$PLAYER[raw_dat$PLAYER == "Paris Bass"] = "Dario Saric"
raw_dat$PLAYER[raw_dat$PLAYER == "Trevor Ariza"] = "Troy Brown Jr."
raw_dat$PLAYER[raw_dat$PLAYER == "Semi Ojeleye"] = "AJ Green"
raw_dat$PLAYER[raw_dat$PLAYER == "Moses Wright"] = "Moussa Diabate"
raw_dat$PLAYER[(raw_dat$PLAYER == "Micah Potter" &
                  raw_dat$TEAM == "DET")] = "Bojan Bogdanovic"
raw_dat$PLAYER[(raw_dat$PLAYER == "Danuel House Jr." &
                  raw_dat$TEAM == "HOU")] = "Jabari Smith Jr."
raw_dat$PLAYER[(raw_dat$PLAYER == "Monte Morris" &
                  raw_dat$TEAM == "DEN")] = "Kentavious Caldwell-Pope"
raw_dat$PLAYER[(raw_dat$PLAYER == "Bruce Brown" &
                  raw_dat$TEAM == "BKN")] = "Royce O'Neale"
raw_dat$PLAYER[(raw_dat$PLAYER == "Frank Kaminsky" &
                  raw_dat$TEAM == "PHX")] = "Duane Washington Jr."
raw_dat$PLAYER[(raw_dat$PLAYER == "Bojan Bogdanovic" &
                  raw_dat$TEAM == "UTA")] = "Talen Horton-Tucker"
raw_dat$PLAYER[(raw_dat$PLAYER == "Leandro Bolmaro" &
                  raw_dat$TEAM == "MIN")] = "Wendell Moore Jr."
raw_dat$PLAYER[(raw_dat$PLAYER == "Cody Martin" &
                  raw_dat$DATE <= "2022-12-31")] = "Theo Maledon"
raw_dat$PLAYER[(raw_dat$PLAYER == "Vernon Carey Jr." &
                  raw_dat$TEAM == "CHA")] = "Dennis Smith Jr."
raw_dat$PLAYER[(raw_dat$PLAYER == "Vernon Carey Jr." &
                  raw_dat$TEAM == "CHA")] = "Dennis Smith Jr."
raw_dat$PLAYER[(raw_dat$PLAYER == "Bruno Fernando" &
                  raw_dat$DATE >= "2022-10-22" &
                  raw_dat$DATE <= "2022-11-20")] = "Boban Marjanovic"
raw_dat$PLAYER[(raw_dat$PLAYER == "Wenyen Gabriel" &
                  raw_dat$DATE >= "2022-12-06" &
                  raw_dat$DATE <= "2022-12-18")] = "Dennis Schroder"
raw_dat$PLAYER[(raw_dat$PLAYER == "Juan Toscano-Anderson" &
                  raw_dat$TEAM == "GSW")] = "Patrick Baldwin Jr."
raw_dat$PLAYER[(raw_dat$PLAYER == "Malachi Flynn" &
                  (raw_dat$DATE == "2022-10-19" |
                     raw_dat$DATE == "2022-10-21" |
                     raw_dat$DATE == "2022-10-26" |
                     raw_dat$DATE == "2022-11-04" |
                     raw_dat$DATE == "2022-11-06" |
                     raw_dat$DATE == "2022-11-09" |
                     raw_dat$DATE == "2022-11-16" |
                     raw_dat$DATE == "2022-12-02" |
                     raw_dat$DATE == "2022-12-05" |
                     raw_dat$DATE == "2022-12-09" |
                     raw_dat$DATE == "2022-12-11" |
                     raw_dat$DATE == "2022-12-14"))] = "Juancho Hernangomez"
raw_dat$PLAYER[(raw_dat$PLAYER == "Khem Birch" &
                  raw_dat$DATE >= "2022-11-04" &
                  raw_dat$DATE <= "2022-11-12")] = "Otto Porter Jr."
raw_dat$PLAYER[(raw_dat$PLAYER == "Duncan Robinson" &
                  (raw_dat$DATE == "2022-11-18" |
                     raw_dat$DATE == "2022-11-21" |
                     raw_dat$DATE == "2022-11-23" |
                     raw_dat$DATE == "2022-11-25" |
                     raw_dat$DATE == "2022-11-27" |
                     raw_dat$DATE == "2022-12-02" |
                     raw_dat$DATE == "2022-12-05" |
                     raw_dat$DATE == "2022-12-08" |
                     raw_dat$DATE == "2022-12-12" |
                     raw_dat$DATE == "2022-12-23"))] = "Nikola Jovic"
raw_dat$PLAYER[(raw_dat$PLAYER == "Seth Curry" &
                  raw_dat$TEAM == "PHI")] = "P.J. Tucker"
raw_dat["20793",c("PLAYER")] = "P.J. Tucker"
raw_dat["33361",c("PLAYER")] = "Cody Martin"
raw_dat["30187",c("PLAYER")] = "Kenneth Lofton Jr."
raw_dat["15821",c("PLAYER")] = "A.J. Lawson"
raw_dat["9214",c("PLAYER")] = "Boban Marjanovic"
raw_dat["11462",c("PLAYER")] = "Scotty Pippen Jr."
raw_dat["5724",c("PLAYER")] = "TyTy Washington Jr."
raw_dat["16816",c("PLAYER")] = "T.J. Warren"
raw_dat["9098",c("PLAYER")] = "TyTy Washington Jr."
raw_dat["21409",c("PLAYER")] = "De'Anthony Melton"

#confirm no more missing fields w nba.com data
nba_dat = read.csv(paste(path,"seas_dat.csv",sep="/"))
nba_dat = nba_dat[,c("GAME_ID", "TEAM_ABBREVIATION", "PLAYER_NAME", "MIN")]

bpm_dat = raw_dat[,c("GAME_ID", "TEAM", "PLAYER", "MP")]
colnames(bpm_dat) = c("GAME_ID", "TEAM_ABBREVIATION", "PLAYER_NAME", "MIN")

bpm_dat$MIN = as.numeric(sapply(bpm_dat$MIN, min_convert))

bpm_dat$KEY = paste(bpm_dat$GAME_ID,
                    bpm_dat$TEAM_ABBREVIATION,
                    bpm_dat$PLAYER_NAME,
                    sep=".")

nba_dat$KEY = paste(nba_dat$GAME_ID,
                    nba_dat$TEAM_ABBREVIATION,
                    nba_dat$PLAYER_NAME,
                    sep=".")

bpm_dat = bpm_dat[,c("MIN","KEY")]
colnames(bpm_dat) = c("BPM_MIN", "KEY")

nba_missing = nba_dat[!(nba_dat$KEY %in% bpm_dat$KEY),]
nba_missing

#compare minutes, additional data integrity check
min_comp = merge(nba_dat, bpm_dat, by = "KEY")

min_comp$diff = round(abs(min_comp$MIN - min_comp$BPM_MIN),1)

bad_dat = min_comp[min_comp$diff > 0,]
sort(table(bad_dat$PLAYER_NAME))
bad_dat[bad_dat$diff > 0,]
#remaining differences deemed minimal

#go the other way
bpm_missing = bpm_dat[!(bpm_dat$KEY %in% nba_dat$KEY),]

game = c()
for(k in c(1:nrow(bpm_missing))){
  game = append(game, str_split_1(bpm_missing$KEY[k], "\\.")[1])
}
#games without tracking data 2022-2023
no_track = c(22200635, 22200678, 22201199, 22201214)
check = unique(game)[!(unique(game) %in% no_track)]
bpm_missing[which(game %in% check),]
#no minutes, confirmed OK

colnames(raw_dat)[colnames(raw_dat) == "PLAYER"] = "PLAYER_NAME"
colnames(raw_dat)[colnames(raw_dat) == "TEAM"] = "TEAM_ABBREVIATION"

raw_dat = subset(raw_dat, select=-c(key))

#append points
seas_dat$key = paste(seas_dat$GAME_ID,
                     seas_dat$TEAM_ABBREVIATION,
                     seas_dat$PLAYER_NAME,
                     sep=".")

pts_df = data.frame("key" = seas_dat$key,
                    "PTS" = seas_dat$PTS)

raw_dat$key = paste(raw_dat$GAME_ID,
                    raw_dat$TEAM,
                    raw_dat$PLAYER,
                    sep=".")

#missing trckng dat games + 2 entries no minutes, same as above - OK
bpm_missing = raw_dat[!(raw_dat$key %in% pts_df$key),]
nba_missing = pts_df[!(pts_df$key %in% raw_dat$key),] #none

raw_dat = merge(raw_dat, pts_df, by = "key") #lose no tracking games - OK
raw_dat = raw_dat[,-1]

write.csv(raw_dat, "./processed_data/seas_dat_bpm.csv")


