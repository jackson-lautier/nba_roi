######################################################################################
######################################################################################
######################################################################################
# Data analysis scripts to produce results used in the manuscript:
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
#   seas_dat_clean.csv    #cleaned WL data
#   seas_dat.csv          #cleaned WS, GS data
#   player_positions.csv  #player position data
#
#The code must be run sequentially downwards.
#As the new, cleaned files are prepared, they will be saved in a new
#folder 'processed_data' in the wd.
#For data analysis, proceed directly to 'data_analysis.R'.
#
#The figure and table references correspond to the January 2024 version
#of the working manuscript.  While the order of figures are subject to
#change, the figures themselves are likely to remain stable.
#
######################################################################################
######################################################################################
######################################################################################
######################################################################################
require('car') #logit function
require(extrafont) #get times new roman for charts
require(ggplot2) #plots
require(jrvFinance) #IRR calculations
require('cowplot') #grid plots
require(stringr) #logistic regression data manipulation
######################################################################################
######################################################################################
######################################################################################
######################################################################################

#results directory
dir.create('./results/')

#functions used throughout
source('./code/total_contrib_WL.R')
source('./code/total_contrib_WS.R')
source('./code/total_contrib_GS.R')
source('./code/plot_games_WL.R')
source('./code/total_contrib_WL_ROI.R')
source('./code/NBA_IRR_WL.R')
source('./code/total_contrib_WS_ROI.R')
source('./code/NBA_IRR_WS.R')
source('./code/total_contrib_GS_ROI.R')
source('./code/NBA_IRR_GS.R')

################################################################################
################################################################################
################################################################################
################################################################################
# Section 2.3
################################################################################
################################################################################
################################################################################
################################################################################


################################################################################
#Table 1 - WinLogit model parameters
################################################################################
seas_dat_clean = read.csv('./clean_data/seas_dat_clean.csv')
seas_dat_clean = seas_dat_clean[,-1]

#examine all fields to find 'winning' fields
game_ids = unique(seas_dat_clean$GAME_ID)
win_loss = c() #for game outcome

flds = list(
  FG2O = c(), 
  FG2X = c(), 
  FG3O = c(), 
  FG3X = c(),
  FTMO = c(), 
  FTMX = c(),
  PF = c(),
  AORB = c(),
  ADRB = c(),
  STL = c(),
  BLK = c(),
  TOV = c(),
  BLKA = c(),
  PFD = c(),
  AST = c(), 
  SAST = c(), 
  DEFL = c(),
  CHGD = c(),
  AC2P = c(),
  C3P = c(),
  OBOX = c(),
  DBOX = c(),
  OLBR = c(),
  DLBR = c(),
  DFGO = c(), 
  DFGX = c(),
  DRV = c(),
  ODIS = c(),
  DDIS = c(),
  APM = c(),
  AST2 = c(), 
  FAST = c(),
  OCRB = c(),
  AORC = c(),
  DCRB = c(),
  ADRC = c()
)

#confirm all fields included except PTS
names(seas_dat_clean)[which(names(seas_dat_clean) %in% names(flds) == FALSE)]

#create the team level logistic regression data
for(g in game_ids){
  
  cur_game = subset(seas_dat_clean, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  #game outcome
  p1 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[1]])
  p2 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[2]])
  
  win_loss = append(win_loss,1*c(p1 > p2, p2 > p1))
  
  #team stats
  for(j in c(1:length(flds))){
    
    f_name = names(flds)[j]
    tot1 = sum( cur_game[cur_game$TEAM_ID == teams[1], f_name])
    tot2 = sum( cur_game[cur_game$TEAM_ID == teams[2], f_name])
    
    flds[[j]] = append(flds[[j]], c(tot1, tot2))
    
  }
  
}

team_df = do.call(cbind,flds)
team_df = as.data.frame(team_df)
#center data to create average team interpretation
team_df = scale(team_df, scale=FALSE)
team_df = as.data.frame(team_df)

team_df$OUTCOME = win_loss

#full model with all 'possible' fields
model <- glm(OUTCOME~., family="binomial", data=team_df)

#basic diagnostics
car::vif(model) #minor colinearity issue for FGs; corrected in final model
pscl::pR2(model)["McFadden"] #model performing 'well'

#Table B.1. for paper online appendix/supplement
sum = summary(model)
as.data.frame(sum$coefficients)

#get significant coefficients
sig = 1*(as.vector(sum$coefficients[,'Pr(>|z|)']) < 0.10)
keep = as.data.frame(sum$coefficients)[(sig == 1),]
rownames(keep)

kill1 = rownames(as.data.frame(sum$coefficients)[(sig == 0),])
kill1 = kill1[-1] #remove intercept

# establish the final model from above results
game_ids = unique(seas_dat_clean$GAME_ID)
win_loss = c() #for game outcome

flds = list(
  FG2O = c(), 
  FG2X = c(), 
  FG3O = c(), 
  FG3X = c(),
  FTMO = c(), 
  FTMX = c(),
  PF = c(),
  AORB = c(),
  ADRB = c(),
  STL = c(),
  BLK = c(),
  TOV = c(),
  BLKA = c(),
  PFD = c(),
  AST = c(), 
  SAST = c(), 
  DEFL = c(),
  CHGD = c(),
  AC2P = c(),
  C3P = c(),
  OBOX = c(),
  DBOX = c(),
  OLBR = c(),
  DLBR = c(),
  DFGO = c(), 
  DFGX = c(),
  DRV = c(),
  ODIS = c(),
  DDIS = c(),
  APM = c(),
  AST2 = c(), 
  FAST = c(),
  OCRB = c(),
  AORC = c(),
  DCRB = c(),
  ADRC = c()
)

#remove non-significant fields
flds = within(flds, rm(list=kill1))

#to get weights on W/L importance
for(g in game_ids){
  
  cur_game = subset(seas_dat_clean, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  #game outcome
  p1 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[1]])
  p2 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[2]])
  
  win_loss = append(win_loss,1*c(p1 > p2, p2 > p1))
  
  #team stats
  for(j in c(1:length(flds))){
    
    f_name = names(flds)[j]
    tot1 = sum( cur_game[cur_game$TEAM_ID == teams[1], f_name])
    tot2 = sum( cur_game[cur_game$TEAM_ID == teams[2], f_name])
    
    flds[[j]] = append(flds[[j]], c(tot1, tot2))
    
  }
  
}

team_df = do.call(cbind,flds)
team_df = as.data.frame(team_df)
team_df = scale(team_df, scale = FALSE)
team_df = as.data.frame(team_df)

team_df$OUTCOME = win_loss

#winLogit model
model <- glm(OUTCOME~., family="binomial", data=team_df)
summary(model)
car::vif(model) #no more VIFs above 5
pscl::pR2(model)["McFadden"] #still performs 'well'
caret::varImp(model) #most important covariates

#winLogit model without intercept
model <- glm(OUTCOME ~ . - 1, family="binomial", data=team_df)
caret::varImp(model)

#summary of model with all significant fields
#Table 1
sum = summary(model)
as.data.frame(sum$coefficients)

#reasonableness checks at team level;
#note using no intercept introduces a small bias
team_prob = predict(model, team_df, type = "response")
team_df$game_prob = team_prob

probs = c()
for(i in c(1:nrow(team_df))){
  
  #lgt = c(1, as.numeric(team_df[i,c(1:24)])) %*% betas
  #p = exp(lgt) / (1 + exp(lgt))
  p = predict(model, team_df[i,], type = "response")
  probs = append(probs, p)
  
}
team_df$game_prob = probs

#expected wins
game_ids = unique(seas_dat_clean$GAME_ID)
teams = c()
for(g in game_ids){
  
  cur_game = subset(seas_dat_clean, GAME_ID == g)
  teams = append(teams, unique(cur_game$TEAM_ABBREVIATION))
  
}
team_df$team = teams

expW = c()
teams = unique(team_df$team)
for(t in teams){
  expW = append(expW,
                sum( team_df$game_prob[team_df$team == t] ))
}
exp_win = data.frame("team" = teams,"expW" = expW)

#exp_win[with(exp_win, order(-expW)), ]
sum(exp_win$expW) #demonstrate minimal bias, unbiased = 1226

################################################################################
#WinLogit calculations
################################################################################
games = unique(seas_dat_clean$GAME_ID)

#create dummy active player
df_15 = seas_dat_clean[1,]
df_15[1,"PLAYER_NAME"] = "Active Player"
df_15[1,"PLAYER_ID"] = 9999999
df_15[1,"GAME_ID"] = 99999999

#make sure each game has 15 active players
start.time <- Sys.time()
for(g in games){
  
  cur_game = subset(seas_dat_clean, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  df1 = subset(cur_game, TEAM_ID == teams[1])
  num_row_add = max(0,15 - nrow(df1))
  
  if(num_row_add > 0){
    add_row = df1[nrow(df1),]
    add_row$PLAYER_ID = 9999999
    add_row$PLAYER_NAME = "Active Player"
    add_row[,6:42] = 0
    
    for(r in c(1:num_row_add)){
      df1 = rbind(df1, add_row)
    }
  }
  
  df2 = subset(cur_game, TEAM_ID == teams[2])
  num_row_add = max(0,15 - nrow(df2))
  
  if(num_row_add > 0){
    add_row = df2[nrow(df2),]
    add_row$PLAYER_ID = 9999999
    add_row$PLAYER_NAME = "Active Player"
    add_row[,6:42] = 0
    
    for(r in c(1:num_row_add)){
      df2 = rbind(df2, add_row)
    }
  }
  
  cur_df15 = rbind(df1, df2)
  
  df_15 = rbind(df_15, cur_df15)
  
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

df_15 = df_15[-1,]

df_15_scale = scale(df_15[,6:42], center = TRUE, scale = FALSE)
df_15[,6:42] = df_15_scale

model_prob = predict(model, df_15, type = "response")
wLogit = logit(model_prob)

df_15$game_prob = model_prob
df_15$game_logit = wLogit

sum(df_15$game_logit) #note sums to zero for all players

#active players only!
df_15 = subset(df_15, PLAYER_NAME != "Active Player")

m_star = nrow(df_15)
m_bar = m_star / length(unique(df_15$GAME_ID))

mu = (1/m_star) * sum(df_15$game_logit)
sig = sqrt( (1/(m_star - 1)) * sum( (df_15$game_logit - mu)^2 ) )
mu; sig
#check
mu = mean(df_15$game_logit)
sig = sd(df_15$game_logit)
mu; sig

#equation (10)
df_15$win_logit = (1/sig) *
  (df_15$game_logit - mu) *
  (1/m_bar) + (1/m_bar)

write.csv(df_15, "./results/2023regseason_WL.csv")
################################################################################

#top PVWL performers
################################################################################

df = df_15
players = unique(df$PLAYER_NAME)

start.time <- Sys.time()
GP = c()
PVWL = c()
for(p in players){
  GP = append(GP, sum(total_contrib_WL(p)$game_totals != 0))
  PVWL = append(PVWL, total_contrib_WL(p)$sum_total)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

results = data.frame("player" = players,
                     "GP" = GP,
                     "PVWL" = PVWL,
                     "WLpg" = PVWL / GP)

#load position data
path = "./clean_data"
file = "player_positions.csv"

bio_dat = read.csv(paste(path,file,sep="/"))
names(bio_dat) = c("PLAYER_NAME", "POSITION")

#check all players available
results$player[!(results$player %in% bio_dat$PLAYER_NAME)]

pos = c()
for(p in results$player){
  pos = append(pos,
               bio_dat$POSITION[bio_dat$PLAYER_NAME == p])
}
results$position = pos
table(results$position)

#clean up multi-position listings
multi_pos = c("PF-C", "PF-SF", "PG-SG", "SF-PF", "SF-SG", "SG-PG")
results[results$position %in% multi_pos,]

#manual changes to positions (ESPN listing)
results$position[results$player == "Matisse Thybulle"] = "SG"
results$position[results$player == "Patrick Beverley"] = "PG"
results$position[results$player == "Kevin Knox II"] = "SF"
results$position[results$player == "Kyrie Irving"] = "PG"
results$position[results$player == "Spencer Dinwiddie"] = "PG"
results$position[results$player == "Mikal Bridges"] = "SF"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "George Hill"] = "PG"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "T.J. Warren"] = "SF"

table(results$position)

#PVWL

#get relative value per position
resultsPG = results[results$position == "PG",]
resultsPG$Z_val = scale(resultsPG$PVWL)

resultsSG = results[results$position == "SG",]
resultsSG$Z_val = scale(resultsSG$PVWL)

resultsSF = results[results$position == "SF",]
resultsSF$Z_val = scale(resultsSF$PVWL)

resultsPF = results[results$position == "PF",]
resultsPF$Z_val = scale(resultsPF$PVWL)

resultsC = results[results$position == "C",]
resultsC$Z_val = scale(resultsC$PVWL)

#top performers based on Z-score values relative to position
results = rbind(resultsPG, resultsSG, resultsSF,
                resultsPF, resultsC)

topX = 5
top_df = subset(results,
                Z_val >= 
                  quantile(Z_val,
                           1 - topX/nrow(results)))
top_df[with(top_df, order(-Z_val)), ]
#max overall
results$player[which.max(results$PVWL)]
#average player
82 * (1 / m_bar)

write.csv(results, './results/PVWL.csv')

################################################################################
#top PVWS performers
################################################################################
rm(list=ls())
source('./code/total_contrib_WS.R')

seas_dat = read.csv('./clean_data/seas_dat.csv')
seas_dat = seas_dat[,-1]

df_15 = seas_dat[1,]
df_15[1,"PLAYER_NAME"] = "Active Player"
df_15[1,"PLAYER_ID"] = 9999999
df_15[1,"GAME_ID"] = 99999999

games = unique(seas_dat$GAME_ID)

#make sure each game has 15 active players
start.time <- Sys.time()
for(g in games){
  
  cur_game = subset(seas_dat, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  df1 = subset(cur_game, TEAM_ID == teams[1])
  num_row_add = max(0,15 - nrow(df1))
  
  if(num_row_add > 0){
    add_row = df1[nrow(df1),]
    add_row$PLAYER_ID = 9999999
    add_row$PLAYER_NAME = "Active Player"
    add_row[,6:47] = 0
    
    for(r in c(1:num_row_add)){
      df1 = rbind(df1, add_row)
    }
  }
  
  df2 = subset(cur_game, TEAM_ID == teams[2])
  num_row_add = max(0,15 - nrow(df2))
  
  if(num_row_add > 0){
    add_row = df2[nrow(df2),]
    add_row$PLAYER_ID = 9999999
    add_row$PLAYER_NAME = "Active Player"
    add_row[,6:47] = 0
    
    for(r in c(1:num_row_add)){
      df2 = rbind(df2, add_row)
    }
  }
  
  cur_df15 = rbind(df1, df2)
  
  df_15 = rbind(df_15, cur_df15)
  
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

df_15 = df_15[-1,]

#cleaned season data
WSc_dat = data.frame(
  "GAME_ID" = df_15$GAME_ID,
  "TEAM_ID" = df_15$TEAM_ID,
  "TEAM_ABBREVIATION" = df_15$TEAM_ABBREVIATION,
  "PLAYER_ID" = df_15$PLAYER_ID,
  "PLAYER_NAME" = df_15$PLAYER_NAME,
  "PTS" = df_15$PTS,
  "ORB" = df_15$OREB,
  "DRB" = df_15$DREB,
  "STL" = df_15$STL,
  "BLK" = df_15$BLK,
  "AST" = df_15$AST,
  "FGA" = df_15$FGA,
  "FTA" = df_15$FTA,
  "TOV" = df_15$TO,
  "PF" = df_15$PF
)

#calculate per game win scores
WSc_dat$WSc = WSc_dat$PTS + WSc_dat$ORB + WSc_dat$DRB +
  WSc_dat$STL + 0.5 * WSc_dat$BLK + 0.5 * WSc_dat$AST -
  WSc_dat$FGA - 0.5 * WSc_dat$FTA - WSc_dat$TOV - 0.5 * WSc_dat$PF

#active players only!
WSc_dat = subset(WSc_dat, PLAYER_NAME != "Active Player")

m_star = nrow(WSc_dat)
m_bar = m_star / length(unique(WSc_dat$GAME_ID))

mu = (1/m_star) * sum(WSc_dat$WSc)
sig = sqrt( (1/(m_star - 1)) * sum( (WSc_dat$WSc - mu)^2 ) )
mu; sig
#check
mu = mean(WSc_dat$WSc)
sig = sd(WSc_dat$WSc)
mu; sig

#equation (12)
WSc_dat$game_WS = (1/sig) *
  (WSc_dat$WSc - mu) *
  (1/m_bar) + (1/m_bar)

write.csv(WSc_dat, './results/2023regseason_WS.csv')

df = WSc_dat
players = unique(df$PLAYER_NAME)

start.time <- Sys.time()
GP = c()
PVWS = c()
for(p in players){
  GP = append(GP, sum(total_contrib_WS(p)$game_totals != 0))
  PVWS = append(PVWS, total_contrib_WS(p)$sum_total)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

results = data.frame("player" = players,
                     "GP" = GP,
                     "PVWS" = PVWS,
                     "WSpg" = PVWS / GP)

#load position data
path = "./clean_data"
file = "player_positions.csv"

bio_dat = read.csv(paste(path,file,sep="/"))
names(bio_dat) = c("PLAYER_NAME", "POSITION")

#check all players available
results$player[!(results$player %in% bio_dat$PLAYER_NAME)]

pos = c()
for(p in results$player){
  pos = append(pos,
               bio_dat$POSITION[bio_dat$PLAYER_NAME == p])
}
results$position = pos
table(results$position)

#clean up multi-position listings
multi_pos = c("PF-C", "PF-SF", "PG-SG", "SF-PF", "SF-SG", "SG-PG")
results[results$position %in% multi_pos,]

#manual changes to positions (ESPN listing)
results$position[results$player == "Matisse Thybulle"] = "SG"
results$position[results$player == "Patrick Beverley"] = "PG"
results$position[results$player == "Kevin Knox II"] = "SF"
results$position[results$player == "Kyrie Irving"] = "PG"
results$position[results$player == "Spencer Dinwiddie"] = "PG"
results$position[results$player == "Mikal Bridges"] = "SF"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "George Hill"] = "PG"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "T.J. Warren"] = "SF"

table(results$position)

#PVWS

#get relative value per position
resultsPG = results[results$position == "PG",]
resultsPG$Z_val = scale(resultsPG$PVWS)

resultsSG = results[results$position == "SG",]
resultsSG$Z_val = scale(resultsSG$PVWS)

resultsSF = results[results$position == "SF",]
resultsSF$Z_val = scale(resultsSF$PVWS)

resultsPF = results[results$position == "PF",]
resultsPF$Z_val = scale(resultsPF$PVWS)

resultsC = results[results$position == "C",]
resultsC$Z_val = scale(resultsC$PVWS)

#top performers based on Z-score values relative to position
results = rbind(resultsPG, resultsSG, resultsSF,
                resultsPF, resultsC)

topX = 5
top_df = subset(results,
                Z_val >= 
                  quantile(Z_val,
                           1 - topX/nrow(results)))
top_df[with(top_df, order(-Z_val)), ]
#max overall
results$player[which.max(results$PVWS)]
#average player
82 * (1 / m_bar)

write.csv(results, './results/PVWS.csv')

################################################################################
#top PVGS performers
################################################################################
rm(list=ls())
source('./code/total_contrib_GS.R')

seas_dat = read.csv('./clean_data/seas_dat.csv')
seas_dat = seas_dat[,-1]

df_15 = seas_dat[1,]
df_15[1,"PLAYER_NAME"] = "Active Player"
df_15[1,"PLAYER_ID"] = 9999999
df_15[1,"GAME_ID"] = 99999999

games = unique(seas_dat$GAME_ID)

#make sure each game has 15 active players
start.time <- Sys.time()
for(g in games){
  
  cur_game = subset(seas_dat, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  df1 = subset(cur_game, TEAM_ID == teams[1])
  num_row_add = max(0,15 - nrow(df1))
  
  if(num_row_add > 0){
    add_row = df1[nrow(df1),]
    add_row$PLAYER_ID = 9999999
    add_row$PLAYER_NAME = "Active Player"
    add_row[,6:47] = 0
    
    for(r in c(1:num_row_add)){
      df1 = rbind(df1, add_row)
    }
  }
  
  df2 = subset(cur_game, TEAM_ID == teams[2])
  num_row_add = max(0,15 - nrow(df2))
  
  if(num_row_add > 0){
    add_row = df2[nrow(df2),]
    add_row$PLAYER_ID = 9999999
    add_row$PLAYER_NAME = "Active Player"
    add_row[,6:47] = 0
    
    for(r in c(1:num_row_add)){
      df2 = rbind(df2, add_row)
    }
  }
  
  cur_df15 = rbind(df1, df2)
  
  df_15 = rbind(df_15, cur_df15)
  
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

df_15 = df_15[-1,]

#cleaned season data
GmSc_dat = data.frame(
  "GAME_ID" = df_15$GAME_ID,
  "TEAM_ID" = df_15$TEAM_ID,
  "TEAM_ABBREVIATION" = df_15$TEAM_ABBREVIATION,
  "PLAYER_ID" = df_15$PLAYER_ID,
  "PLAYER_NAME" = df_15$PLAYER_NAME,
  "PTS" = df_15$PTS,
  "FG" = df_15$FGM,
  "FGA" = df_15$FGA,
  "FTA" = df_15$FTA,
  "FT" = df_15$FTM,
  "ORB" = df_15$OREB,
  "DRB" = df_15$DREB,
  "STL" = df_15$STL,
  "AST" = df_15$AST,
  "BLK" = df_15$BLK,
  "PF" = df_15$PF,
  "TOV" = df_15$TO
)

#calculate game scores
GmSc_dat$GmSc = GmSc_dat$PTS + 0.4 * GmSc_dat$FG -
  0.7 * GmSc_dat$FGA - 0.4 * (GmSc_dat$FTA - GmSc_dat$FT) +
  0.7 * GmSc_dat$ORB + 0.3 * GmSc_dat$DRB + GmSc_dat$STL +
  0.7 * GmSc_dat$AST + 0.7 * GmSc_dat$BLK - 0.4 * GmSc_dat$PF -
  GmSc_dat$TO

#active players only!
GmSc_dat = subset(GmSc_dat, PLAYER_NAME != "Active Player")

m_star = nrow(GmSc_dat)
m_bar = m_star / length(unique(GmSc_dat$GAME_ID))

mu = (1/m_star) * sum(GmSc_dat$GmSc)
sig = sqrt( (1/(m_star - 1)) * sum( (GmSc_dat$GmSc - mu)^2 ) )
mu; sig

mu = mean(GmSc_dat$GmSc)
sig = sd(GmSc_dat$GmSc)
mu; sig

#equation (11)
GmSc_dat$game_GS = (1/sig) *
  (GmSc_dat$GmSc - mu) *
  (1/m_bar) + (1/m_bar)

write.csv(GmSc_dat, './results/2023regseason_GS.csv')

df = GmSc_dat
players = unique(df$PLAYER_NAME)

start.time <- Sys.time()
GP = c()
PVGS = c()
for(p in players){
  GP = append(GP, sum(total_contrib_GS(p)$game_totals != 0))
  PVGS = append(PVGS, total_contrib_GS(p)$sum_total)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

results = data.frame("player" = players,
                     "GP" = GP,
                     "PVGS" = PVGS,
                     "GSpg" = PVGS / GP)


#load position data
path = "./clean_data"
file = "player_positions.csv"

bio_dat = read.csv(paste(path,file,sep="/"))
names(bio_dat) = c("PLAYER_NAME", "POSITION")

#check all players available
results$player[!(results$player %in% bio_dat$PLAYER_NAME)]

pos = c()
for(p in results$player){
  pos = append(pos,
               bio_dat$POSITION[bio_dat$PLAYER_NAME == p])
}
results$position = pos
table(results$position)

#clean up multi-position listings
multi_pos = c("PF-C", "PF-SF", "PG-SG", "SF-PF", "SF-SG", "SG-PG")
results[results$position %in% multi_pos,]

#manual changes to positions (ESPN listing)
results$position[results$player == "Matisse Thybulle"] = "SG"
results$position[results$player == "Patrick Beverley"] = "PG"
results$position[results$player == "Kevin Knox II"] = "SF"
results$position[results$player == "Kyrie Irving"] = "PG"
results$position[results$player == "Spencer Dinwiddie"] = "PG"
results$position[results$player == "Mikal Bridges"] = "SF"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "George Hill"] = "PG"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "T.J. Warren"] = "SF"

table(results$position)

#PVGS

#get relative value per position
resultsPG = results[results$position == "PG",]
resultsPG$Z_val = scale(resultsPG$PVGS)

resultsSG = results[results$position == "SG",]
resultsSG$Z_val = scale(resultsSG$PVGS)

resultsSF = results[results$position == "SF",]
resultsSF$Z_val = scale(resultsSF$PVGS)

resultsPF = results[results$position == "PF",]
resultsPF$Z_val = scale(resultsPF$PVGS)

resultsC = results[results$position == "C",]
resultsC$Z_val = scale(resultsC$PVGS)

#top performers based on Z-score values relative to position
results = rbind(resultsPG, resultsSG, resultsSF,
                resultsPF, resultsC)

topX = 5
top_df = subset(results,
                Z_val >= 
                  quantile(Z_val,
                           1 - topX/nrow(results)))
top_df[with(top_df, order(-Z_val)), ]
#max overall
results$player[which.max(results$PVGS)]
#average player
82 * (1 / m_bar)

write.csv(results, './results/PVGS.csv')

################################################################################
#Figure 2
################################################################################
rm(list=ls())

wl = read.csv('./results/2023regseason_WL.csv')$win_logit
ws = read.csv('./results/2023regseason_WS.csv')$game_WS
gs = read.csv('./results/2023regseason_GS.csv')$game_GS


#density plots
plot_df = data.frame("Measure" = c(rep("WinLogit",length(wl)),
                                   rep("Win Score",length(ws)),
                                   rep("Game Score",length(gs))),
                     "Value" = c(wl, ws, gs))


ggplot(plot_df, aes(x = Value, colour = Measure, fill=Measure)) +
  geom_density(alpha = 0.25, linewidth = 0.5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab("Player Game Share") +
  ylab("Frequency (Density)") +
  theme_bw() +
  theme(axis.title.x=element_text(size=9, family="Times New Roman"),
        axis.text.x=element_text(size=9, family="Times New Roman"),
        axis.text.y=element_text(size=9, family="Times New Roman"),
        axis.title.y=element_text(size=9,family="Times New Roman"),
        legend.text=element_text(size=9, family="Times New Roman"),
        strip.text.y = element_text(size = 9, family="Times New Roman"),
        legend.title=element_blank(),
        legend.position=c(.15,.70))

#save, if desired
ggsave("./results/wealth_shape.pdf",height=4,width=6,device = cairo_pdf)


################################################################################
#Table 2
################################################################################
WS = read.csv('./results/PVWS.csv')
WL = read.csv('./results/PVWL.csv')
GS = read.csv('./results/PVGS.csv')

WL$percentile = rank(WL$PVWL) / length(WL$PVWL)
WS$percentile = rank(WS$PVWS) / length(WS$PVWS)
GS$percentile = rank(GS$PVGS) / length(GS$PVGS)

full_dat = merge(WL, WS, by = "player")
full_dat = merge(full_dat, GS, by = "player")

comp_dat = data.frame(player = full_dat$player,
                      WLp = full_dat$percentile.x,
                      WSp = full_dat$percentile.y,
                      GSp = full_dat$percentile)

comp_dat$WL_WS = abs(comp_dat$WLp - comp_dat$WSp)
comp_dat$WL_GS = abs(comp_dat$WLp - comp_dat$GSp)
comp_dat$WS_GS = abs(comp_dat$GSp - comp_dat$WSp)

topX = 10
top_df = subset(comp_dat,
                WL_WS >= 
                  quantile(WL_WS,
                           1 - topX/nrow(comp_dat)))
top_df[with(top_df, order(-WL_WS)), ]

topX = 10
top_df = subset(comp_dat,
                WL_GS >= 
                  quantile(WL_GS,
                           1 - topX/nrow(comp_dat)))
top_df[with(top_df, order(-WL_GS)), ]

topX = 10
top_df = subset(comp_dat,
                WS_GS >= 
                  quantile(WS_GS,
                           1 - topX/nrow(comp_dat)))
top_df[with(top_df, order(-WS_GS)), ]

################################################################################
#Figure 3
################################################################################
rm(list=ls())
source('./code/total_contrib_WL.R')
source('./code/plot_games_WL.R')

WL = read.csv('./results/PVWL.csv')
WL = WL[WL$position == "PF",]
topX = 25
top_df = subset(WL,
                PVWL >= 
                  quantile(PVWL,
                           1 - topX/nrow(WL)))
top_df[with(top_df, order(-PVWL)), ]
#Durant, Eason good example

df = read.csv('./results/2023regseason_WL.csv')

g_durant = plot_games_WL("Kevin Durant")
g_eason = plot_games_WL("Tari Eason")

plot_grid(g_durant, g_eason, nrow=2)

#save, if desired
ggsave("./results/WL_comp.pdf",height=4,width=6,device = cairo_pdf)

################################################################################
################################################################################
################################################################################
################################################################################
# Section 3.3
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
#Table 3
################################################################################
rm(list=ls())

#list of TV games
path = "./clean_data"
game_tv = read.csv(paste(path,"game_tv.csv",sep="/"))

#links to references used in manuscript
#tv deal:
#https://www.sportingnews.com/us/nba/news/nba-espn-tnt-tv-deal-adam-silver-lebron-james-media-agreement/tr4vopgnuw72zkuxz8jsdrce
#https://www.sportsmediawatch.com/2014/10/nba-tv-deal-espn-abc-tnt-nine-year-deal-2025-24-billion-lockout/

#gate sales 21.57%
#https://www.statista.com/statistics/193410/percentage-of-ticketing-revenue-in-the-nba-since-2006/

#total revenue $10.58B
#https://www.statista.com/statistics/193467/total-league-revenue-of-the-nba-since-2005/

#sponsorship $1.66B
#https://www.statista.com/statistics/380270/nba-sponsorship-revenue/

#make units easier to work with
B = 1000000000

#tv & ad money
tv_games = sum( (game_tv$tv != "") )
sponsor = (1.66 * B) / tv_games

espn_games = sum( (game_tv$tv == "ESPN")|(game_tv$tv == "ESPN2")|
                    (game_tv$tv == "ABC")|(game_tv$tv == "ABC/ESPN") )
espn = (1.4 * B) / espn_games

tnt_games = sum( (game_tv$tv == "TNT") )
tnt = (1.2 * B) / tnt_games

game_ids = unique(game_tv$game_id)
game_val = c()

for(g in game_ids){
  cur_game_val = 0
  
  cur_tv = game_tv$tv[game_tv$game_id == g]
  
  if( (cur_tv == "ESPN")|(cur_tv == "ESPN2")|
      (cur_tv == "ABC")|(cur_tv == "ABC/ESPN") ){
    cur_game_val = espn + sponsor
  }
  
  if( (cur_tv == "TNT") ){
    cur_game_val = tnt + sponsor
  }
  
  if( (cur_tv == "NBA TV") ){
    cur_game_val = sponsor
  }
  
  game_val = append(game_val, cur_game_val)
}

tv = data.frame("GAME_ID" = game_ids,
                "TV_AD_REV" = game_val)

#attendance
path = "./clean_data"
game_attend = read.csv(paste(path,"attendance.csv",sep="/"))

#add missing data
game_attend$game_id[ which( is.na(game_attend$attendance) ) ]

#https://www.espn.com/nba/boxscore/_/gameId/401468211
game_attend$attendance[game_attend$game_id == 22200056] = 19432

#https://www.espn.com/nba/boxscore/_/gameId/401469246
game_attend$attendance[game_attend$game_id == 22201091] = 18206

game_attend = data.frame("GAME_ID" = game_attend$game_id,
                         "ATTEND" = game_attend$attendance)

attendance = sum(game_attend$ATTEND)
ticket = 10.58 * (B) * 0.2157 / (attendance)

game_attend$GATE = ticket * game_attend$ATTEND

game_attend = data.frame("GAME_ID" = game_attend$GAME_ID,
                         "GATE" = game_attend$GATE)

sgv = merge(tv, game_attend, by = "GAME_ID")

sgv$TOTAL = sgv$TV_AD_REV + sgv$GATE

write.csv(sgv, './results/single_game_val.csv')

#c(alpha1, alpha2, alpha3, alpha4)
alpha = c(ticket, espn, tnt, sponsor)

data.frame("parameter" = c("a1", "a2", "a3", "a4"),
           "description" = c("Ticket", "ESPN", "TNT", "Ad Rev"),
           "estimate" = format(round(alpha,2), big.mark=",", scientific=F, trim=T))

#top 5 teams
game_info = read.csv('./raw_data/2023regseason.csv')

T1 = c()
T2 = c()
for(g in sgv$GAME_ID){
  teams = sort(unique(game_info$TEAM_ABBREVIATION[game_info$GAME_ID == g]))
  T1 = append(T1, teams[1])
  T2 = append(T2, teams[2])
}

sgv$T1 = T1; sgv$T2 = T2

teams = unique(c(unique(sgv$T1), unique(sgv$T2)))

res = matrix(NA, nrow = 30, ncol = 1)
rownames(res) = teams

for(t in teams){
  res[t,] = sum( sgv$TOTAL[(sgv$T1 == t | sgv$T2 == t)] )
}

res[order(res[,1], decreasing = TRUE),][1:5]/1000000

################################################################################
#ROI Calculations
################################################################################

#winlogit
################################################################################
rm(list=ls())
df = read.csv('./results/2023regseason_WL.csv')

source('./code/total_contrib_WL_ROI.R')
source('./code/NBA_IRR_WL.R')

#get salary data
path = "./clean_data"
file = "22.23_player_salary.csv"

sal_dat = read.csv(paste(path,file,sep="/"))
names(sal_dat) = c("PLAYER_NAME", "SALARY")

sal_dat$SALARY = as.numeric(gsub('[$,]', '', sal_dat$SALARY))

#get same spellings
ctrb_dat = data.frame("PLAYER_NAME" = sort(unique(df$PLAYER_NAME)))

#prepare cleaned file; need to connect by names
sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])

#clean names
sal_spelling = c("AJ Lawson", "BJ Boston", "Danuel House", "David Duke",
                 "Dennis Schroeder", "Dennis Smith","Derrick Jones",
                 "Devonte Graham", "DJ Augustin", "DJ Steward", "DJ Wilson",
                 "Duane Washington Jr", "EJ Liddell", "Gary Trent Jr",
                 "Greg Brown", "Herb Jones", "Ishmael Smith", "Ishmail Wainright",
                 "Jabari Smith", "Jaren Jackson Jr",
                 "Jeenathan Williams", "Jeff Dowtin", "John Butler",
                 "Josh Primo", "Juan Hernangomez", "Kelly Oubre", "Kenneth Lofton",
                 "Kevin Knox", "Kevin Porter", "Kira Lewis", "Kenyon Martin Jr.",
                 'Larry Nance Jr',
                 'Lonnie Walker',
                 'Marcus Morris',
                 'Marvin Bagley',
                 'McKinley Wright',
                 'Michael Foster',
                 'Michael Porter',
                 'Nicolas Claxton',
                 'O.G. Anunoby',
                 'Otto Porter',
                 'Patrick Baldwin',
                 'Patrick Mills',
                 'PJ Tucker',
                 'PJ Washington',
                 'Reggie Bullock',
                 'RJ Hampton',
                 'Robert Williams',
                 'Ron Harper Jr',
                 'Santiago Aldama',
                 'Scottie Pippen Jr',
                 'Sviatoslav Mykhailiuk',
                 'Tim Hardaway Jr',
                 'TJ McConnell',
                 'TJ Warren',
                 'Trey Murphy',
                 'Troy Brown',
                 'TyTy Washington',
                 'Vernon Carey',
                 'Vince Williams Jr',
                 'Wendell Carter',
                 'Wendell Moore')
nba_spelling = c("A.J. Lawson", "Brandon Boston Jr.", "Danuel House Jr.",
                 "David Duke Jr.", "Dennis Schroder", "Dennis Smith Jr.",
                 "Derrick Jones Jr.", "Devonte' Graham", "D.J. Augustin",
                 "D.J. Steward", "D.J. Wilson", "Duane Washington Jr.",
                 "E.J. Liddell", "Gary Trent Jr.", "Greg Brown III",
                 "Herbert Jones", "Ish Smith", "Ish Wainright", "Jabari Smith Jr.",
                 "Jaren Jackson Jr.",
                 "Nate Williams",
                 "Jeff Dowtin Jr.", "John Butler Jr.",
                 "Joshua Primo", "Juancho Hernangomez", "Kelly Oubre Jr.",
                 "Kenneth Lofton Jr.", "Kevin Knox II", "Kevin Porter Jr.",
                 "Kira Lewis Jr.", "KJ Martin",
                 'Larry Nance Jr.',
                 'Lonnie Walker IV',
                 'Marcus Morris Sr.',
                 'Marvin Bagley III',
                 'McKinley Wright IV',
                 'Michael Foster Jr.',
                 'Michael Porter Jr.',
                 'Nic Claxton',
                 'OG Anunoby',
                 'Otto Porter Jr.',
                 'Patrick Baldwin Jr.',
                 'Patty Mills',
                 'P.J. Tucker',
                 'P.J. Washington',
                 "Reggie Bullock Jr.",
                 'R.J. Hampton',
                 'Robert Williams III',
                 'Ron Harper Jr.',
                 'Santi Aldama',
                 'Scotty Pippen Jr.',
                 'Svi Mykhailiuk',
                 'Tim Hardaway Jr.',
                 'T.J. McConnell',
                 'T.J. Warren',
                 'Trey Murphy III',
                 'Troy Brown Jr.',
                 'TyTy Washington Jr.',
                 'Vernon Carey Jr.',
                 'Vince Williams Jr.',
                 'Wendell Carter Jr.',
                 'Wendell Moore Jr.')

k = 1
for(n in sal_spelling){
  
  row = which(sal_dat$PLAYER_NAME == n)
  sal_dat$PLAYER_NAME[row] = nba_spelling[k]
  k = k + 1
  
}

#add missing salary info
add_sal = data.frame("PLAYER_NAME" = c("Chance Comanche"),
                     "SALARY" = c(5849))
sal_dat = rbind(sal_dat, add_sal)

#Chance Comanche: https://www.spotrac.com/nba/portland-trail-blazers/chance-comanche-82139/

no_PT = c("Andrew Nicholson", "Armoni Brooks", "Chet Holmgren", "Collin Gillespie",
          "Danilo Gallinari", "David Nwaba", "Demetrius Jackson", "Didi Louzada",
          "D.J. Steward", "D.J. Wilson", "Eric Bledsoe", "Justin Lewis",
          "Juwan Morgan", "Keaton Wallace", "Killian Tillie", "Kyle Singler",
          "Lonzo Ball", "Malik Fitts", "Marquese Chriss", "Moe Harkless",
          "Nate Darling", "Nik Stauskas", "Robert Woodard", "Trey Burke",
          "Tristan Thompson", "Troy Williams", "Tyrell Terry", "Zhaire Smith",
          "D.J. Augustin", "DaQuan Jeffries", "Derrick Favors", "E.J. Liddell",      
          "Feron Hunt", "Ibou Badji", "Kostas Antetokounmpo","Willie Cauley-Stein")

sal_dat = subset(sal_dat, !(PLAYER_NAME %in% no_PT))

#check, should be double NULL
sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])

sgvs = read.csv('./results/single_game_val.csv')
sgvs = sgvs[,-c(1)]

#sgvs2 removes the 4 no tracking data games
no_track = c(22200635, 22200678, 22201199, 22201214)
sgvs2 = sgvs[!(sgvs$GAME_ID %in% no_track),]
sum(mean(sgvs2$TOTAL) * df$win_logit)
sum(sgvs2$TOTAL)
#this is a basic check of Theorem 3.1

#set minimum games played
MIN_GAME = 42

all_players = unique(df$PLAYER_NAME)

start.time <- Sys.time()
ROIs = sapply(all_players, NBA_IRR_WL, min_games = MIN_GAME)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

results = data.frame("player" = all_players,
                     "ROI" = ROIs)

results = results[results$ROI != "Minimum games not met!",]
results = results[!is.na(results$ROI),]

s = c()
for(p in results$player){
  s = append(s, sal_dat$SALARY[sal_dat$PLAYER_NAME == p])
}
results$salary = s

gp = c()
for(p in results$player){
  g = sum( total_contrib_WL_ROI(p)$game_totals != 0)
  gp = append(gp, g)
}
results$gp = gp

pvwl = c()
for(p in results$player){
  pvwl = append(pvwl, total_contrib_WL_ROI(p)$sum_total)
}
results$PVWL = pvwl
results$ROI = as.numeric(results$ROI)

row.names(results) = NULL

#load position data
path = "./clean_data"
file = "player_positions.csv"

bio_dat = read.csv(paste(path,file,sep="/"))
names(bio_dat) = c("PLAYER_NAME", "POSITION")

#check all players available
results$player[!(results$player %in% bio_dat$PLAYER_NAME)]

pos = c()
for(p in results$player){
  pos = append(pos,
               bio_dat$POSITION[bio_dat$PLAYER_NAME == p])
}
results$position = pos
table(results$position)

#clean up multi-position listings
multi_pos = c("PF-C", "PF-SF", "PG-SG", "SF-PF", "SF-SG", "SG-PG")
results[results$position %in% multi_pos,]

#manual changes to positions (ESPN listing)
results$position[results$player == "Matisse Thybulle"] = "SG"
results$position[results$player == "Patrick Beverley"] = "PG"
results$position[results$player == "Kevin Knox II"] = "SF"
results$position[results$player == "Kyrie Irving"] = "PG"
results$position[results$player == "Spencer Dinwiddie"] = "PG"
results$position[results$player == "Mikal Bridges"] = "SF"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "George Hill"] = "PG"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "T.J. Warren"] = "SF"

table(results$position)

#get relative value per position
resultsPG = results[results$position == "PG",]
resultsPG$Z_val = scale(resultsPG$ROI)

resultsSG = results[results$position == "SG",]
resultsSG$Z_val = scale(resultsSG$ROI)

resultsSF = results[results$position == "SF",]
resultsSF$Z_val = scale(resultsSF$ROI)

resultsPF = results[results$position == "PF",]
resultsPF$Z_val = scale(resultsPF$ROI)

resultsC = results[results$position == "C",]
resultsC$Z_val = scale(resultsC$ROI)

#top performers based on Z-score values relative to position
results = rbind(resultsPG, resultsSG, resultsSF,
                resultsPF, resultsC)

topX = 5
top_df = subset(results,
                Z_val >= 
                  quantile(Z_val,
                           1 - topX/nrow(results)))
top_df[with(top_df, order(-Z_val)), ]
#max overall
results$player[which.max(results$ROI)]

write.csv(results, './results/WL_ROI.csv')

#table 4
pos_mean = aggregate(results$ROI,by=list(results$position), FUN=mean)
pos_sd = aggregate(results$ROI,by=list(results$position), FUN=sd)
position_CV = pos_sd$x / pos_mean$x

data.frame("position" = pos_mean$Group.1,
           "CV" = position_CV)
rm(list=ls())

################################################################################
#win score
################################################################################
source('./code/total_contrib_WS_ROI.R')
source('./code/NBA_IRR_WS.R')

df = read.csv('./results/2023regseason_WS.csv')

#get salary data
path = "./clean_data"
file = "22.23_player_salary.csv"

sal_dat = read.csv(paste(path,file,sep="/"))
names(sal_dat) = c("PLAYER_NAME", "SALARY")

sal_dat$SALARY = as.numeric(gsub('[$,]', '', sal_dat$SALARY))

#get same spellings
ctrb_dat = data.frame("PLAYER_NAME" = sort(unique(df$PLAYER_NAME)))

#prepare cleaned file; need to connect by names
sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])

#clean names
sal_spelling = c("AJ Lawson", "BJ Boston", "Danuel House", "David Duke",
                 "Dennis Schroeder", "Dennis Smith","Derrick Jones",
                 "Devonte Graham", "DJ Augustin", "DJ Steward", "DJ Wilson",
                 "Duane Washington Jr", "EJ Liddell", "Gary Trent Jr",
                 "Greg Brown", "Herb Jones", "Ishmael Smith", "Ishmail Wainright",
                 "Jabari Smith", "Jaren Jackson Jr",
                 "Jeenathan Williams", "Jeff Dowtin", "John Butler",
                 "Josh Primo", "Juan Hernangomez", "Kelly Oubre", "Kenneth Lofton",
                 "Kevin Knox", "Kevin Porter", "Kira Lewis", "Kenyon Martin Jr.",
                 'Larry Nance Jr',
                 'Lonnie Walker',
                 'Marcus Morris',
                 'Marvin Bagley',
                 'McKinley Wright',
                 'Michael Foster',
                 'Michael Porter',
                 'Nicolas Claxton',
                 'O.G. Anunoby',
                 'Otto Porter',
                 'Patrick Baldwin',
                 'Patrick Mills',
                 'PJ Tucker',
                 'PJ Washington',
                 'Reggie Bullock',
                 'RJ Hampton',
                 'Robert Williams',
                 'Ron Harper Jr',
                 'Santiago Aldama',
                 'Scottie Pippen Jr',
                 'Sviatoslav Mykhailiuk',
                 'Tim Hardaway Jr',
                 'TJ McConnell',
                 'TJ Warren',
                 'Trey Murphy',
                 'Troy Brown',
                 'TyTy Washington',
                 'Vernon Carey',
                 'Vince Williams Jr',
                 'Wendell Carter',
                 'Wendell Moore')
nba_spelling = c("A.J. Lawson", "Brandon Boston Jr.", "Danuel House Jr.",
                 "David Duke Jr.", "Dennis Schroder", "Dennis Smith Jr.",
                 "Derrick Jones Jr.", "Devonte' Graham", "D.J. Augustin",
                 "D.J. Steward", "D.J. Wilson", "Duane Washington Jr.",
                 "E.J. Liddell", "Gary Trent Jr.", "Greg Brown III",
                 "Herbert Jones", "Ish Smith", "Ish Wainright", "Jabari Smith Jr.",
                 "Jaren Jackson Jr.",
                 "Nate Williams",
                 "Jeff Dowtin Jr.", "John Butler Jr.",
                 "Joshua Primo", "Juancho Hernangomez", "Kelly Oubre Jr.",
                 "Kenneth Lofton Jr.", "Kevin Knox II", "Kevin Porter Jr.",
                 "Kira Lewis Jr.", "KJ Martin",
                 'Larry Nance Jr.',
                 'Lonnie Walker IV',
                 'Marcus Morris Sr.',
                 'Marvin Bagley III',
                 'McKinley Wright IV',
                 'Michael Foster Jr.',
                 'Michael Porter Jr.',
                 'Nic Claxton',
                 'OG Anunoby',
                 'Otto Porter Jr.',
                 'Patrick Baldwin Jr.',
                 'Patty Mills',
                 'P.J. Tucker',
                 'P.J. Washington',
                 "Reggie Bullock Jr.",
                 'R.J. Hampton',
                 'Robert Williams III',
                 'Ron Harper Jr.',
                 'Santi Aldama',
                 'Scotty Pippen Jr.',
                 'Svi Mykhailiuk',
                 'Tim Hardaway Jr.',
                 'T.J. McConnell',
                 'T.J. Warren',
                 'Trey Murphy III',
                 'Troy Brown Jr.',
                 'TyTy Washington Jr.',
                 'Vernon Carey Jr.',
                 'Vince Williams Jr.',
                 'Wendell Carter Jr.',
                 'Wendell Moore Jr.')

k = 1
for(n in sal_spelling){
  
  row = which(sal_dat$PLAYER_NAME == n)
  sal_dat$PLAYER_NAME[row] = nba_spelling[k]
  k = k + 1
  
}

#add missing salary info
add_sal = data.frame("PLAYER_NAME" = c("Chance Comanche"),
                     "SALARY" = c(5849))
sal_dat = rbind(sal_dat, add_sal)

#Chance Comanche: https://www.spotrac.com/nba/portland-trail-blazers/chance-comanche-82139/

no_PT = c("Andrew Nicholson", "Armoni Brooks", "Chet Holmgren", "Collin Gillespie",
          "Danilo Gallinari", "David Nwaba", "Demetrius Jackson", "Didi Louzada",
          "D.J. Steward", "D.J. Wilson", "Eric Bledsoe", "Justin Lewis",
          "Juwan Morgan", "Keaton Wallace", "Killian Tillie", "Kyle Singler",
          "Lonzo Ball", "Malik Fitts", "Marquese Chriss", "Moe Harkless",
          "Nate Darling", "Nik Stauskas", "Robert Woodard", "Trey Burke",
          "Tristan Thompson", "Troy Williams", "Tyrell Terry", "Zhaire Smith",
          "D.J. Augustin", "DaQuan Jeffries", "Derrick Favors", "E.J. Liddell",      
          "Feron Hunt", "Ibou Badji", "Kostas Antetokounmpo","Willie Cauley-Stein")

sal_dat = subset(sal_dat, !(PLAYER_NAME %in% no_PT))

#check, should be double NULL
sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])

sgvs = read.csv('./results/single_game_val.csv')
sgvs = sgvs[,-c(1)]

#sgvs2 removes the 4 no tracking data games
no_track = c(22200635, 22200678, 22201199, 22201214)
sgvs2 = sgvs[!(sgvs$GAME_ID %in% no_track),]
sum(mean(sgvs2$TOTAL) * df$game_WS)
sum(sgvs2$TOTAL)
#this is a basic check of Theorem 3.1

#set minimum games played
MIN_GAME = 42

all_players = unique(df$PLAYER_NAME)

start.time <- Sys.time()
ROIs = sapply(all_players, NBA_IRR_WS, min_games = MIN_GAME)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

results = data.frame("player" = all_players,
                     "ROI" = ROIs)

results = results[results$ROI != "Minimum games not met!",]
results = results[!is.na(results$ROI),]

s = c()
for(p in results$player){
  s = append(s, sal_dat$SALARY[sal_dat$PLAYER_NAME == p])
}
results$salary = s

gp = c()
for(p in results$player){
  g = sum( total_contrib_WS_ROI(p)$game_totals != 0)
  gp = append(gp, g)
}
results$gp = gp

pvws = c()
for(p in results$player){
  pvws = append(pvws, total_contrib_WS_ROI(p)$sum_total)
}
results$PVWS = pvws
results$ROI = as.numeric(results$ROI)

row.names(results) = NULL

#load position data
path = "./clean_data"
file = "player_positions.csv"

bio_dat = read.csv(paste(path,file,sep="/"))
names(bio_dat) = c("PLAYER_NAME", "POSITION")

#check all players available
results$player[!(results$player %in% bio_dat$PLAYER_NAME)]

pos = c()
for(p in results$player){
  pos = append(pos,
               bio_dat$POSITION[bio_dat$PLAYER_NAME == p])
}
results$position = pos
table(results$position)

#clean up multi-position listings
multi_pos = c("PF-C", "PF-SF", "PG-SG", "SF-PF", "SF-SG", "SG-PG")
results[results$position %in% multi_pos,]

#manual changes to positions (ESPN listing)
results$position[results$player == "Matisse Thybulle"] = "SG"
results$position[results$player == "Patrick Beverley"] = "PG"
results$position[results$player == "Kevin Knox II"] = "SF"
results$position[results$player == "Kyrie Irving"] = "PG"
results$position[results$player == "Spencer Dinwiddie"] = "PG"
results$position[results$player == "Mikal Bridges"] = "SF"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "George Hill"] = "PG"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "T.J. Warren"] = "SF"

table(results$position)

#get relative value per position
resultsPG = results[results$position == "PG",]
resultsPG$Z_val = scale(resultsPG$ROI)

resultsSG = results[results$position == "SG",]
resultsSG$Z_val = scale(resultsSG$ROI)

resultsSF = results[results$position == "SF",]
resultsSF$Z_val = scale(resultsSF$ROI)

resultsPF = results[results$position == "PF",]
resultsPF$Z_val = scale(resultsPF$ROI)

resultsC = results[results$position == "C",]
resultsC$Z_val = scale(resultsC$ROI)

#top performers based on Z-score values relative to position
results = rbind(resultsPG, resultsSG, resultsSF,
                resultsPF, resultsC)

topX = 5
top_df = subset(results,
                Z_val >= 
                  quantile(Z_val,
                           1 - topX/nrow(results)))
top_df[with(top_df, order(-Z_val)), ]
#max overall
results$player[which.max(results$ROI)]

write.csv(results, './results/WS_ROI.csv')

#table 4
pos_mean = aggregate(results$ROI,by=list(results$position), FUN=mean)
pos_sd = aggregate(results$ROI,by=list(results$position), FUN=sd)
position_CV = pos_sd$x / pos_mean$x

data.frame("position" = pos_mean$Group.1,
           "CV" = position_CV)

rm(list=ls())

################################################################################
#game score
################################################################################
source('./code/total_contrib_GS_ROI.R')
source('./code/NBA_IRR_GS.R')

df = read.csv('./results/2023regseason_GS.csv')

#get salary data
path = "./clean_data"
file = "22.23_player_salary.csv"

sal_dat = read.csv(paste(path,file,sep="/"))
names(sal_dat) = c("PLAYER_NAME", "SALARY")

sal_dat$SALARY = as.numeric(gsub('[$,]', '', sal_dat$SALARY))

#get same spellings
ctrb_dat = data.frame("PLAYER_NAME" = sort(unique(df$PLAYER_NAME)))

#prepare cleaned file; need to connect by names
sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])

#clean names
sal_spelling = c("AJ Lawson", "BJ Boston", "Danuel House", "David Duke",
                 "Dennis Schroeder", "Dennis Smith","Derrick Jones",
                 "Devonte Graham", "DJ Augustin", "DJ Steward", "DJ Wilson",
                 "Duane Washington Jr", "EJ Liddell", "Gary Trent Jr",
                 "Greg Brown", "Herb Jones", "Ishmael Smith", "Ishmail Wainright",
                 "Jabari Smith", "Jaren Jackson Jr",
                 "Jeenathan Williams", "Jeff Dowtin", "John Butler",
                 "Josh Primo", "Juan Hernangomez", "Kelly Oubre", "Kenneth Lofton",
                 "Kevin Knox", "Kevin Porter", "Kira Lewis", "Kenyon Martin Jr.",
                 'Larry Nance Jr',
                 'Lonnie Walker',
                 'Marcus Morris',
                 'Marvin Bagley',
                 'McKinley Wright',
                 'Michael Foster',
                 'Michael Porter',
                 'Nicolas Claxton',
                 'O.G. Anunoby',
                 'Otto Porter',
                 'Patrick Baldwin',
                 'Patrick Mills',
                 'PJ Tucker',
                 'PJ Washington',
                 'Reggie Bullock',
                 'RJ Hampton',
                 'Robert Williams',
                 'Ron Harper Jr',
                 'Santiago Aldama',
                 'Scottie Pippen Jr',
                 'Sviatoslav Mykhailiuk',
                 'Tim Hardaway Jr',
                 'TJ McConnell',
                 'TJ Warren',
                 'Trey Murphy',
                 'Troy Brown',
                 'TyTy Washington',
                 'Vernon Carey',
                 'Vince Williams Jr',
                 'Wendell Carter',
                 'Wendell Moore')
nba_spelling = c("A.J. Lawson", "Brandon Boston Jr.", "Danuel House Jr.",
                 "David Duke Jr.", "Dennis Schroder", "Dennis Smith Jr.",
                 "Derrick Jones Jr.", "Devonte' Graham", "D.J. Augustin",
                 "D.J. Steward", "D.J. Wilson", "Duane Washington Jr.",
                 "E.J. Liddell", "Gary Trent Jr.", "Greg Brown III",
                 "Herbert Jones", "Ish Smith", "Ish Wainright", "Jabari Smith Jr.",
                 "Jaren Jackson Jr.",
                 "Nate Williams",
                 "Jeff Dowtin Jr.", "John Butler Jr.",
                 "Joshua Primo", "Juancho Hernangomez", "Kelly Oubre Jr.",
                 "Kenneth Lofton Jr.", "Kevin Knox II", "Kevin Porter Jr.",
                 "Kira Lewis Jr.", "KJ Martin",
                 'Larry Nance Jr.',
                 'Lonnie Walker IV',
                 'Marcus Morris Sr.',
                 'Marvin Bagley III',
                 'McKinley Wright IV',
                 'Michael Foster Jr.',
                 'Michael Porter Jr.',
                 'Nic Claxton',
                 'OG Anunoby',
                 'Otto Porter Jr.',
                 'Patrick Baldwin Jr.',
                 'Patty Mills',
                 'P.J. Tucker',
                 'P.J. Washington',
                 "Reggie Bullock Jr.",
                 'R.J. Hampton',
                 'Robert Williams III',
                 'Ron Harper Jr.',
                 'Santi Aldama',
                 'Scotty Pippen Jr.',
                 'Svi Mykhailiuk',
                 'Tim Hardaway Jr.',
                 'T.J. McConnell',
                 'T.J. Warren',
                 'Trey Murphy III',
                 'Troy Brown Jr.',
                 'TyTy Washington Jr.',
                 'Vernon Carey Jr.',
                 'Vince Williams Jr.',
                 'Wendell Carter Jr.',
                 'Wendell Moore Jr.')

k = 1
for(n in sal_spelling){
  
  row = which(sal_dat$PLAYER_NAME == n)
  sal_dat$PLAYER_NAME[row] = nba_spelling[k]
  k = k + 1
  
}

#add missing salary info
add_sal = data.frame("PLAYER_NAME" = c("Chance Comanche"),
                     "SALARY" = c(5849))
sal_dat = rbind(sal_dat, add_sal)

#Chance Comanche: https://www.spotrac.com/nba/portland-trail-blazers/chance-comanche-82139/

no_PT = c("Andrew Nicholson", "Armoni Brooks", "Chet Holmgren", "Collin Gillespie",
          "Danilo Gallinari", "David Nwaba", "Demetrius Jackson", "Didi Louzada",
          "D.J. Steward", "D.J. Wilson", "Eric Bledsoe", "Justin Lewis",
          "Juwan Morgan", "Keaton Wallace", "Killian Tillie", "Kyle Singler",
          "Lonzo Ball", "Malik Fitts", "Marquese Chriss", "Moe Harkless",
          "Nate Darling", "Nik Stauskas", "Robert Woodard", "Trey Burke",
          "Tristan Thompson", "Troy Williams", "Tyrell Terry", "Zhaire Smith",
          "D.J. Augustin", "DaQuan Jeffries", "Derrick Favors", "E.J. Liddell",      
          "Feron Hunt", "Ibou Badji", "Kostas Antetokounmpo","Willie Cauley-Stein")

sal_dat = subset(sal_dat, !(PLAYER_NAME %in% no_PT))

#check, should be double NULL
sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])

sgvs = read.csv('./results/single_game_val.csv')
sgvs = sgvs[,-c(1)]

#sgvs2 removes the 4 no tracking data games
no_track = c(22200635, 22200678, 22201199, 22201214)
sgvs2 = sgvs[!(sgvs$GAME_ID %in% no_track),]
sum(mean(sgvs2$TOTAL) * df$game_GS)
sum(sgvs2$TOTAL)
#this is a basic check of Theorem 3.1

#set minimum games played
MIN_GAME = 42

all_players = unique(df$PLAYER_NAME)

start.time <- Sys.time()
ROIs = sapply(all_players, NBA_IRR_GS, min_games = MIN_GAME)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

results = data.frame("player" = all_players,
                     "ROI" = ROIs)

results = results[results$ROI != "Minimum games not met!",]
results = results[!is.na(results$ROI),]

s = c()
for(p in results$player){
  s = append(s, sal_dat$SALARY[sal_dat$PLAYER_NAME == p])
}
results$salary = s

gp = c()
for(p in results$player){
  g = sum( total_contrib_GS_ROI(p)$game_totals != 0)
  gp = append(gp, g)
}
results$gp = gp

pvgs = c()
for(p in results$player){
  pvgs = append(pvgs, total_contrib_GS_ROI(p)$sum_total)
}
results$PVGS = pvgs
results$ROI = as.numeric(results$ROI)

row.names(results) = NULL

#load position data
path = "./clean_data"
file = "player_positions.csv"

bio_dat = read.csv(paste(path,file,sep="/"))
names(bio_dat) = c("PLAYER_NAME", "POSITION")

#check all players available
results$player[!(results$player %in% bio_dat$PLAYER_NAME)]

pos = c()
for(p in results$player){
  pos = append(pos,
               bio_dat$POSITION[bio_dat$PLAYER_NAME == p])
}
results$position = pos
table(results$position)

#clean up multi-position listings
multi_pos = c("PF-C", "PF-SF", "PG-SG", "SF-PF", "SF-SG", "SG-PG")
results[results$position %in% multi_pos,]

#manual changes to positions (ESPN listing)
results$position[results$player == "Matisse Thybulle"] = "SG"
results$position[results$player == "Patrick Beverley"] = "PG"
results$position[results$player == "Kevin Knox II"] = "SF"
results$position[results$player == "Kyrie Irving"] = "PG"
results$position[results$player == "Spencer Dinwiddie"] = "PG"
results$position[results$player == "Mikal Bridges"] = "SF"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "George Hill"] = "PG"
results$position[results$player == "Dario Saric"] = "PF"
results$position[results$player == "T.J. Warren"] = "SF"

table(results$position)

#get relative value per position
resultsPG = results[results$position == "PG",]
resultsPG$Z_val = scale(resultsPG$ROI)

resultsSG = results[results$position == "SG",]
resultsSG$Z_val = scale(resultsSG$ROI)

resultsSF = results[results$position == "SF",]
resultsSF$Z_val = scale(resultsSF$ROI)

resultsPF = results[results$position == "PF",]
resultsPF$Z_val = scale(resultsPF$ROI)

resultsC = results[results$position == "C",]
resultsC$Z_val = scale(resultsC$ROI)

#top performers based on Z-score values relative to position
results = rbind(resultsPG, resultsSG, resultsSF,
                resultsPF, resultsC)

topX = 5
top_df = subset(results,
                Z_val >= 
                  quantile(Z_val,
                           1 - topX/nrow(results)))
top_df[with(top_df, order(-Z_val)), ]
#max overall
results$player[which.max(results$ROI)]

write.csv(results, './results/GS_ROI.csv')

#table 4
pos_mean = aggregate(results$ROI,by=list(results$position), FUN=mean)
pos_sd = aggregate(results$ROI,by=list(results$position), FUN=sd)
position_CV = pos_sd$x / pos_mean$x

data.frame("position" = pos_mean$Group.1,
           "CV" = position_CV)

rm(list=ls())

#average/replacement player ROI
df = read.csv('./results/2023regseason_WL.csv')
m_bar = nrow(df) / length(unique(df$GAME_ID))

avg_game = 1 / m_bar

sgvs = read.csv('./results/single_game_val.csv')
sgvs = sgvs[,-c(1)]

avg_sgv = mean(sgvs$TOTAL)

path = "./clean_data"
file = "22.23_player_salary.csv"
sal_dat = read.csv(paste(path,file,sep="/"))

names(sal_dat) = c("PLAYER_NAME", "SALARY")

sal_dat$SALARY = as.numeric(gsub('[$,]', '', sal_dat$SALARY))

#get same spellings
ctrb_dat = data.frame("PLAYER_NAME" = sort(unique(df$PLAYER_NAME)))

#prepare cleaned file; need to connect by names
sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])

#clean names
sal_spelling = c("AJ Lawson", "BJ Boston", "Danuel House", "David Duke",
                 "Dennis Schroeder", "Dennis Smith","Derrick Jones",
                 "Devonte Graham", "DJ Augustin", "DJ Steward", "DJ Wilson",
                 "Duane Washington Jr", "EJ Liddell", "Gary Trent Jr",
                 "Greg Brown", "Herb Jones", "Ishmael Smith", "Ishmail Wainright",
                 "Jabari Smith", "Jaren Jackson Jr",
                 "Jeenathan Williams", "Jeff Dowtin", "John Butler",
                 "Josh Primo", "Juan Hernangomez", "Kelly Oubre", "Kenneth Lofton",
                 "Kevin Knox", "Kevin Porter", "Kira Lewis", "Kenyon Martin Jr.",
                 'Larry Nance Jr',
                 'Lonnie Walker',
                 'Marcus Morris',
                 'Marvin Bagley',
                 'McKinley Wright',
                 'Michael Foster',
                 'Michael Porter',
                 'Nicolas Claxton',
                 'O.G. Anunoby',
                 'Otto Porter',
                 'Patrick Baldwin',
                 'Patrick Mills',
                 'PJ Tucker',
                 'PJ Washington',
                 'Reggie Bullock',
                 'RJ Hampton',
                 'Robert Williams',
                 'Ron Harper Jr',
                 'Santiago Aldama',
                 'Scottie Pippen Jr',
                 'Sviatoslav Mykhailiuk',
                 'Tim Hardaway Jr',
                 'TJ McConnell',
                 'TJ Warren',
                 'Trey Murphy',
                 'Troy Brown',
                 'TyTy Washington',
                 'Vernon Carey',
                 'Vince Williams Jr',
                 'Wendell Carter',
                 'Wendell Moore')
nba_spelling = c("A.J. Lawson", "Brandon Boston Jr.", "Danuel House Jr.",
                 "David Duke Jr.", "Dennis Schroder", "Dennis Smith Jr.",
                 "Derrick Jones Jr.", "Devonte' Graham", "D.J. Augustin",
                 "D.J. Steward", "D.J. Wilson", "Duane Washington Jr.",
                 "E.J. Liddell", "Gary Trent Jr.", "Greg Brown III",
                 "Herbert Jones", "Ish Smith", "Ish Wainright", "Jabari Smith Jr.",
                 "Jaren Jackson Jr.",
                 "Nate Williams",
                 "Jeff Dowtin Jr.", "John Butler Jr.",
                 "Joshua Primo", "Juancho Hernangomez", "Kelly Oubre Jr.",
                 "Kenneth Lofton Jr.", "Kevin Knox II", "Kevin Porter Jr.",
                 "Kira Lewis Jr.", "KJ Martin",
                 'Larry Nance Jr.',
                 'Lonnie Walker IV',
                 'Marcus Morris Sr.',
                 'Marvin Bagley III',
                 'McKinley Wright IV',
                 'Michael Foster Jr.',
                 'Michael Porter Jr.',
                 'Nic Claxton',
                 'OG Anunoby',
                 'Otto Porter Jr.',
                 'Patrick Baldwin Jr.',
                 'Patty Mills',
                 'P.J. Tucker',
                 'P.J. Washington',
                 "Reggie Bullock Jr.",
                 'R.J. Hampton',
                 'Robert Williams III',
                 'Ron Harper Jr.',
                 'Santi Aldama',
                 'Scotty Pippen Jr.',
                 'Svi Mykhailiuk',
                 'Tim Hardaway Jr.',
                 'T.J. McConnell',
                 'T.J. Warren',
                 'Trey Murphy III',
                 'Troy Brown Jr.',
                 'TyTy Washington Jr.',
                 'Vernon Carey Jr.',
                 'Vince Williams Jr.',
                 'Wendell Carter Jr.',
                 'Wendell Moore Jr.')

k = 1
for(n in sal_spelling){
  
  row = which(sal_dat$PLAYER_NAME == n)
  sal_dat$PLAYER_NAME[row] = nba_spelling[k]
  k = k + 1
  
}

#add missing salary info
add_sal = data.frame("PLAYER_NAME" = c("Chance Comanche"),
                     "SALARY" = c(5849))
sal_dat = rbind(sal_dat, add_sal)

#Chance Comanche: https://www.spotrac.com/nba/portland-trail-blazers/chance-comanche-82139/

no_PT = c("Andrew Nicholson", "Armoni Brooks", "Chet Holmgren", "Collin Gillespie",
          "Danilo Gallinari", "David Nwaba", "Demetrius Jackson", "Didi Louzada",
          "D.J. Steward", "D.J. Wilson", "Eric Bledsoe", "Justin Lewis",
          "Juwan Morgan", "Keaton Wallace", "Killian Tillie", "Kyle Singler",
          "Lonzo Ball", "Malik Fitts", "Marquese Chriss", "Moe Harkless",
          "Nate Darling", "Nik Stauskas", "Robert Woodard", "Trey Burke",
          "Tristan Thompson", "Troy Williams", "Tyrell Terry", "Zhaire Smith",
          "D.J. Augustin", "DaQuan Jeffries", "Derrick Favors", "E.J. Liddell",      
          "Feron Hunt", "Ibou Badji", "Kostas Antetokounmpo","Willie Cauley-Stein")

sal_dat = subset(sal_dat, !(PLAYER_NAME %in% no_PT))

#check, should be double NULL
sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])

players = unique(df$PLAYER_NAME)
sal_dat = sal_dat[sal_dat$PLAYER_NAME %in% players,]

avg_sal = mean(sal_dat$SALARY)

player_cfs = c(-avg_sal, rep(avg_game * avg_sgv, 82))

irr(player_cfs, interval = NULL, cf.freq = 1)


################################################################################
################################################################################
################################################################################
#Table B.1 - Preliminary Logistic Regression
################################################################################
################################################################################
################################################################################
seas_dat_clean = read.csv('./clean_data/seas_dat_clean.csv')
seas_dat_clean = seas_dat_clean[,-1]

#examine all fields to find 'winning' fields
game_ids = unique(seas_dat_clean$GAME_ID)
win_loss = c() #for game outcome

flds = list(
  FG2O = c(), 
  FG2X = c(), 
  FG3O = c(), 
  FG3X = c(),
  FTMO = c(), 
  FTMX = c(),
  PF = c(),
  AORB = c(),
  ADRB = c(),
  STL = c(),
  BLK = c(),
  TOV = c(),
  BLKA = c(),
  PFD = c(),
  AST = c(), 
  SAST = c(), 
  DEFL = c(),
  CHGD = c(),
  AC2P = c(),
  C3P = c(),
  OBOX = c(),
  DBOX = c(),
  OLBR = c(),
  DLBR = c(),
  DFGO = c(), 
  DFGX = c(),
  DRV = c(),
  ODIS = c(),
  DDIS = c(),
  APM = c(),
  AST2 = c(), 
  FAST = c(),
  OCRB = c(),
  AORC = c(),
  DCRB = c(),
  ADRC = c()
)

#confirm all fields included except PTS
names(seas_dat_clean)[which(names(seas_dat_clean) %in% names(flds) == FALSE)]

#create the team level logistic regression data
for(g in game_ids){
  
  cur_game = subset(seas_dat_clean, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  #game outcome
  p1 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[1]])
  p2 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[2]])
  
  win_loss = append(win_loss,1*c(p1 > p2, p2 > p1))
  
  #team stats
  for(j in c(1:length(flds))){
    
    f_name = names(flds)[j]
    tot1 = sum( cur_game[cur_game$TEAM_ID == teams[1], f_name])
    tot2 = sum( cur_game[cur_game$TEAM_ID == teams[2], f_name])
    
    flds[[j]] = append(flds[[j]], c(tot1, tot2))
    
  }
  
}

team_df = do.call(cbind,flds)
team_df = as.data.frame(team_df)
#center data to create average team interpretation
team_df = scale(team_df, scale=FALSE)
team_df = as.data.frame(team_df)

team_df$OUTCOME = win_loss

#full model with all 'possible' fields
model <- glm(OUTCOME~., family="binomial", data=team_df)

#basic diagnostics
car::vif(model) #minor colinearity issue for FGs; corrected in final model
pscl::pR2(model)["McFadden"] #model performing 'well'

#Table B.1. for paper online appendix/supplement
sum = summary(model)
as.data.frame(sum$coefficients)


################################################################################
################################################################################
################################################################################
#Table C.1 - Model Versus Actual Wins
################################################################################
################################################################################
################################################################################
path = "./results"
file = '2023regseason_WL.csv'

WL_results = read.csv(paste(path,file,sep="/"))

expW = c()
teams = unique(WL_results$TEAM_ABBREVIATION)
for(t in teams){
  expW = append(expW,
                sum( WL_results$win_logit[WL_results$TEAM_ABBREVIATION == t] ))
}
exp_win = data.frame("team" = teams,"expW" = expW)

exp_win[with(exp_win, order(-expW)), ]
sum(exp_win$expW) #demonstrate no bias

path = "./results"
file = '2023regseason_WS.csv'
WS_results = read.csv(paste(path,file,sep="/"))

expW = c()
teams = unique(WS_results$TEAM_ABBREVIATION)
for(t in teams){
  expW = append(expW,
                sum( WS_results$game_WS[WS_results$TEAM_ABBREVIATION == t] ))
}
exp_win = data.frame("team" = teams,"expW" = expW)

exp_win[with(exp_win, order(-expW)), ]
sum(exp_win$expW) #demonstrate no bias

path = "./results"
file = '2023regseason_GS.csv'
GS_results = read.csv(paste(path,file,sep="/"))

#GS_results = GS_results[!(GS_results$GAME_ID %in% no_track),]

expW = c()
teams = unique(GS_results$TEAM_ABBREVIATION)
for(t in teams){
  expW = append(expW,
                sum( GS_results$game_GS[GS_results$TEAM_ABBREVIATION == t] ))
}
exp_win = data.frame("team" = teams,"expW" = expW)

exp_win[with(exp_win, order(-expW)), ]
sum(exp_win$expW) #demonstrate no bias

################################################################################
################################################################################
################################################################################
#Table C.2 - Team Level Models and Wins
################################################################################
################################################################################
################################################################################
path = "./results"
file = '2023regseason_WL.csv'

WL_results = read.csv(paste(path,file,sep="/"))
seas_dat = read.csv("./clean_data/seas_dat.csv")

#cleaned season data
game_results = data.frame(
  "GAME_ID" = seas_dat$GAME_ID,
  "TEAM_ID" = seas_dat$TEAM_ID,
  "PLAYER_ID" = seas_dat$PLAYER_ID,
  "PTS" = seas_dat$PTS
)

game_results$M_ID = paste(game_results$GAME_ID,
                          game_results$TEAM_ID,
                          game_results$PLAYER_ID,
                          sep="_")

WL_results = data.frame("GAME_ID" = WL_results$GAME_ID,
                        "PLAYER_ID" = WL_results$PLAYER_ID,
                        "TEAM_ID" = WL_results$TEAM_ID,
                        "WL" = WL_results$win_logit)

WL_results$M_ID = paste(WL_results$GAME_ID,
                        WL_results$TEAM_ID,
                        WL_results$PLAYER_ID,
                        sep="_")

WL_results = data.frame("M_ID" = WL_results$M_ID,
                        "WL" = WL_results$WL)

log_df = merge(game_results, WL_results, by = "M_ID")

game_ids = unique(log_df$GAME_ID)

win_loss = c()
WL_WL = c()
g_id = c()

for(g in game_ids){
  
  cur_game = subset(log_df, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  p1 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[1]])
  p2 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[2]])
  
  win_loss = append(win_loss,1*c(p1 > p2, p2 > p1))
  
  gs1 = sum(cur_game$WL[cur_game$TEAM_ID == teams[1]])
  gs2 = sum(cur_game$WL[cur_game$TEAM_ID == teams[2]])
  
  WL_WL = append(WL_WL, c(gs1, gs2))
  
  g_id = append(g_id, c(g, g))
  
}

dat = data.frame("GAME_ID" = g_id,
                 "OUTCOME" = win_loss,
                 "WL" = WL_WL)

wl_DAT = dat
wl_DAT$link = paste(wl_DAT$GAME_ID, wl_DAT$OUTCOME, sep="_")

path = "./results"
file = '2023regseason_WS.csv'

WS_results = read.csv(paste(path,file,sep="/"))

#cleaned season data
game_results = data.frame(
  "GAME_ID" = seas_dat$GAME_ID,
  "TEAM_ID" = seas_dat$TEAM_ID,
  "PLAYER_ID" = seas_dat$PLAYER_ID,
  "PTS" = seas_dat$PTS
)

game_results$M_ID = paste(game_results$GAME_ID,
                          game_results$TEAM_ID,
                          game_results$PLAYER_ID,
                          sep="_")

WS_results = data.frame("GAME_ID" = WS_results$GAME_ID,
                        "PLAYER_ID" = WS_results$PLAYER_ID,
                        "TEAM_ID" = WS_results$TEAM_ID,
                        "WS" = WS_results$game_WS)

WS_results$M_ID = paste(WS_results$GAME_ID,
                        WS_results$TEAM_ID,
                        WS_results$PLAYER_ID,
                        sep="_")

WS_results = data.frame("M_ID" = WS_results$M_ID,
                        "WS" = WS_results$WS)

log_df = merge(game_results, WS_results, by = "M_ID")

game_ids = unique(log_df$GAME_ID)

win_loss = c()
WS_WL = c()
g_id = c()

for(g in game_ids){
  
  cur_game = subset(log_df, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  p1 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[1]])
  p2 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[2]])
  
  win_loss = append(win_loss,1*c(p1 > p2, p2 > p1))
  
  ws1 = sum(cur_game$WS[cur_game$TEAM_ID == teams[1]])
  ws2 = sum(cur_game$WS[cur_game$TEAM_ID == teams[2]])
  
  WS_WL = append(WS_WL, c(ws1, ws2))
  
  g_id = append(g_id, c(g, g))
  
}

ws_DAT = data.frame("GAME_ID" = g_id,
                 "OUTCOME" = win_loss,
                 "WS" = WS_WL)
ws_DAT$link = paste(ws_DAT$GAME_ID, ws_DAT$OUTCOME, sep="_")

path = "./results"
file = '2023regseason_GS.csv'

GS_results = read.csv(paste(path,file,sep="/"))

#cleaned season data
game_results = data.frame(
  "GAME_ID" = seas_dat$GAME_ID,
  "TEAM_ID" = seas_dat$TEAM_ID,
  "PLAYER_ID" = seas_dat$PLAYER_ID,
  "PTS" = seas_dat$PTS
)

game_results$M_ID = paste(game_results$GAME_ID,
                          game_results$TEAM_ID,
                          game_results$PLAYER_ID,
                          sep="_")

GS_results = data.frame("GAME_ID" = GS_results$GAME_ID,
                        "PLAYER_ID" = GS_results$PLAYER_ID,
                        "TEAM_ID" = GS_results$TEAM_ID,
                        "GS" = GS_results$game_GS)

GS_results$M_ID = paste(GS_results$GAME_ID,
                        GS_results$TEAM_ID,
                        GS_results$PLAYER_ID,
                        sep="_")

GS_results = data.frame("M_ID" = GS_results$M_ID,
                        "GS" = GS_results$GS)

log_df = merge(game_results, GS_results, by = "M_ID")

game_ids = unique(log_df$GAME_ID)

win_loss = c()
GS_WL = c()
g_id = c()

for(g in game_ids){
  
  cur_game = subset(log_df, GAME_ID == g)
  teams = unique(cur_game$TEAM_ID)
  
  p1 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[1]])
  p2 = sum(cur_game$PTS[cur_game$TEAM_ID == teams[2]])
  
  win_loss = append(win_loss,1*c(p1 > p2, p2 > p1))
  
  gs1 = sum(cur_game$GS[cur_game$TEAM_ID == teams[1]])
  gs2 = sum(cur_game$GS[cur_game$TEAM_ID == teams[2]])
  
  GS_WL = append(GS_WL, c(gs1, gs2))
  
  g_id = append(g_id, c(g, g))
  
}

gs_DAT = data.frame("GAME_ID" = g_id,
                 "OUTCOME" = win_loss,
                 "GS" = GS_WL)

gs_DAT$link = paste(gs_DAT$GAME_ID, gs_DAT$OUTCOME, sep="_")

dat1 = merge(wl_DAT, ws_DAT, by = "link")
dat = merge(dat1, gs_DAT, by = "link")

outcome = as.numeric(str_sub(dat$link,-1,-1))

dat = data.frame("res" = outcome,
                 "wl" = dat$WL,
                 "ws" = dat$WS,
                 "gs" = dat$GS)

mod1 <- glm(res ~ wl + ws + gs, family=binomial, data=dat)
pscl::pR2(mod1)["McFadden"]
summary(mod1)
car::vif(mod1) #colinearity issue; GS and WS highly correlated
pscl::pR2(mod1)["McFadden"] #still performs 'well'
caret::varImp(mod1) #most important covariates

#above is Table C2; bonus regressions below

df = scale(data.frame("wl" = dat$wl, "ws" = dat$ws, "gs" = dat$gs))
df = as.data.frame(df)
df = cbind(outcome, df)

mod1 <- glm(outcome ~ wl + ws + gs, family=binomial, data=df)
pscl::pR2(mod1)["McFadden"]
summary(mod1)
car::vif(mod1) #colinearity issue; GS and WS highly correlated
pscl::pR2(mod1)["McFadden"] #still performs 'well'
caret::varImp(mod1) #most important covariates

mod <- glm(res ~ wl + ws, family=binomial, data=dat)
summary(mod)
pscl::pR2(mod)["McFadden"]
car::vif(mod) #no colinearity issue
pscl::pR2(mod)["McFadden"] #still performs 'well'
caret::varImp(mod) #most important covariates

mod <- glm(res ~ wl + gs, family=binomial, data=dat)
summary(mod)
pscl::pR2(mod)["McFadden"]
car::vif(mod) #no colinearity issue
pscl::pR2(mod)["McFadden"] #still performs 'well'
caret::varImp(mod) #most important covariates

mod <- glm(res ~ ws + gs, family=binomial, data=dat)
summary(mod)
pscl::pR2(mod)["McFadden"]
car::vif(mod) #no colinearity issue
pscl::pR2(mod)["McFadden"] #still performs 'well'
caret::varImp(mod) #most important covariates

################################################################################
################################################################################
################################################################################
#Figure C1: Simulation Study Results
################################################################################
################################################################################
################################################################################


path = "./results"
file = '2023regseason_WL.csv'

df = read.csv(paste(path,file,sep="/"))
head(df)

sum(df$game_logit)
sum(df$win_logit)

d1 <- df[,c(2,46)]
head(d1)

n = length(unique(df$GAME_ID))
g_id = unique(df$GAME_ID)

#parameters of normal distribution for SGV
mu = 100
sig = 5
mu2 = 5 + 100^2

#theorem 3.1 expected value
res <- c()
for(i in c(1:1000)){
  
  #sim one result of SGVs for each game
  set.seed(i)
  cur_scen_sgv = data.frame("GAME_ID" = g_id,
                            "SGV" = rnorm(n, mu, sig))
  
  #merge conditional win logits
  cur_scen_dat = merge(d1, cur_scen_sgv, by = "GAME_ID")
  
  #save results
  res = append(res, sum( cur_scen_dat$win_logit * cur_scen_dat$SGV ))
  
  if(i %% 100==0) {
    # Print on the screen some message
    cat(paste0("iteration: ", i, "\n"))
  }
  
}

summary(res)
mean(res)
mu * n

plot_df = data.frame("Sim_Result" = res,
                     "Measure" = rep(1, length(res)))

ggplot(plot_df, aes(x = Sim_Result, colour = Measure, fill=Measure)) +
  geom_density(alpha = 0.25, linewidth = 0.5) +
  xlab("Simulated Result") +
  ylab("Frequency (Density)") +
  scale_x_continuous(labels = scales::comma) +
  theme_bw() +
  theme(axis.title.x=element_text(size=9, family="Times New Roman"),
        axis.text.x=element_text(size=9, family="Times New Roman"),
        axis.text.y=element_text(size=9, family="Times New Roman"),
        axis.title.y=element_text(size=9,family="Times New Roman"),
        strip.text.y = element_text(size = 9, family="Times New Roman"),
        legend.position = "none") +
  geom_vline(xintercept = mean(res),
             linetype='dashed',
             color="red",
             linewidth=1) +
  geom_vline(xintercept = mu * n,
             linetype='solid',
             color="black",
             linewidth=1)

#save, if desired
ggsave("./results/sim_results.pdf",height=4,width=6,device = cairo_pdf)


#theorem 3.1 variance
res <- c()
for(i in c(1:10000)){
  
  #sim one result of SGVs for each game
  set.seed(i)
  cur_scen_sgv = data.frame("GAME_ID" = g_id,
                            "SGV" = rnorm(n, mu, sig))
  
  #merge conditional win logits
  cur_scen_dat = merge(d1, cur_scen_sgv, by = "GAME_ID")
  
  #save results
  res = append(res, sum( cur_scen_dat$win_logit * cur_scen_dat$SGV ))
  
  if(i %% 100==0) {
    # Print on the screen some message
    cat(paste0("iteration: ", i, "\n"))
  }
  
}

Wg = c()
for(g in g_id){
  Wg = append(Wg, sum(d1$win_logit[d1$GAME_ID == g]))
}

var(res)
sum( Wg^2 ) * sig^2

