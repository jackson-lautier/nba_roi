#R version: 4.2.2
#RStudio 2023.03.0+386 "Cherry Blossom" Release (3c53477afb13ab959aeb5b34df1f10c237b256c3, 2023-03-09) for Windows

#necessary packages
require(stringr) #convert minutes to usable formats
require(ggplot2) #game by game plots.
require(jrvFinance) #IRR calculations
require('extrafont') #may need to load fonts
require('cowplot') #multiple plots same
require(ggrepel) #label ggplot ROI by salary

#bring in required functions
source('~/nba_roi/code/rep_code_functions.R')

#clean scraped nba.com data
{
  path = "~/nba_roi/data/"
  
  f1 = read.csv(paste(path,"full_stats1.csv",sep="/"))
  f2 = read.csv(paste(path,"full_stats2.csv",sep="/"))
  f3 = read.csv(paste(path,"full_stats3.csv",sep="/"))
  f4 = read.csv(paste(path,"full_stats4.csv",sep="/"))
  f5 = read.csv(paste(path,"full_stats5.csv",sep="/"))
  f6 = read.csv(paste(path,"full_stats6.csv",sep="/"))
  
  seas_dat = rbind(f1, f2, f3, f4, f5, f6)
  seas_dat = subset(seas_dat, PLAYER_ID != 999999)
  
  #clean up minutes
  seas_dat$MIN = as.numeric(sapply(seas_dat$MIN, min_convert))
  #convert NA to 0's
  seas_dat[is.na(seas_dat)] <- 0
  
  write.csv(seas_dat, '~/nba_roi/data/2023regseason.csv')
  rm(list=ls())
}

#get box score data, field names for GCP
#perform GCP calculation
{
  source('~/nba_roi/code/rep_code_functions.R')
  
  path = "~/nba_roi/data"
  seas_dat = read.csv(paste(path,"2023regseason.csv",sep="/"))
  
  #cleaned season data
  seas_dat_clean = data.frame(
    "GAME_ID" = seas_dat$GAME_ID,
    "TEAM_ID" = seas_dat$TEAM_ID,
    "TEAM_ABBREVIATION" = seas_dat$TEAM_ABBREVIATION,
    "PLAYER_ID" = seas_dat$PLAYER_ID,
    "PLAYER_NAME" = seas_dat$PLAYER_NAME,
    "MIN" = seas_dat$MIN,
    "FG2O" = seas_dat$FGM - seas_dat$FG3M,
    "FG2X" = (seas_dat$FGA - seas_dat$FG3A) - (seas_dat$FGM - seas_dat$FG3M),
    "FG3O" = seas_dat$FG3M,
    "FG3X" = seas_dat$FG3A - seas_dat$FG3M,
    "FTMO" = seas_dat$FTM,
    "FTMX" = seas_dat$FTA - seas_dat$FTM,
    "PF" = seas_dat$PF,
    #"PTS" = seas_dat$PTS,
    "STL" = seas_dat$STL,
    "BLK" = seas_dat$BLK,
    "TOV" = seas_dat$TO,
    "BLKA" = seas_dat$BLKA,
    "PFD" = seas_dat$PFD,
    "POSS" = seas_dat$POSS,
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
    "TCH" = seas_dat$TOUCHES,
    "APM" = seas_dat$PASSES_MADE - seas_dat$SECONDARY_AST - seas_dat$POTENTIAL_AST,
    "PASR" = seas_dat$PASSES_RECEIVED,
    "AST2" = seas_dat$SECONDARY_AST,
    "PAST" = seas_dat$POTENTIAL_AST,
    "OCRB" = seas_dat$OREB_CONTEST,
    "AORC" = seas_dat$OREB_CHANCES - seas_dat$OREB_CONTEST,
    "DCRB" = seas_dat$DREB_CONTEST,
    "ADRC" = seas_dat$DREB_CHANCES - seas_dat$DREB_CONTEST
  )
  
  
  
  perc_fields = names(seas_dat_clean)[!(names(seas_dat_clean)
                                        %in%
                                          c("GAME_ID", "TEAM_ID",
                                            "TEAM_ABBREVIATION", "PLAYER_ID",
                                            "PLAYER_NAME"))]
  
  #naive, uniform weights
  w = 1/length(perc_fields)
  weights = matrix(rep(w,length(perc_fields)),1,length(perc_fields))
  colnames(weights) = perc_fields
  weights = data.frame(weights)
  
  game_list = sort(unique(seas_dat$GAME_ID))
  
  start.time <- Sys.time()
  cont_df_list = lapply(game_list, contrib_calc)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  df = do.call(rbind, cont_df_list)
  write.csv(df, '~/nba_roi/data/2023regseason_percontrib.csv')
}

#table 2, 3
{
  rm(list=ls())
  source('~/nba_roi/code/rep_code_functions.R')
  
  path = "~/nba_roi/data"
  file = '2023regseason_percontrib.csv'
  
  df = read.csv(paste(path,file,sep="/"))
  
  g_id = 22201181
  
  df = df[df$GAME_ID == g_id,]
  write.csv(df, '~/nba_roi/data/gcp_illustration.csv')
  
}

#histogram all GCPs
{
  rm(list=ls())
  source('~/nba_roi/code/rep_code_functions.R')
  
  path = "~/nba_roi/data"
  file = '2023regseason_percontrib.csv'
  
  df = read.csv(paste(path,file,sep="/"))
  
  df_nozero = df[df$game_contrib_perc > 0,]
  
  ggplot(df_nozero, aes(x=game_contrib_perc)) +
    geom_histogram(color="black", fill="steelblue", binwidth = 0.005) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    xlab("Game Contribution Percentage") +
    ylab("Number of Games") +
    theme_bw() +
    theme(axis.title.x=element_text(size=9, family="Times New Roman"),
          axis.text.x=element_text(size=9, family="Times New Roman"),
          axis.text.y=element_text(size=9, family="Times New Roman"),
          axis.title.y=element_text(size=9,family="Times New Roman"),
          legend.text=element_text(size=9, family="Times New Roman"),
          strip.text.y = element_text(size = 9, family="Times New Roman"),
          legend.title=element_text(size=10, family="Times New Roman"))
    
  
  #ggsave("GCP_hist.pdf",height=4,width=6,device = cairo_pdf)
  
  df_nozero$PLAYER_NAME[which.max(df_nozero$game_contrib_perc)]
  df_nozero$game_contrib_perc[which.max(df_nozero$game_contrib_perc)]
  df_nozero$GAME_ID[which.max(df_nozero$game_contrib_perc)]
  
  #embiid calculation
  X <- sort(df_nozero$game_contrib_perc)
  je_gcp = 
    df_nozero$game_contrib_perc[(df_nozero$GAME_ID == 22201181)
                                & (df_nozero$PLAYER_NAME == "Joel Embiid")]
  e_cdf <- 1:length(X) / length(X)
  e_cdf[which(X == je_gcp)]
  
}

#top 50 PV GCP, per game GCP
{
  rm(list=ls())
  
  source('~/nba_roi/code/rep_code_functions.R')
  
  path = "~/nba_roi/data"
  file = '2023regseason_percontrib.csv'
  
  df = read.csv(paste(path,file,sep="/"))
  
  players = unique(df$PLAYER_NAME)
  
  start.time <- Sys.time()
  GP = c()
  PV_GCP = c()
  for(p in players){
    GP = append(GP, sum(total_contrib(p)$game_totals > 0))
    PV_GCP = append(PV_GCP, total_contrib(p)$sum_total)
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  table_df = data.frame("player" = players,
                       "GP" = GP,
                       "PV_GCP" = PV_GCP,
                       "GCPpg" = PV_GCP / GP)
  
  topX = 50
  top_df = subset(table_df, PV_GCP >= quantile(PV_GCP, 1 - topX/547))
  
  top_df = top_df[with(top_df, order(-PV_GCP)), ]
  
  write.csv(top_df, "~/nba_roi/data/top50.csv")
  
  table_df$player[which.max(table_df$GCPpg)]
  
}

# GCP comparison Davis, Lopez
{
  rm(list=ls())
  
  source('~/nba_roi/code/rep_code_functions.R')
  
  path = "~/nba_roi/data"
  file = '2023regseason_percontrib.csv'
  
  df = read.csv(paste(path,file,sep="/"))
  
  #plot_df = data.frame("player" = players,
  #                     "GP" = GP,
  #                     "PV_GCP" = PV_GCP,
  #                     "GCPpg" = PV_GCP / GP)
  
  
  g_davis = plot_games("Anthony Davis")
  g_lopez = plot_games("Brook Lopez")
  
  plot_grid(g_davis, g_lopez, nrow=2)
  
  #ggsave("GCP_comp.pdf",height=4,width=6,device = cairo_pdf)
  
}

#ROI calculations
{
  rm(list=ls())
  source('~/nba_roi/code/rep_code_functions.R')
  
  path = "~/nba_roi/data"
  file = '2023regseason_percontrib.csv'
  
  df = read.csv(paste(path,file,sep="/"))
  
  #calc for all players 22-23 regular season
  all_players = sort(unique(df$PLAYER_NAME))
  
  start.time <- Sys.time()
  pvs = sapply(all_players, total_contrib)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  
  path = "~/nba_roi/data/"
  file = "22.23_player_salary.csv"
  
  sal_dat = read.csv(paste(path,file,sep="/"))
  names(sal_dat) = c("PLAYER_NAME", "SALARY")
  
  sal_dat$SALARY = as.numeric(gsub('[$,]', '', sal_dat$SALARY))
  
  #clean and combine into single file
  ctrb_dat = data.frame("PLAYER_NAME" = names(unlist(pvs["sum_total",])),
                        "CONTRIB_PV_FACTOR" = as.numeric(unlist(pvs["sum_total",])))
  
  
  #prepare cleaned file; need to connect by names
  sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
  sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])
  
  #clean names
  sal_spelling = c("AJ Lawson", "BJ Boston", "Danuel House", "David Duke",
                   "Dennis Schroeder", "Dennis Smith","Derrick Jones",
                   "Devonte Graham", "DJ Augustin", "DJ Steward", "DJ Wilson",
                   "Duane Washington Jr", "EJ Liddell", "Gary Trent Jr",
                   "Greg Brown", "Herb Jones", "Ishmael Smith", "Ishmail Wainright",
                   "Jabari Smith", "Jaren Jackson Jr", "Jeff Dowtin", "John Butler",
                   "Josh Primo", "Juan Hernangomez", "Kelly Oubre", "Kenneth Lofton",
                   "Kevin Knox", "Kevin Porter", "Kira Lewis", "KJ Martin",
                   'Larry Nance Jr',
                   'Lonnie Walker',
                   'Marcus Morris',
                   'Marvin Bagley',
                   'McKinley Wright',
                   'Michael Foster',
                   'Michael Porter',
                   'Nicolas Claxton',
                   'OG Anunoby',
                   'Otto Porter',
                   'Patrick Baldwin',
                   'Patrick Mills',
                   'PJ Tucker',
                   'PJ Washington',
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
                   "Jaren Jackson Jr.", "Jeff Dowtin Jr.", "John Butler Jr.",
                   "Joshua Primo", "Juancho Hernangomez", "Kelly Oubre Jr.",
                   "Kenneth Lofton Jr.", "Kevin Knox II", "Kevin Porter Jr.",
                   "Kira Lewis Jr.", "Kenyon Martin Jr.",
                   'Larry Nance Jr.',
                   'Lonnie Walker IV',
                   'Marcus Morris Sr.',
                   'Marvin Bagley III',
                   'McKinley Wright IV',
                   'Michael Foster Jr.',
                   'Michael Porter Jr.',
                   'Nic Claxton',
                   'O.G. Anunoby',
                   'Otto Porter Jr.',
                   'Patrick Baldwin Jr.',
                   'Patty Mills',
                   'P.J. Tucker',
                   'P.J. Washington',
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
  
  #GCP = 0; i.e., no NBA playing time 22-23 list
  no_GCP = c("Andrew Nicholson", "Armoni Brooks", "Chet Holmgren", "Collin Gillespie",
             "Danilo Gallinari", "David Nwaba", "Demetrius Jackson", "Didi Louzada",
             "D.J. Steward", "D.J. Wilson", "Eric Bledsoe", "Justin Lewis",
             "Juwan Morgan", "Keaton Wallace", "Killian Tillie", "Kyle Singler",
             "Lonzo Ball", "Malik Fitts", "Marquese Chriss", "Moe Harkless",
             "Nate Darling", "Nik Stauskas", "Robert Woodard", "Trey Burke",
             "Tristan Thompson", "Troy Williams", "Tyrell Terry", "Zhaire Smith")
  sal_dat = subset(sal_dat, !(PLAYER_NAME %in% no_GCP))
  
  #add missing salary info
  add_sal = data.frame("PLAYER_NAME" = c("Chance Comanche"),
                       "SALARY" = c(5849))
  sal_dat = rbind(sal_dat, add_sal)
  
  #Chance Comanche: https://www.spotrac.com/nba/portland-trail-blazers/chance-comanche-82139/
  
  #check, should be double NULL
  sort(sal_dat$PLAYER_NAME[which(!(sal_dat$PLAYER_NAME %in% ctrb_dat$PLAYER_NAME))])
  sort(ctrb_dat$PLAYER_NAME[which(!(ctrb_dat$PLAYER_NAME %in% sal_dat$PLAYER_NAME))])
  
  #build unique data file w salary, GCP CFs
  roi_dat = merge(sal_dat, ctrb_dat, by = "PLAYER_NAME")
  
  #minimum games played criteria
  n_players = nrow(roi_dat)
  g_played = c()
  for(i in c(1:n_players)){
    g_played = append(g_played, sum(pvs[[2 * i]] > 0))
  }
  
  roi_dat$ADJ_GP = g_played
  write.csv(roi_dat, '~/nba_roi/data/2023roi.csv')
  
  #perform the ROI calculations
  rm(list=ls())
  source('~/nba_roi/code/rep_code_functions.R')
  
  path = "~/nba_roi/data"
  file = '2023roi.csv'
  
  roi_dat = read.csv(paste(path,file,sep="/"))
  
  #set minimum games played
  MIN_GAME = 25
  
  #calculate dollar value of each game
  one_game_val = sum(roi_dat$SALARY) / (1230 * 2)
  
  roi_dat_calc = subset(roi_dat, ADJ_GP >= MIN_GAME)
  
  all_players = roi_dat_calc$PLAYER_NAME
  
  path = "~/nba_roi/data"
  file = '2023regseason_percontrib.csv'
  
  df = read.csv(paste(path,file,sep="/"))
  
  start.time <- Sys.time()
  ROIs = sapply(all_players, NBA_IRR)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  table_df = data.frame("player" = roi_dat_calc$PLAYER_NAME,
                        "salary" = roi_dat_calc$SALARY,
                        "GP" = roi_dat_calc$ADJ_GP,
                        "PV_GCP" = roi_dat_calc$CONTRIB_PV_FACTOR,
                        "ROI" = as.numeric(ROIs))
  
  topX = 50
  top_df = subset(table_df, ROI >= quantile(ROI, 1 - topX/nrow(table_df)))
  
  top_df = top_df[with(top_df, order(-ROI)), ]
  
  write.csv(top_df, "~/nba_roi/data/top50_ROI.csv")
  
  bottom_df = subset(table_df, ROI <= quantile(ROI, topX/nrow(table_df)))
  
  bottom_df = bottom_df[with(bottom_df, order(ROI)), ]
  
  write.csv(bottom_df, "~/nba_roi/data/bottom50_ROI.csv")
  
  summary(roi_dat_calc$SALARY) / 1000000
  summary(roi_dat_calc$ADJ_GP)
}

#ROI by salary plot
{
  #perform the ROI calculations
  rm(list=ls())
  source('~/nba_roi/code/rep_code_functions.R')
  
  path = "~/nba_roi/data"
  file = '2023roi.csv'
  
  roi_dat = read.csv(paste(path,file,sep="/"))
  
  max(roi_dat$SALARY)/1000000
  roi_dat$PLAYER_NAME[which.max(roi_dat$SALARY)]
  
  #set minimum games played
  MIN_GAME = 25
  
  #calculate dollar value of each game
  one_game_val = sum(roi_dat$SALARY) / (1230 * 2)
  
  roi_dat_calc = subset(roi_dat, ADJ_GP >= MIN_GAME)
  
  all_players = roi_dat_calc$PLAYER_NAME
  
  path = "~/nba_roi/data"
  file = '2023regseason_percontrib.csv'
  
  df = read.csv(paste(path,file,sep="/"))
  
  start.time <- Sys.time()
  ROIs = sapply(all_players, NBA_IRR)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  table_df = data.frame("player" = roi_dat_calc$PLAYER_NAME,
                        "salary" = roi_dat_calc$SALARY / 1000000,
                        "GP" = roi_dat_calc$ADJ_GP,
                        "PV_GCP" = roi_dat_calc$CONTRIB_PV_FACTOR,
                        "ROI" = as.numeric(ROIs))
  
  #find players to label
  plot(table_df$salary, table_df$ROI)
  #uncomment below function to search the plot, can customize
  #identify(table_df$salary, table_df$ROI)
  p_high = table_df$player[c(41, 109, 125, 128, 138, 249, 316, 356, 393)]
  p_low = table_df$player[c(17, 38,  45, 118, 259, 264, 334, 344)]
  
  #average line for visual reference
  sal.lo <- loess(ROI ~ salary, table_df)
  curv = predict(sal.lo, data.frame(salary = seq(0.4,48,0.1)), se = FALSE)
  c_dat = data.frame("sal" = seq(0.4,48,0.1),
                     "pred" = curv)
  
  nbaplot <- 
    ggplot(table_df, aes(x=salary, y=ROI)) +
    geom_point(color = "black", size = 1)
  
  nbaplot <- nbaplot +
    geom_line(data = c_dat,
                      aes(x=sal,y=pred),
                      color = "red")
  
  nbaplot <- nbaplot +
    geom_label_repel(data = subset(table_df, player %in% p_high),
                             aes(x=salary, y = ROI, label = player),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             nudge_y = 0.04,
                             nudge_x = 3,
                             segment.color = 'grey50',
                             family = "Times New Roman",
                             size = 2)
  
  nbaplot <- nbaplot +
    geom_label_repel(data = subset(table_df, player %in% p_low),
                     aes(x=salary, y = ROI, label = player),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     nudge_y = -0.04,
                     nudge_x = -2,
                     segment.color = 'grey50',
                     family = "Times New Roman",
                     size = 2)
  
  nbaplot <- nbaplot +
    xlab("Player Salary (Millions)")+
    ylab("Return on Investment (ROI)")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    theme_bw()+
    theme(axis.title.x=element_text(size=8, family="Times New Roman"),
          axis.text.x=element_text(size=8, family="Times New Roman"),
          axis.text.y=element_text(size=8, family="Times New Roman"),
          axis.title.y=element_text(size=8,family="Times New Roman"),
          legend.text=element_text(size=8, family="Times New Roman"),
          strip.text.y = element_text(size = 8, family="Times New Roman"),
          legend.title=element_text(size=8, family="Times New Roman"),
          title=element_text(size=8, family="Times New Roman"))
  
  nbaplot
  #ggsave("ROI_plot.pdf",height=4,width=6,device = cairo_pdf)
  
  
}


