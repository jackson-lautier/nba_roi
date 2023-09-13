#convert minutes to sum-able format
min_convert = function(game_min){
  
  if(nchar(game_min) %in% c(4,5)){
    min = as.numeric(str_split(game_min, fixed(":"))[[1]][1])
    sec = as.numeric(str_split(game_min, fixed(":"))[[1]][2])
  }
  else if(nchar(game_min) %in% c(11,12)){
    min = as.numeric(str_split(game_min, fixed(".000000:"))[[1]][1])
    sec = as.numeric(str_split(game_min, fixed(".000000:"))[[1]][2])
  }
  else if(nchar(game_min) == 0){
    min = 0
    sec = 0
  }
  return(min + (sec/60))
  
}

#GCP calculation formula per game_id
contrib_calc = function(game_id){
  
  game = subset(seas_dat_clean, GAME_ID == game_id)
  
  teams = unique(game$TEAM_ABBREVIATION)
  
  #list to store df of results for each team
  df_list = list()
  
  for(t in c(1:length(teams))){
    cur_team = subset(game, TEAM_ABBREVIATION == teams[t])
    
    #find zeros
    zeros = names(which(colSums(cur_team[,perc_fields]) == 0))
    calc_fields = perc_fields[!(perc_fields %in% zeros)]
    
    #adjust weights
    cur_weights = weights[,!(names(weights) %in% zeros)]
    cur_norm = sum(cur_weights[1,])
    cur_weights = cur_weights/cur_norm
    
    cur_num_players = nrow(cur_team)
    
    contrib = c()
    for(p in c(1:cur_num_players)){
      play_perc = data.frame(cur_team[p,calc_fields] / colSums(cur_team[,calc_fields]))
      calc_df = rbind(play_perc, cur_weights)
      contrib = append(contrib,sum(calc_df[1,] * calc_df[2,]))
    }
    
    cur_team$game_contrib_perc = contrib
    df_list[[t]] = cur_team
  }
  
  res_df = do.call(rbind,df_list)
  return(res_df)
  
}

#calculate the sum total of all GCPs per player
total_contrib = function(player_name){
  player_name = player_name
  player_team = unique(df$TEAM_ABBREVIATION[df$PLAYER_NAME == player_name])
  
  #if player not traded in season (i.e., # teams == 1)
  if(length(player_team) > 1){
    player_games = sort(unique(df$GAME_ID[df$TEAM_ABBREVIATION == player_team]))
    player_contribs = c()
    for(g in player_games){
      
      play = which((df$GAME_ID == g) & (df$PLAYER_NAME == player_name))
      if( length(play) == 1){
        player_contribs = append(player_contribs, df$game_contrib_perc[play])
      }
      else if( length(play) == 0){
        player_contribs = append(player_contribs, 0)
      }
    }
  }
  
  #if player traded (i.e., #teams > 1)
  player_contribs = matrix(NA,nrow=82,ncol=length(player_team))
  colnames(player_contribs) = player_team
  
  for(t in player_team){
    
    player_games = sort(unique(df$GAME_ID[df$TEAM_ABBREVIATION == t]))
    
    t_contribs = c()
    for(g in player_games){
      play = which((df$GAME_ID == g) &
                     (df$PLAYER_NAME == player_name) &
                     (df$TEAM_ABBREVIATION == t))
      if( length(play) == 1){
        t_contribs = append(t_contribs, df$game_contrib_perc[play])
      }
      else if( length(play) == 0){
        t_contribs = append(t_contribs, 0)
      }
    }
    
    player_contribs[,t] = t_contribs
    
  }
  player_contribs = apply(player_contribs, 1, FUN = max)
  
  #organize results
  res = list("sum_total" = sum(player_contribs),
             "game_totals" = player_contribs)
  
  return(res)
}

#function to plot game by game results as percentages
plot_games = function(player_name){
  results = total_contrib(player_name)
  total = format(round(results[["sum_total"]], 3), nsmall = 2)
  GP = sum(results$game_totals > 0)
  per_game = format(round(results$sum_total / GP, 4), nsmall = 2)
  
  games = as.factor(c(1:82))
  plot_df = data.frame("games" = games,
                       "contributions" = results[["game_totals"]])
  
  g <- 
    ggplot(data=plot_df, aes(x=games, y=contributions))+
    geom_bar(stat="identity",fill="steelblue")+
    guides(x =  guide_axis(angle = 90))+
    ggtitle(paste(player_name,
                  " (PVGCP: ",total,";",
                  " Per Game GCP: ",per_game,")",
                  sep=""))+
    xlab("Game Number")+
    ylab("Game Contribution Percentage")+
    coord_cartesian(ylim=c(0,0.35))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    theme_bw()+
    theme(axis.title.x=element_text(size=8, family="Times New Roman"),
          axis.text.x=element_text(size=5, family="Times New Roman"),
          axis.text.y=element_text(size=8, family="Times New Roman"),
          axis.title.y=element_text(size=8,family="Times New Roman"),
          legend.text=element_text(size=8, family="Times New Roman"),
          strip.text.y = element_text(size = 8, family="Times New Roman"),
          legend.title=element_text(size=8, family="Times New Roman"),
          title=element_text(size=8, family="Times New Roman"))
  
  return(g)
}

#nba IRR function
NBA_IRR = function(player_name){
  player_name = player_name
  player_team = unique(df$TEAM_ABBREVIATION[df$PLAYER_NAME == player_name])
  player_games = sort(unique(df$GAME_ID[df$TEAM_ABBREVIATION == player_team]))
  
  player_contribs = c()
  for(g in player_games){
    play = which((df$GAME_ID == g) & (df$PLAYER_NAME == player_name))
    if( length(play) == 1){
      player_contribs = append(player_contribs, df$game_contrib_perc[play])
    }
    else if( length(play) == 0){
      player_contribs = append(player_contribs, 0)
    }
  }
  
  sal = roi_dat$SALARY[which(roi_dat$PLAYER_NAME == player_name)]
  
  ply_cfs = c(-sal,player_contribs * one_game_val)
  
  return(irr(ply_cfs, interval = NULL, cf.freq = 1))
}

