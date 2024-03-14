#new total contrib formula for GS
total_contrib_GS_ROI = function(player_name){
  
  if(!(player_name %in% df$PLAYER_NAME)){
    return(print("ERROR! Player not found."))
  }
  player_name = player_name
  player_team = unique(df$TEAM_ABBREVIATION[df$PLAYER_NAME == player_name])
  
  #if player not traded in season (i.e., # teams == 1)
  if(length(player_team) == 1){
    player_games = sort(unique(df$GAME_ID[df$TEAM_ABBREVIATION == player_team]))
    player_contribs = c()
    for(g in player_games){
      
      play = which((df$GAME_ID == g) & (df$PLAYER_NAME == player_name))
      if( length(play) == 1){
        player_contribs = append(player_contribs, df$game_GS[play])
      }
      else if( length(play) == 0){
        player_contribs = append(player_contribs, 0)
      }
    }
  }
  
  #if player traded (i.e., #teams > 1)
  if(length(player_team) > 1){
    player_contribs = matrix(0,nrow=82,ncol=length(player_team))
    colnames(player_contribs) = player_team
    
    player_games = matrix(NA,nrow=82,ncol=length(player_team))
    colnames(player_games) = player_team
    
    for(t in player_team){
      
      player_games_t = sort(unique(df$GAME_ID[df$TEAM_ABBREVIATION == t]))
      
      t_contribs = c()
      for(g in player_games_t){
        play = which((df$GAME_ID == g) &
                       (df$PLAYER_NAME == player_name) &
                       (df$TEAM_ABBREVIATION == t))
        if( length(play) == 1){
          t_contribs = append(t_contribs, df$game_GS[play])
        }
        else if( length(play) == 0){
          t_contribs = append(t_contribs, 0)
        }
      }
      
      player_contribs[1:length(t_contribs),t] = t_contribs
      player_games[1:length(player_games_t),t] = player_games_t
      
    }
    
    #replace no track games with min single game value
    player_games[is.na(player_games)] <- 22201222
    
    max_row = c()
    for(t in player_team){
      max_row = append(max_row, max(which(player_contribs[,t] != 0)))
    }
    
    max_row[length(player_team)] = 82
    p_games = rep(NA,82); k = 1
    for(c in max_row){
      if(c == max_row[1]){
        min_row = 1
      }
      if(c != max_row[1]){
        min_row = max_row[k-1] + 1
      }
      p_games[c(min_row:max_row[k])] = player_games[c(min_row:max_row[k]),k]
      k = k + 1
    }
    
    player_contribs = apply(player_contribs, 1, FUN = sum)
    player_games = p_games
    
  }
  
  #organize results
  res = list("sum_total" = sum(player_contribs),
             "game_totals" = player_contribs,
             "game_ids" = player_games)
  
  return(res)
}