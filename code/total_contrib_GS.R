#calculate the PVGS per player
total_contrib_GS = function(player_name){
  
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
    
    for(t in player_team){
      
      player_games = sort(unique(df$GAME_ID[df$TEAM_ABBREVIATION == t]))
      
      t_contribs = c()
      for(g in player_games){
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
      
    }
    player_contribs = apply(player_contribs, 1, FUN = sum)
  }
  
  #organize results
  res = list("sum_total" = sum(player_contribs),
             "game_totals" = player_contribs)
  
  return(res)
}