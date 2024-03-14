NBA_IRR_WL = function(player_name, min_games){
  
  oncourt_info = total_contrib_WL_ROI(player_name)
  
  if( class(oncourt_info) == "character" ){
    return("Executed halted.")
  }
  
  if( sum(oncourt_info$game_totals != 0) < min_games){
    return(print("Minimum games not met!"))
  }
  
  player_salary = sal_dat$SALARY[sal_dat$PLAYER_NAME == player_name]
  
  player_game_vals = sgvs$TOTAL[sgvs$GAME_ID %in% oncourt_info$game_ids]
  
  player_cfs = c(-player_salary,
                 player_game_vals * oncourt_info$game_totals)
  
  return(irr(player_cfs, interval = NULL, cf.freq = 1))
  
}