require(stringr) #convert minutes to usable formats

#convert minutes to sum-able format
min_convert = function(game_min){
  
  if(nchar(game_min) %in% c(4,5)){
    min = as.numeric(str_split(game_min, fixed(":"))[[1]][1])
    sec = as.numeric(str_split(game_min, fixed(":"))[[1]][2])
  }
  else if(nchar(game_min) > 5){
    min = 0
    sec = 0
  }
  return(min + (sec/60))
  
}