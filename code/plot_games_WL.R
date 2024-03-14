plot_games_WL = function(player_name){
  results = total_contrib_WL(player_name)
  total = format(round(results[["sum_total"]], 3), nsmall = 2)
  GP = sum(results$game_totals > 0)
  per_game = format(round(results$sum_total / GP, 4), nsmall = 2)
  
  games = as.factor(c(1:length(results$game_totals)))
  plot_df = data.frame("games" = games,
                       "contributions" = results[["game_totals"]])
  
  g <- 
    ggplot(data=plot_df, aes(x=games, y=contributions))+
    geom_bar(stat="identity",fill="steelblue")+
    guides(x =  guide_axis(angle = 90))+
    ggtitle(paste(player_name,
                  " (PVWL: ",total,";",
                  " Per Game WinLogit: ",per_game,")",
                  sep=""))+
    xlab("Game Number")+
    ylab("WinLogit")+
    coord_cartesian(ylim=c(-0.025,0.20))+
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