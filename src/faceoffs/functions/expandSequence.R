# sequence <- sequences[1,]

expandSequence <- function(sequence, seq_id = NULL){
  
  # clock time  
  seq_dt <- data.table(seq_id = seq_id,
                       clock = seq(sequence$clock_begin,sequence$clock_end,-1))
  
  # time since faceoff
  seq_dt[, time_since_faceoff := 0:(nrow(seq_dt)-1)]
  
  # situation
  seq_dt[, situation := paste(sequence$team_skaters_nb,
                              sequence$opponent_skaters_nb,
                              sep = "-")]
  
  # goal scored
  seq_dt[, goal_for_scored := c(rep(0,nrow(seq_dt)-1),sequence$result_goal_for)]
  seq_dt[, goal_against_scored := c(rep(0,nrow(seq_dt)-1),sequence$result_goal_against)]
  
  # faceoff zone
  seq_dt[, faceoff_zone := sequence$faceoff_zone]
  
  # faceoff won
  seq_dt[, faceoff_won := factor(sequence$faceoff_win, levels = c(FALSE, TRUE))]
  
  return(seq_dt)
}
