# sequence <- sequences[1,]

expandSequenceNHL <- function(sequence, seq_id = NULL, max_time = NULL){
  
  # clock time
  clock <- seq(sequence$clock_begin,sequence$clock_end,-1)
  if(!is.null(max_time) & max_time + 1 < length(clock)){
    trunc <- T
    clock <- clock[1:(max_time+1)]
  }else{
    trunc <- F
  }
  
  seq_dt <- data.table(seq_id = seq_id,
                       season_years = sequence$season_years,
                       game_datetime = sequence$game_datetime,
                       team_abbreviation = sequence$team_abbreviation,
                       opponent_abbreviation = sequence$opponent_abbreviation,
                       team_status = sequence$team_status,
                       period_label = sequence$period_label,
                       period_type = sequence$period_type,
                       clock = clock)
  
  # time since faceoff
  seq_dt[, time_since_faceoff := 0:(nrow(seq_dt)-1)]
  
  # goal scored
  seq_dt[, goal_for_scored := c(rep(0,nrow(seq_dt)-1),sequence$result_goal_for*(!trunc))]
  seq_dt[, goal_against_scored := c(rep(0,nrow(seq_dt)-1),sequence$result_goal_against*(!trunc))]
  
  # faceoff zone
  seq_dt[, faceoff_zone := sequence$faceoff_zone]
  
  # faceoff won
  seq_dt[, faceoff_won := factor(sequence$faceoff_win, levels = c(FALSE,TRUE))]
  
  return(seq_dt)
}
