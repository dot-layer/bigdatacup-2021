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
                       season_years2 = as.integer(substr(sequence$season_years,6,7)),
                       # game_datetime = sequence$game_datetime,
                       team_abbreviation = sequence$team_abbreviation,
                       # opponent_abbreviation = sequence$opponent_abbreviation,
                       # team_status = sequence$team_status,
                       # period_label = sequence$period_label,
                       period_type = factor(sequence$period_type),
                       faceoff_zone = factor(sequence$faceoff_zone),
                       faceoff_won = factor(sequence$faceoff_win),
                       # clock = clock,
                       time_since_faceoff = seq(length(clock))-1,
                       goal_for_scored =     c(rep(0,length(clock)-1),sequence$result_goal_for*(!trunc)),
                       goal_against_scored = c(rep(0,length(clock)-1),sequence$result_goal_against*(!trunc)))
  
  # relevel(seq_dt$faceoff_won, ref = FALSE)
  return(seq_dt)
}
