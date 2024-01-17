define_positions_segments <- function(ordered_race)
{
  print(ordered_race)
  
  num_races <- length(ordered_race)
  
  position_all <- which(ordered_race == "All NHANES Women")
  
  df_ordered_positions <- data.frame(ordered_race = ordered_race) %>%
    mutate(position = seq(num_races)) %>%
    add_row(ordered_race = "All NHANES Women"
            , position = position_all) %>%
    arrange(position)
  
  position_all <- which(df_ordered_positions$ordered_race == "All NHANES Women")
  
  df_ordered_positions[position_all[1],"position"] <- df_ordered_positions[position_all[1],"position"] - 0.25 
  df_ordered_positions[position_all[2],"position"] <- df_ordered_positions[position_all[2],"position"] + 0.25 
  
  View(df_ordered_positions)
  
  return(df_ordered_positions)
}