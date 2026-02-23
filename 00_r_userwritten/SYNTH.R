SYNTH <- function(reference_df, ideology_df, fill_prefixes = c("rel_", "net_", "abs_", "rel_left_", "rel_right_")) {
  
  if (!"synthetic_origin" %in% names(ideology_df)) {
    ideology_df$synthetic_origin <- NA_character_
    ideology_df$synthetic_note <- NA_character_
  }
  
  for (i in seq_len(nrow(reference_df))) {
    orig <- reference_df$original_comune[i]
    new <- reference_df$new_comune[i]
    note <- reference_df$note[i]
    
    source_row <- ideology_df %>% filter(COMUNE == orig)
    if (nrow(source_row) == 0) next
    
    cols_to_fill <- names(ideology_df)[sapply(names(ideology_df), function(x) any(startsWith(x, fill_prefixes)))]
    
    if (new %in% ideology_df$COMUNE) {
      # Target exists → fill only missing values
      row_idx <- which(ideology_df$COMUNE == new)
      for (col in cols_to_fill) {
        if (is.na(ideology_df[[col]][row_idx])) {
          ideology_df[[col]][row_idx] <- source_row[[col]]
        }
      }
      ideology_df$synthetic_origin[row_idx] <- orig
      ideology_df$synthetic_note[row_idx] <- note
      
    } else {
      # Add full new row
      new_row <- source_row %>%
        mutate(
          COMUNE = new,
          synthetic_origin = orig,
          synthetic_note = note
        )
      ideology_df <- bind_rows(ideology_df, new_row)
    }
  }
  
  return(ideology_df)
}
