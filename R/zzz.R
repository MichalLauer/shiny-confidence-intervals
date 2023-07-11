fmt <- \(x, width = 16) str_pad(sprintf(paste0('%0.', width - 3,'f'), x), width = width)

streak <- function(x) {
  streak_str <- NULL
  if (sum(!x) == 0) {
    streak_str <- ""
  } else {
    runs <- rle(x)
    longest_streak <- max(runs$lengths[runs$values == FALSE])
    index <- which(runs$values == FALSE & runs$lengths == longest_streak)
    position <- sapply(index, \(x) sum(runs$lengths[1:x]) - longest_streak + 1)
    streak_str <- glue("Longest streak of incorrect guesses: ",
                       "{longest_streak} (Starting at {combine_words(position)})")
  }
  return(streak_str)
}
