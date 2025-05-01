avengers_run <- function(data, start, alt) {
  # Create a list of function names
  filter_functions <- paste0("filter_avengers_ex", 15:26)
  
  # Apply each function dynamically and store results in a list
  avengers_list <- lapply(filter_functions, function(f) {
    do.call(f, list(data, start, alt))
  })
  
  # Combine all results into one dataframe
  avengers <- bind_rows(avengers_list)
  
  return(avengers)
}
