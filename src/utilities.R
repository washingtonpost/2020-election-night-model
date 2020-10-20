get_preprocessed_data = function() {
  df = read_csv(paste('..','data', 'turnout_2012.csv', sep='/')) %>%
    rename(last_election_results=results) %>%
    mutate(total_voters=last_election_results)
  return(df)
}