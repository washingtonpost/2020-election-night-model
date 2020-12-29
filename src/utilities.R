get_preprocessed_data = function(geographic_unit_type) {
  name = sprintf("%s_turnout_baseline.csv", geographic_unit_type)
  df = read_csv(paste('..','data', name, sep='/')) %>%
    rename(last_election_results=results) %>%
    mutate(total_voters=last_election_results)
  return(df)
}