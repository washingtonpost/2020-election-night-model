library(readr)
library(MLmetrics)
library(ggplot2)
library(gridExtra)

source('model.R')

data = read_csv('../data/turnout_2016.csv')

state_results = data %>% 
  group_by(postal_code) %>% 
  summarize(results=sum(results), .groups='drop')

initial_pred = read_csv('../data/turnout_2012.csv') %>%
  group_by(postal_code) %>%
  summarize(results=sum(results), .groups='drop')

state_mae = MAE(initial_pred$results, state_results$results)
state_mape = MAPE(initial_pred$results, state_results$results)
state_within_pi = NA

results_df = data.frame(n_reported=c(0), mae=c(state_mae), mape=c(state_mape), within_pi=c(state_within_pi))
set.seed(42)

shuffle = sample(nrow(data))
data_shuffled = data[shuffle,]

fixed_effects = c("postal_code")
robust = FALSE
model_settings = list(fixed_effects=fixed_effects, robust=robust)

for (i in seq(20, nrow(data_shuffled), 50)) {
  print(i)
  data_shuffled_observed = data_shuffled[1:i,] %>% mutate(precincts_reporting_pct=100)
  data_shuffled_unobserved = data_shuffled[(i+1):nrow(data_shuffled),] %>% mutate(precincts_reporting_pct=0, results=0)
  data_shuffled_i = data_shuffled_observed %>% rbind(data_shuffled_unobserved)
  
  prediction = estimate(data_shuffled_i, model_settings, )
  
  state_mae = MAE(prediction$state_data$pred, state_results$results)
  state_mape = MAPE(prediction$state_data$pred, state_results$results)
  state_within_pi = mean((prediction$state_data['upper_0.8'] >= state_results$results) & (prediction$state_data['lower_0.8'] <= state_results$results))
  
  results_df = results_df %>% add_row(n_reported=i, mae=state_mae, mape=state_mape, within_pi=state_within_pi)
}

mae_plot_state = ggplot(data=results_df, aes(x=n_reported, y=mae)) +
  geom_line() + 
  labs(title="MAE - state")

mape_plot_state = ggplot(data=results_df, aes(x=n_reported, y=mape)) +
  geom_line() + 
  labs(title="MAPE - state")

within_pi_state = ggplot(data=results_df, aes(x=n_reported, y=within_pi)) +
  geom_line() + 
  geom_hline(yintercept=0.8) +
  labs(title="Within PI - state")

grid.arrange(mae_plot_state, mape_plot_state, within_pi_state)

