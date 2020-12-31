library(dplyr)
library(quantreg)
library(readr)
library(tidyr)
library(tibble)
library(boot)

source('utilities.R')
source('model_utilities.R')

ALL_FEATURES = c("age_le_30", "age_geq_30_le_45", "age_geq_45_le_65", "age_geq_65", 
                 "ethnicity_east_and_south_asian", "ethnicity_european", "ethnicity_hispanic_and_portuguese", 
                 "ethnicity_likely_african_american", "ethnicity_other", 
                 "gender_f", "gender_m", "median_household_income", "percent_bachelor_or_higher")

fit_quantile_reg_model = function(data, tau, weights) {
  reg = rq(residuals ~ . - 1 - total_voters, data=data, tau=tau, weights=weights)
  return(reg)
}

get_unit_predictions = function(observed_data, unobserved_data, model_settings) {

  observed_data_features = observed_data %>%
    select(c(all_of(model_settings$fixed_effects), intercept, residuals, total_voters))
  unobserved_data_features = unobserved_data %>%
    select(c(all_of(model_settings$fixed_effects), intercept, total_voters))
  
  weights = observed_data_features$total_voters
  observed_data_features = observed_data_features %>% 
  mutate(residuals=residuals / total_voters)
  
  reg = fit_quantile_reg_model(observed_data_features, 0.5, weights)

  preds = predict(reg, unobserved_data_features)

  # add in last election results to move from residual to number of vote space
  # pmax with unobserved results, so that predictions are at least as large as # of votes seen
  preds = preds * unobserved_data_features$total_voters
  preds = pmax(preds + unobserved_data$last_election_results, unobserved_data$results)
  
  return(preds)
}

# this function takes in the calibration data set, the unobserved data set
# and the aggregation method 
get_gaussian_model = function(c_data, observed_data, unobserved_data, aggregate=c(), alpha=0.9, reweight=FALSE) {
  if (nrow(c_data) == 0) {
    empty_tibble = c_data %>%
      select(all_of(aggregate)) %>%
      add_column(mu_lb=numeric(),
                 mu_ub=numeric(),
                 sigma_lb=numeric(),
                 sigma_ub=numeric(),
                 var_inflate=numeric())
    return(empty_tibble)
  }
  if (reweight) {
    # TODO: implement reweight
  } else {
    counts = c_data %>% 
      count(.dots=aggregate)
    # if one group has fewer than MODEL_THRESHOLD observations to fit with, 
    # remove one layer of aggregation and refit the parametric model
    # e.g. instead of fitting a parametric model for each county in a state...produce
    # a parametric model for the state
    MODEL_THRESHOLD = min(10, nrow(c_data))
    if (min(counts$n) < MODEL_THRESHOLD) {
      # model for small groups (ie. ones where count < MODEL_THRESHOLD)
      g_model_small = get_gaussian_model(c_data, observed_data, unobserved_data, 
                                         head(aggregate, -1), alpha, reweight)
      # still construct the per-group parametric model when possible
      c_data_in_large_aggregates = counts %>%
        filter(n >= MODEL_THRESHOLD) %>%
        inner_join(c_data, by=aggregate) %>%
        filter(n < 0) # comment me out to run
      g_model_large = get_gaussian_model(c_data_in_large_aggregates, observed_data, unobserved_data,
                                         aggregate, alpha, reweight)
      g_model = g_model_small %>% 
        bind_rows(g_model_large)
    } else {
      # when the group is large enough we can compute the Gaussian model for conformalization
      g_model = c_data %>% 
        group_by(.dots=aggregate) %>% 
        summarize(var_inflate = sum(total_voters^2) / sum(total_voters)^2, # gamma
                  mu_lb = sum(total_voters * c_lb) / sum(total_voters),
                  mu_ub = sum(total_voters * c_ub) / sum(total_voters),
                  sigma_lb = boot_sigma(c_lb, conf=(3 + alpha) / 4),
                  sigma_ub = boot_sigma(c_ub, conf=(3 + alpha) / 4), .groups="drop")
    }
  }
  return(g_model)
}


# this computes conformal prediction intervals. we only get marginal (not conditional prediction interval).
get_unit_prediction_intervals = function(observed_data, unobserved_data, alpha, model_settings, fixed_effects) {
  n_fixed_effects = length(fixed_effects)
  
  # we need the contraint rows to be in t_data. so I remove them from observed_data
  if (n_fixed_effects > 0) {
    fixed_effects_constraint_rows = tail(observed_data, n_fixed_effects)
    observed_data = head(observed_data, dim(observed_data)[1] - n_fixed_effects)
  }
  
  shuffle = sample(nrow(observed_data)) # shuffle to create training and conformal set
  observed_data_shuffled = observed_data[shuffle,]
  conf_frac = min(1 - (alpha / 0.05) / nrow(observed_data_shuffled), 0.9) # fraction for conformal is at least 10%
  t_rows = floor(nrow(observed_data_shuffled) * conf_frac)
  t_data = observed_data_shuffled[1:t_rows,] # first t_rows are training set
  
  if (n_fixed_effects > 0) {
    # since we split observed_data_features it is now possible that some fixed effect columns will be all zero. We drop those to avoid singular design issues
    t_data = t_data %>% select(where(~ any(. != 0))) # drop column if all examples are in c_data
    # now the columns between t_data and the contraint rows are different. so we only take the columns from the constraint
    # rows that also appear in t_data and then row bind the to the end
    t_data = t_data %>% rbind((fixed_effects_constraint_rows %>% select(colnames(t_data))))
  }
  
  c_data = observed_data_shuffled[(t_rows + 1):nrow(observed_data_shuffled), ] # other rows are conformal set
  t_data = t_data %>% mutate(residuals=residuals / total_voters) # scaled error
  c_data = c_data %>% mutate(residuals=residuals / total_voters) # scaled error
  weights = t_data$total_voters
  
  upper = (1 + alpha) / 2
  lower = (1 - alpha) / 2
  
  t_data_features = t_data %>%
    select(c(all_of(model_settings$features), all_of(model_settings$fixed_effects), intercept, residuals, total_voters))

  reg_bounds = fit_quantile_reg_model(t_data_features, c(lower, upper), weights)

  c_data_features = c_data %>%
    select(c(all_of(model_settings$features), all_of(model_settings$fixed_effects), intercept, residuals, total_voters))
  c_bounds = predict(reg_bounds, c_data)
  
  unobserved_data_features = unobserved_data %>%
    select(c(all_of(model_settings$features), all_of(model_settings$fixed_effects), intercept, total_voters))
  uo_bounds = predict(reg_bounds, unobserved_data_features)
  
  # c_bounds is guess for upper and lower bounds
  # c_ub and c_lb how much we miss are outside of prediction intervals
  c_ub = c_data$residuals - c_bounds[,2]
  c_lb = c_bounds[,1] - c_data$residuals
  
  to_return = list()
  
  if (model_settings$pi_method == 'nonparametric') {
    scores = pmax(c_lb, c_ub) # this is e_j
    
    # our desired coverage is that alpha-% percentile of e_j should be less than 0 
    # this is roughly equivalent to alpha-% of c_data is covered by initial guess
    # to get there we need to add the correction, which is equal to alpha * (1 + 1/nrow(c_data))
    correction = quantile(scores, probs=c(alpha * (1 + 1/nrow(c_data))))
    
    # we care about larger units more than units counties when computing state
    # prediction intervals. to accomplish this, we will weight the i-th score by the 
    # number of voters in that unit in the previous election 
    weights = c_data$total_voters / sum(c_data$total_voters)
    pop_corr = as_tibble(cbind(scores, weights)) %>% arrange(scores) %>% 
      mutate(perc = cumsum(weights)) %>% filter(perc > alpha * (1 + 1/nrow(c_data)))
    pop_corr = min(pop_corr$scores)
    if (model_settings$robust) {
      correction = max(correction, pop_corr)
    } else {
      correction = pop_corr # "unbiased" state PIs
    }
    
    # apply correction
    lower = uo_bounds[,1] - correction
    upper = uo_bounds[,2] + correction
  } else if (model_settings$pi_method == 'gaussian') {
    c_data = cbind(c_data, c_lb, c_ub)
    
    # fit gaussian model to nonconformity scores
    g_model = get_gaussian_model(c_data, observed_data, unobserved_data, aggregate=c(), alpha=alpha)
    
    # parametric model for a single unit prediction interval
    lower = uo_bounds[,1] - qnorm((3 + alpha) / 4, mean=g_model$mu_lb, 
                                  sd=sqrt(g_model$var_inflate + 1) * g_model$sigma_lb)
    upper = uo_bounds[,2] + qnorm((3 + alpha) / 4, mean=g_model$mu_ub, 
                                  sd=sqrt(g_model$var_inflate + 1) * g_model$sigma_ub)
    
    to_return[['c_data']] = c_data
    to_return[['uo_bounds']] = uo_bounds
  }
  
  # move bounds back from normalized residual to residual space
  lower = lower * unobserved_data$total_voters
  upper = upper * unobserved_data$total_voters
  
  # add in last election results to move from residual to number of vote space
  # pmax with unobserved results, so that bounds are at least as large as # of votes seen
  to_return[['lower']] = pmax(lower + unobserved_data$last_election_results, unobserved_data$results)
  to_return[['upper']] = pmax(upper + unobserved_data$last_election_results, unobserved_data$results)
  
  return(to_return)
}

# this function gets the aggregate votes for geographic units made up of counties.
# it sums to the required level (state, cd, county category) and then adds in the unecpected data if necessary.
get_observed_votes_aggregate = function(observed_data, observed_unexpected_data, aggregate) {
  observed_data_known_votes = observed_data %>% 
    group_by(.dots=aggregate) %>%
    summarize(observed_data_votes=sum(results), .groups='drop')
  
  observed_unexpected_data_known_votes = observed_unexpected_data %>%
    group_by(.dots=aggregate) %>%
    summarize(observed_unexpected_data_votes=sum(results), .groups='drop')
  
  # the full join here makes sure that if entire congressional districts or county categories
  # are unexpectedly present, we will capture them. This is also why we need to replace_NA with zero
  # NA here means that there were no such counties, so they contribute zero votes.
  aggregate_votes = observed_data_known_votes %>%
    full_join(observed_unexpected_data_known_votes, by=aggregate) %>%
    replace_na(list(observed_data_votes=0, observed_unexpected_data_votes=0)) %>%
    mutate(results=observed_data_votes + observed_unexpected_data_votes) %>%
    select(all_of(c(aggregate, 'results')))
  return(aggregate_votes)
}

# This function returns the predictions for state, cd, county category
# predictions are made up of known results in observed counties (also within the prediction column)
# and predicted votes for unobserved counties
get_aggregate_predictions = function(observed_data, unobserved_data, observed_unexpected_data, aggregate) {
  
  # first get only the aggregate votes from observed counties (both expected and unexpevted)
  aggregate_votes = get_observed_votes_aggregate(observed_data, observed_unexpected_data, aggregate)
  
  aggregate_preds = unobserved_data %>%
    group_by(.dots=aggregate) %>%
    summarize(pred_only=sum(pred), .groups='drop')
  
  # the full join accounts for if entire states, cd, or county categories are either observed or predicted. 
  aggregate_data = aggregate_votes %>%
    full_join(aggregate_preds, by=aggregate) %>%
    replace_na(list(results=0, pred_only=0)) %>%
    mutate(pred=results + pred_only) %>%
    arrange(!!!syms(aggregate)) %>% # this allows aggregate to be a vector of strings
    select(all_of(c(aggregate, 'pred')))
  
  return(aggregate_data)
}

# this function produces aggregate prediction intervals for state, cd, county category level
get_aggregate_prediction_intervals = function(observed_data, unobserved_data, observed_unexpected_data, aggregate, alpha, model_settings, c_data, uo_bounds) {
  lower_string = paste('lower', alpha, sep='_')
  upper_string = paste('upper', alpha, sep='_')
  
  aggregate_votes = get_observed_votes_aggregate(observed_data, observed_unexpected_data, aggregate)
  
  if (model_settings$pi_method == 'nonparametric') {
    # prediction intervals just sum, kinda miraculous
    aggregate_prediction_intervals = unobserved_data %>%
      group_by(.dots=aggregate) %>%
      summarize(predicted_lower=sum(!!sym(lower_string)), predicted_upper=sum(!!sym(upper_string)), .groups='drop')
    # since lower and upper string are always just strings, we can use !!sym(x) instead of !!!syms(x) as with aggregate
    
  } else if (model_settings$pi_method == 'gaussian') {
    last_election = unobserved_data %>%
      group_by(.dots=aggregate) %>%
      summarize(last_election_results=sum(last_election_results), .groups="drop")
    
    # fit gaussian model to aggregate case
    g_model = get_gaussian_model(c_data, observed_data, unobserved_data, aggregate, alpha, reweight=FALSE)
    
    bounds = cbind(unobserved_data, uo_lb=uo_bounds[,1], uo_ub=uo_bounds[,2]) 
    
    # aggregate the upper and lower quantile regression predictions
    # also aggregate sum w_i^2 and sum w_i for each group
    bounds = bounds %>%
      group_by(.dots=aggregate) %>%
      summarize(unobserved_aggregate_lb=sum(total_voters * uo_lb),
                unobserved_aggregate_ub=sum(total_voters * uo_ub),
                unobserved_weight_sum=sum(total_voters),
                unobserved_weight_ssum=sum(total_voters^2), .groups="drop")
    
    # first join gaussian model based on *all* aggregates (that is groups that had enough examples)
    modeled_bounds = bounds %>% 
      inner_join(g_model, by=aggregate)
    # for groups that did not have enough examples, we want to join the models trained on higher aggregations 
    # (ie. postal_code instead of postal_code, county_category)
    for (i in seq(length(aggregate) - 1, 0, by=-1)) {
      remaining_models = g_model %>%
        filter(
          across(
            tail(aggregate, length(aggregate) - i), # take last i aggregations
            ~ is.na(.x)
          ) # only keep models for which we have an NA in the last i aggregations
        ) %>%
        select(
          -all_of(
            tail(aggregate, length(aggregate) - i)
          )
        )
      remaining_bounds = bounds %>%
        anti_join(modeled_bounds, by=head(aggregate, i + 1)) # only select bounds that have not been matched to model yet
      modeled_bounds = modeled_bounds %>%
        bind_rows(remaining_bounds %>% #match unmatched bounds to higher aggregate models
                    inner_join(remaining_models, by=head(aggregate, i)))
    }
    
    # constructing conformal corrections using Gaussian
    # get means and standard deviations for aggregation
    modeled_bounds = modeled_bounds %>% 
      mutate(lb_mean=unobserved_weight_sum * mu_lb,
             lb_sd=sigma_lb * sqrt(unobserved_weight_ssum + var_inflate * unobserved_weight_sum^2),
             ub_mean=unobserved_weight_sum * mu_ub,
             ub_sd=sigma_ub * sqrt(unobserved_weight_ssum + var_inflate * unobserved_weight_sum^2)) %>%
      mutate(lb=unobserved_aggregate_lb - qnorm((3 + alpha) / 4, mean=lb_mean, sd=lb_sd),
             ub=unobserved_aggregate_ub + qnorm((3 + alpha) / 4, mean=ub_mean, sd=ub_sd)) %>%
      select(all_of(aggregate), lb, ub)
    
    # add correction
    aggregate_prediction_intervals = last_election %>%
      inner_join(modeled_bounds, by=aggregate) %>%
      mutate(predicted_lower = last_election_results + lb,
             predicted_upper = last_election_results + ub) %>%
      # pmax to avoid adding negative vote count in unobserved units
      mutate(predicted_lower=pmax(predicted_lower, 0), predicted_upper=pmax(predicted_upper, 0)) %>%
      select(-last_election_results)
  }

  aggregate_data = aggregate_votes %>%
    full_join(aggregate_prediction_intervals, by=aggregate) %>%
    replace_na(list(results=0, predicted_lower=0, predicted_upper=0)) %>%
    mutate(lower=predicted_lower + results, upper=predicted_upper + results) %>%
    arrange(!!!syms(aggregate)) %>%
    select(all_of(c(aggregate, 'lower', 'upper')))
  
  return(aggregate_data)
}

estimate = function(current_data, model_settings=list(fixed_effects=c(), robust=FALSE), prediction_intervals=c(0.8), geographic_unit_type='county') {
  fixed_effects = model_settings$fixed_effects

  preprocessed_data = get_preprocessed_data(geographic_unit_type) %>%
    select(postal_code, geographic_unit_fips, county_fips, last_election_results, total_voters)
  
  # joining current results to preprocessed data. This is a left_join on the preprocessed data, that means
  # if we didn't expect the county, we drop it. This is because we don't have any covariates for it.
  # we solve this by using observed_unexpected_data below, which we then add into state, cd, county category totals
  data = preprocessed_data %>% left_join(current_data, by=c("postal_code", "geographic_unit_fips", "county_fips"))
  
  observed_data = data %>%
    filter(precincts_reporting_pct >= 100) %>%   # these are the counties that we have observed
    mutate(residuals=results - last_election_results) %>% # residual
    mutate(intercept=1)
  
  unobserved_data = data %>% 
    filter(precincts_reporting_pct < 100 | is.na(results)) %>%
    mutate(residuals=NA) %>%
    mutate(intercept=1)
  
  # these are observed counties that we were not expecting (mostly for townships). We add these
  # results back in later to get state totals
  observed_unexpected_data = current_data %>% 
    filter(precincts_reporting_pct >= 100) %>%
    filter(!geographic_unit_fips %in% preprocessed_data$geographic_unit_fips)
  
  for (fixed_effect in fixed_effects) {
    # for each fixed effect we want to turn the string column into categorical variables. We need to do this, over using levels because
    # there might be unseen fixed effects that we don't want to learn anything for
    # to fix issues with with singular matrix design we add a constraint row, which has all fixed effects but no intercept and no residual
    # this contrains the solutions set.
    fixed_effect_prefix = paste(fixed_effect, '_', sep="")
    observed_data = observed_data %>%
      pivot_wider(names_from=fixed_effect, values_from=fixed_effect, names_prefix=fixed_effect_prefix) %>% #turn categorical variable into columns
      mutate_at(vars(starts_with(fixed_effect_prefix)), funs(ifelse(is.na(.), 0, 1))) %>% # replace values with 1 and NA with 0s
      select(-starts_with(paste(fixed_effect_prefix, '0', sep=""))) %>% # if we have more than one fixed effect, we have an additional zero column for the previous fixed effects that we want to get rid of
      add_row() %>% # we add a row full of NAs
      replace(is.na(.), 0) %>% # we replace the NAs with zero
      mutate_at(vars(starts_with(fixed_effect_prefix)), funs(case_when(intercept == 0 ~ 1, TRUE ~ .))) %>% # we set all possible fixed effects variables to 1, except the intercept that is 0
      mutate_at('total_voters', funs(case_when(intercept == 0 ~ 1, TRUE ~ .))) # we give that new row 1 total voter, so that when we compute weight we don't get (0/0=) NA
    
    unobserved_data = unobserved_data %>%
      mutate(row=row_number()) %>%
      pivot_wider(names_from=fixed_effect, values_from=fixed_effect, names_prefix=fixed_effect_prefix) %>% # we do the same for unobserved counties
      select(-row) %>%
      mutate_at(vars(starts_with(fixed_effect_prefix)), funs(ifelse(is.na(.), 0, 1)))
    
    # there might be fixed effect categories that appear in observed data only. we add those manully to unseen counties and set them to 0
    # we don't need to worry about fixed effect categories that appear in unseen data only, since we don't need to learn a coefficient for those
    for (col_name in colnames(observed_data_features)) {
      if (startsWith(col_name, fixed_effect_prefix) & !(col_name %in% colnames(unobserved_data))) {
        unobserved_data[col_name] = 0
      }
    }
  }
  
  # for observed counties prediction is just the results
  # for unobserved counties prediction is the estimated residual between the last and current election. So we 
  # add in the last election result to get our estimate for current election result. We max that with the results
  # we've seen so far to make sure the counted vote isn't more than the prediction.
  unit_predictions = get_unit_predictions(observed_data, unobserved_data, model_settings)
  observed_data['pred'] = observed_data$results
  unobserved_data['pred'] = unit_predictions
  observed_unexpected_data['pred'] = observed_unexpected_data$results
  
  # if we are doing precinct level model, we also want county predictions
  if (geographic_unit_type == 'precinct') {
    county_data = get_aggregate_predictions(observed_data, unobserved_data, observed_unexpected_data, c('postal_code', 'county_fips'))
  }

  # get aggregate predictions for state
  state_data = get_aggregate_predictions(observed_data, unobserved_data, observed_unexpected_data, 'postal_code')
  
  # for observed counties, lower and upper prediction intervals are predictions
  for (alpha in prediction_intervals) {
    lower_string = paste('lower', alpha, sep='_')
    upper_string = paste('upper', alpha, sep='_')
    
    unit_prediction_intervals = get_unit_prediction_intervals(observed_data, unobserved_data, alpha, model_settings, fixed_effects)
    observed_data[lower_string] = observed_data$results
    observed_data[upper_string] = observed_data$results
    unobserved_data[lower_string] = unit_prediction_intervals$lower
    unobserved_data[upper_string] = unit_prediction_intervals$upper
    observed_unexpected_data[lower_string] = observed_unexpected_data$results
    observed_unexpected_data[upper_string] = observed_unexpected_data$results
    
    c_data = unit_prediction_intervals$c_data
    uo_bounds = unit_prediction_intervals$uo_bounds
    if (geographic_unit_type == 'precinct') {
      county_prediction_intervals = get_aggregate_prediction_intervals(observed_data, unobserved_data, observed_unexpected_data, c('postal_code', 'county_fips'), alpha, model_settings, c_data, uo_bounds)
      county_data[lower_string] = county_prediction_intervals$lower
      county_data[upper_string] = county_prediction_intervals$upper
    }
    
    state_prediction_intervals = get_aggregate_prediction_intervals(observed_data, unobserved_data, observed_unexpected_data, 'postal_code', alpha, model_settings, c_data, uo_bounds)
    state_data[lower_string] = state_prediction_intervals$lower
    state_data[upper_string] = state_prediction_intervals$upper
  }
  
  unit_data = observed_data %>%
    bind_rows(unobserved_data) %>%
    bind_rows(observed_unexpected_data) %>%
    arrange(geographic_unit_fips) %>%
    select(postal_code, geographic_unit_fips, pred, starts_with('lower'), starts_with('upper'))
  
  to_return = list(unit_data=unit_data, state_data=state_data)
  
  if (geographic_unit_type == 'precinct') {
    to_return[['county_data']] = county_data
  }
  
  return(to_return)
}