rm(list=ls())


pacman::p_load(pacman,
               rio,
               tidyverse,
               rstatix,
               DT,
               kableExtra,
               readr,
               writexl,
               jsonlite,
               stringr,
               gridExtra,
               knitr,
               magrittr,
               pdist,
               gghighlight)


# Load the data ##########################
session_results_all_ptp <- import(
        './results/pilots/preprocessed_data/session_results_long_form.csv'
)

session_results_all_ptp <- session_results_all_ptp %>%
        reorder_levels(condition, order = c('practice',
                                            'practice2',
                                            'schema_c',
                                            'schema_ic',
                                            'landmark_schema',
                                            'random_locations',
                                            'no_schema')
        )


# Load an external script which contains functions for estimating either just the learning rate, or also the asymptote
source('./scripts/utils/functions_for_fitting_learning_curves.R')

# Create parameters as starting points for estimations
i_start <- 0.5
c_start <- 0.1

# Create lower and upper bound constraints on the asymptote and learning rate
c_lower <- 0
c_upper <- 20
i_lower <- 0
i_upper <- 1

qc_filter <- T

# Do all the calculations of all the dependent variables

source('./scripts/utils/analyze_dependent_variables.R')

# Now, take the ones where convergence failed, and individually fit their data ############################################

cond <- 'schema_c'
ptp  <- '6047ae5...'

idf <- mean_by_rep_all_types_long %>%
        filter(condition == cond,
               ptp_trunk == ptp,
               accuracy_type == 'correct_one_square_away',
               new_pa_status == 'both')

# Now fit this individually

optim(c(i_start,c_start),
      fit_learning_and_intercept,
      gr = NULL,
      seq(1,8),
      idf$correct_mean,
      'sse',
      method = 'L-BFGS-B',
      lower = c(i_lower,c_lower),
      upper = c(i_upper,c_upper),
      control=list(trace=6)
      )






























