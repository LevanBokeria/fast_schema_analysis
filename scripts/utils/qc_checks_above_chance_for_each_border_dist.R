# Description #####################################################

# Load the chance level for border distance c(3,4,5) for each participant.
# For each participant, check separately whether each border distance PAs on their own are above the combined chance level for c(3,4,5)

# The idea is that we want to make sure the participants are doing above chance for 3 and 4, cause those are the near- vs far-PAs.


# Clean the environment and load libraries ############################

rm(list=ls())

source('./scripts/utils/load_all_libraries.R')


# Load the data #########################################
session_results_all_ptp <- import('./results/pilots/preprocessed_data/session_results_long_form.csv')

session_results_all_ptp <- session_results_all_ptp %>%
        mutate(ptp_trunk = as.factor(str_trunc(ptp, width = 10))) %>% 
        reorder_levels(condition, order = c('practice',
                                            'practice2',
                                            'schema_c',
                                            'schema_ic',
                                            'landmark_schema',
                                            'random_locations',
                                            'no_schema'))
        
# Define flags and filtering conditions ######################################

border_away_to_analyze <- c(1,2,3,4,5) # so KEEP these

saveData <- F
# Load the data ###########################################################

results_bound <- import(
        paste0(
                './results/pilots/preprocessed_data/qc_permutations_raw_across_conditions_border_dist_',
                paste(border_away_to_analyze,collapse = '_'),
                '.csv'
        ))

df_percentile <- import(
        paste0(
                './results/pilots/preprocessed_data/qc_permutations_summary_across_conditions_border_dist_',
                paste(border_away_to_analyze,collapse = '_'),
                '.csv'
        )
)

niter <- df_percentile$n_perm[1]


# Check if above for each border distance ########################################################

# 1. Get the 95th percentile for each participant
percentile_95 <- results_bound %>%
        group_by(ptp_trunk) %>%
        summarise(n_perm = n(),
                  percentile_95 = quantile(mean_correct_exact_shuff, probs = c(0.95)))

# 2. Get mean performance by each border dist
results <- session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2'),
               session == 2) %>% 
        group_by(ptp_trunk,border_dist) %>%
        summarise(mean_correct_one_square_away = mean(correct_one_square_away, na.rm = T),
                  mean_correct_exact           = mean(correct_exact, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(pass_threshold_correct_exact = as.numeric(mean_correct_exact >= percentile_95$percentile_95[1]))

# Plot ------------------------------------------------------------------------

results %>%
        group_by(border_dist,
                 pass_threshold_correct_exact) %>%
        summarise(n = n()) %>% 
        ungroup() %>%
        ggplot(aes(x=as.factor(border_dist),y=n,fill=as.factor(pass_threshold_correct_exact))) +
        geom_bar(stat = 'identity') +
        scale_y_continuous(breaks = seq(0,12))


