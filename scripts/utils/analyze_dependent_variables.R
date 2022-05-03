# Load libraries, data, define flags, etc #####################################

## Load all the libraries ----------------------------------------------------
source('./scripts/utils/load_all_libraries.R')
source('./scripts/utils/functions_for_fitting_learning_curves.R')


## Load the data -------------------------------------------------------------

session_results_all_ptp <- import(
        './results/pilots/preprocessed_data/session_results_long_form.csv')

## Define flags --------------------------------------------------------------

# If learning fit bounds don't exist
if (!exists('i_lower')){
        
        # Create parameters as starting points for estimations
        i_start <- 0.5
        c_start <- 0.1
        
        # Create lower and upper bound constraints on the asymptote and learning rate
        c_lower <- 0
        c_upper <- 20
        i_lower <- 0
        i_upper <- 1
        
}

# If qc_filter variable doesnt exist, create it
if (!exists('qc_filter')){
        
        qc_filter <- F
        
}


if (qc_filter){
        
        # Exclude the one participant that did not pay attention to instructions
        session_results_all_ptp <-
                session_results_all_ptp %>%
                filter(ptp != '609478fa9e5b4d075246cfaf') %>%
                droplevels()
        
}

if (!exists('exclude_border')){
        
        # Load the data 
        exclude_border <- F
        
}

# Reorder the condition levels 
session_results_all_ptp <- session_results_all_ptp %>%
        reorder_levels(condition, order = c('practice',
                                            'practice2',
                                            'schema_c',
                                            'schema_ic',
                                            'landmark_schema',
                                            'random_locations',
                                            'no_schema'))

## Exclude the practice trials -----------------------------------------------
session_results_all_ptp <- session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        droplevels()

## Exclude specific border items? ----------------------------------------------
if (exclude_border){
        
        # Exclude close to border items
        session_results_all_ptp <- session_results_all_ptp %>%
                filter(!border_dist %in% border_dist_to_exclude) %>%
                droplevels()
        
}

# Start analysis ###############################################################

## Create long form accuracy type
session_results_all_ptp_long_accuracy <- 
        session_results_all_ptp %>%
        pivot_longer(cols = c(starts_with("correct_"),'mouse_dist_euclid'),
                     names_to = 'accuracy_type',
                     values_to = 'accuracy_value') %>%
        mutate(accuracy_type = as.factor(accuracy_type)) %>%
        reorder_levels(accuracy_type, order = c(
                'mouse_dist_euclid',
                'correct_exact',
                'correct_one_square_away',
                'correct_rad_21',
                'correct_rad_42',
                'correct_rad_63',
                'correct_rad_84',
                'correct_rad_105'
        ))

## Create one large long form for image repetitions
mean_by_rep_long <- 
        session_results_all_ptp_long_accuracy %>%
        droplevels() %>% 
        group_by(ptp_trunk,
                 condition,
                 new_pa_img_row_number_across_sessions,
                 accuracy_type) %>% 
        summarise(correct_mean = mean(accuracy_value, na.rm = T),
                  correct_sd = sd(accuracy_value, na.rm = T),
                  correct_n = n()) %>%
        ungroup()


# Calculate mean for neighbor vs non neighbor
mean_by_landmark_rep_long <-
        session_results_all_ptp_long_accuracy %>%
        droplevels() %>%
        group_by(ptp_trunk,
                 condition,
                 adjascent_neighbor,
                 new_pa_img_row_number_across_sessions,
                 accuracy_type) %>%
        summarise(correct_mean = mean(accuracy_value, na.rm = T),
                  correct_sd = sd(accuracy_value, na.rm = T),
                  correct_n = as.numeric(n())) %>%
        ungroup() %>%
        mutate(across(c(correct_mean,
                        correct_sd,
                        correct_n),
                      ~ case_when(
                              is.na(adjascent_neighbor) ~ as.numeric(NA),
                              TRUE ~ .
                      )))


# Pivot into wide form, so we can later merge with the other data reflecting overall performance
mean_by_landmark_rep_long_wide <- mean_by_landmark_rep_long %>%
        filter(!condition %in% c('random_locations',
                                 'no_schema')) %>%
        droplevels() %>%
        pivot_wider(id_cols = c(ptp_trunk,
                                condition,
                                new_pa_img_row_number_across_sessions,
                                accuracy_type),
                    values_from = c(correct_mean,
                                    correct_sd,
                                    correct_n),
                    names_from = adjascent_neighbor,
                    names_prefix = 'neighbor_'
        )

# Now merge into one giant dataset
mean_by_rep_all_types <- merge(mean_by_rep_long,
                               mean_by_landmark_rep_long_wide,
                               by = c('ptp_trunk',
                                      'condition',
                                      'new_pa_img_row_number_across_sessions',
                                      'accuracy_type'),
                               all = TRUE)

## Go from wide to long for both/near/far-PA -----------------------------------

# Pivot longer, but we have to do three columns, so break this up into two parts, then merge.
# Its possible to do this in one line, using names_pattern, but that needs complicated regular expressions
mean_by_rep_all_types_long_1 <-
        mean_by_rep_all_types %>%
        select(-contains(c('sd','correct_n'))) %>% 
        rename(both = correct_mean,
               island = correct_mean_neighbor_FALSE,
               neighbor = correct_mean_neighbor_TRUE) %>%
        pivot_longer(cols = c('both','island','neighbor'),
                     names_to = 'neighbor_status',
                     values_to = 'correct_mean',
        )
mean_by_rep_all_types_long_2 <-
        mean_by_rep_all_types %>%
        select(-contains(c('mean','correct_n'))) %>%
        rename(both = correct_sd,
               island = correct_sd_neighbor_FALSE,
               neighbor = correct_sd_neighbor_TRUE) %>%        
        pivot_longer(cols = c('both','island','neighbor'),
                     names_to = 'neighbor_status',
                     values_to = 'correct_sd',
        )

mean_by_rep_all_types_long_3 <-
        mean_by_rep_all_types %>%
        select(-contains(c('mean','sd'))) %>%
        rename(both = correct_n,
               island = correct_n_neighbor_FALSE,
               neighbor = correct_n_neighbor_TRUE) %>%        
        pivot_longer(cols = c('both','island','neighbor'),
                     names_to = 'neighbor_status',
                     values_to = 'n',
        )

mean_by_rep_all_types_long <-
        merge(mean_by_rep_all_types_long_1,
              
              merge(mean_by_rep_all_types_long_2,
                    mean_by_rep_all_types_long_3,
                    by = c('ptp_trunk',
                           'condition',
                           'new_pa_img_row_number_across_sessions',
                           'accuracy_type',
                           'neighbor_status')),
              
              by = c('ptp_trunk',
                     'condition',
                     'new_pa_img_row_number_across_sessions',
                     'accuracy_type',
                     'neighbor_status'))


# Add 95% CI for each calculation
mean_by_rep_all_types_long <- 
        mean_by_rep_all_types_long %>%
        mutate(ci_95 = 1.96*correct_sd/sqrt(n))


## Calculate for each distance from border -----------------------------------

mean_by_border_dist_rep_long <-
        session_results_all_ptp_long_accuracy %>%
        droplevels() %>%
        group_by(ptp_trunk,
                 condition,
                 border_dist,
                 new_pa_img_row_number_across_sessions,
                 session,
                 accuracy_type) %>%
        summarise(correct_mean = mean(accuracy_value, na.rm = T),
                  correct_sd   = sd(accuracy_value, na.rm = T),
                  correct_n    = as.numeric(n())) %>%
        ungroup()

# Fit the learning curves #############################################
learning_and_intercept_each_participant <-
        mean_by_rep_all_types_long %>%
        filter(!(condition %in% c('no_schema','random_locations') & 
                         neighbor_status %in% c('island','neighbor'))) %>%  # filter these, cause for those conditions there are no landmarks
        group_by(ptp_trunk,
                 condition,
                 neighbor_status,
                 accuracy_type) %>% 
        do(as.data.frame(
                optim(c(i_start,c_start),
                      fit_learning_and_intercept,
                      gr = NULL,
                      seq(1,8),
                      .$correct_mean,
                      'sse',
                      method = 'L-BFGS-B',
                      lower = c(i_lower,c_lower),
                      upper = c(i_upper,c_upper)
                )) %>%
                   mutate(id = row_number()) %>%
                   pivot_wider(names_from = id,
                               values_from = par,
                               names_prefix = 'par_')) %>%
        rename(sse = value,
               n_iterations = counts,
               i = par_1,
               c = par_2) %>%
        ungroup()

# Perform log transformation of the learning rate
learning_and_intercept_each_participant <- 
        learning_and_intercept_each_participant %>%
        mutate(c_log = log(c))
# This will result in Inf for those c==0. Do empirical log-odds?
smallest_c_log <- learning_and_intercept_each_participant$c_log[!is.infinite(learning_and_intercept_each_participant$c_log)] %>% min()
learning_and_intercept_each_participant <-
        learning_and_intercept_each_participant %>%
        mutate(c_log = case_when(
                is.infinite(c_log) ~ smallest_c_log,
                TRUE ~ c_log
        ))

# Add the predicted data to the dataframe
learning_and_intercept_each_participants_y_hat <-
        learning_and_intercept_each_participant %>%
        group_by(ptp_trunk,
                 condition,
                 neighbor_status,
                 accuracy_type) %>% 
        mutate(y_hat_i_c = list(fit_learning_and_intercept(c(i,c),
                                                           seq(1:8),
                                                           seq(1:8),
                                                           'fit')),
               new_pa_img_row_number_across_sessions = list(seq(1:8))) %>%
        unnest(c(y_hat_i_c,
                 new_pa_img_row_number_across_sessions)) %>% 
        select(c(ptp_trunk,
                 condition,
                 neighbor_status,
                 accuracy_type,
                 y_hat_i_c,
                 new_pa_img_row_number_across_sessions)) %>%
        ungroup()

mean_by_rep_all_types_long <- merge(mean_by_rep_all_types_long,
                                    learning_and_intercept_each_participants_y_hat,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'neighbor_status',
                                           'accuracy_type',
                                           'new_pa_img_row_number_across_sessions'),
                                    all = TRUE)

# Rough measures for learning #########################################

## For both/near/far-PA -----------------------------------------------

last_two_reps_stats <-
        mean_by_rep_all_types_long %>%
        filter(new_pa_img_row_number_across_sessions %in% c(7,8)) %>%
        group_by(ptp_trunk,
                 condition,
                 neighbor_status,
                 accuracy_type) %>%
        summarise(last_two_mean = mean(correct_mean),
                  last_two_sd   = sd(correct_sd)) %>%
        ungroup()

last_four_reps_stats <-
        mean_by_rep_all_types_long %>%
        filter(new_pa_img_row_number_across_sessions %in% c(5,6,7,8)) %>%
        group_by(ptp_trunk,
                 condition,
                 neighbor_status,
                 accuracy_type) %>%
        summarise(last_four_mean = mean(correct_mean),
                  last_four_sd   = sd(correct_sd)) %>%
        ungroup()

# Create one variable, that will have all the dependent variables
sum_stats_each_participant <- merge(last_two_reps_stats,
                                    last_four_reps_stats,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'neighbor_status',
                                           'accuracy_type'))

# Log transform the last 2 and last 4 measures
sum_stats_each_participant <- sum_stats_each_participant %>%
        mutate(log_last_two_mean = log(last_two_mean),
               log_last_four_mean = log(last_four_mean))
# If any of them are Inf values, substitute with the lowest value.

sum_stats_each_participant <- sum_stats_each_participant %>%
        mutate(log_last_two_mean = case_when(
                is.infinite(log_last_two_mean) ~ min(log_last_two_mean*is.finite(log_last_two_mean),na.rm = T),
                TRUE ~ log_last_two_mean),
               log_last_four_mean = case_when(
                       is.infinite(log_last_four_mean) ~ min(log_last_four_mean*is.finite(log_last_four_mean),na.rm = T),
                       TRUE ~ log_last_four_mean))

sum_stats_each_participant <- merge(sum_stats_each_participant,
                                    learning_and_intercept_each_participant,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'neighbor_status',
                                           'accuracy_type'),
                                    all = TRUE)

# Remove extra variables #####################################################
rm(mean_by_rep_all_types_long_1)
rm(mean_by_rep_all_types_long_2)
rm(mean_by_rep_all_types_long_3)
rm(mean_by_landmark_rep_long)
rm(mean_by_landmark_rep_long_wide)
rm(mean_by_rep_long)





