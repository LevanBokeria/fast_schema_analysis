# Load libraries, data, define flags, etc #####################################

## Load all the libraries ----------------------------------------------------
source('./scripts/utils/load_all_libraries.R')
source('./scripts/utils/functions_for_fitting_learning_curves.R')

options(error = recover)

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
        c_upper <- Inf
        i_lower <- 0
        i_upper <- Inf
        
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
        exclude_border <- T
        border_dist_to_exclude <- c(1,2,5)
}

if (!exists('saveData')){
        
        saveData <- T
        
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

# Drop the accuracy measures that we no longer analyze:
session_results_all_ptp <- session_results_all_ptp %>%
        select(-c( 'correct_rad_21',
                   'correct_rad_42',
                   'correct_rad_63',
                   'correct_rad_84',
                   'correct_rad_105'))


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
                'correct_one_square_away'
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


# Calculate mean for far_pa vs non far_pa
mean_by_landmark_rep_long <-
        session_results_all_ptp_long_accuracy %>%
        filter(!condition %in% c('random_locations','no_schema')) %>%
        droplevels() %>%
        group_by(ptp_trunk,
                 condition,
                 near_pa,
                 new_pa_img_row_number_across_sessions,
                 accuracy_type) %>%
        summarise(correct_mean = mean(accuracy_value, na.rm = T),
                  correct_sd = sd(accuracy_value, na.rm = T),
                  correct_n = as.numeric(n())) %>%
        ungroup() 

# Combine these
mean_by_rep_all_types_long <- bind_rows(mean_by_rep_long,mean_by_landmark_rep_long)

# Remove extra variables
rm(mean_by_rep_long)
rm(mean_by_landmark_rep_long)

# Rename the near pa column to new_pa_status
mean_by_rep_all_types_long <- mean_by_rep_all_types_long %>%
        mutate(new_pa_status = case_when(
                is.na(near_pa) ~ 'both',
                near_pa == TRUE ~ 'near_pa',
                near_pa != TRUE ~ 'far_pa',
                TRUE ~ as.character(NA)
        )) %>%
        select(-near_pa)

# Add 95% CI for each calculation
mean_by_rep_all_types_long <- 
        mean_by_rep_all_types_long %>%
        mutate(ci_95 = 1.96*correct_sd/sqrt(correct_n))


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
        ungroup() %>%
        mutate(border_dist = as.factor(border_dist))

# Fit the learning curves #############################################
# a <- mean_by_rep_all_types_long %>%
#         filter(ptp_trunk == '5c9a698...',
#                condition == 'schema_ic',
#                accuracy_type == 'correct_exact',
#                new_pa_status == 'both') %>%
#         select(correct_mean,accuracy_type,new_pa_status)
# 
# optim(c(i_start,c_start),
#       fit_learning_and_intercept,
#       gr = NULL,
#       seq(1,8),
#       a$correct_mean,
#       'sse',
#       a$accuracy_type,
#       TRUE,
#       method = 'L-BFGS-B',
#       lower = c(i_lower,c_lower),
#       upper = c(i_upper,c_upper)
# )


learning_and_intercept_each_participant <-
        mean_by_rep_all_types_long %>%
        group_by(ptp_trunk,
                 condition,
                 new_pa_status,
                 accuracy_type) %>%
        do(as.data.frame(
                optim(c(i_start,c_start),
                      fit_learning_and_intercept,
                      gr = NULL,
                      seq(1,8),
                      .$correct_mean,
                      'sse',
                      .$accuracy_type,
                      FALSE,
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
# learning_and_intercept_each_participant <- 
#         learning_and_intercept_each_participant %>%
#         mutate(c_log = log(c))
# # This will result in Inf for those c==0. Do empirical log-odds?
# print('CHANGE HOW SMALLEST LOG C GETS SUBSTITUTED')
# smallest_c_log <- learning_and_intercept_each_participant$c_log[!is.infinite(learning_and_intercept_each_participant$c_log)] %>% min()
# learning_and_intercept_each_participant <-
#         learning_and_intercept_each_participant %>%
#         mutate(c_log = case_when(
#                 is.infinite(c_log) ~ smallest_c_log,
#                 TRUE ~ c_log
#         ))

# Add the predicted data to the dataframe
learning_and_intercept_each_participants_y_hat <-
        learning_and_intercept_each_participant %>%
        group_by(ptp_trunk,
                 condition,
                 new_pa_status,
                 accuracy_type) %>% 
        rowwise() %>%
        mutate(y_hat_i_c = list(fit_learning_and_intercept(c(i,c),
                                                           seq(1:8),
                                                           seq(1:8),
                                                           'fit',
                                                           accuracy_type,
                                                           FALSE)),
               new_pa_img_row_number_across_sessions = list(seq(1:8))) %>% 
        unnest(c(y_hat_i_c,
                 new_pa_img_row_number_across_sessions)) %>% 
        select(c(ptp_trunk,
                 condition,
                 new_pa_status,
                 accuracy_type,
                 y_hat_i_c,
                 new_pa_img_row_number_across_sessions)) %>%
        ungroup()

mean_by_rep_all_types_long <- merge(mean_by_rep_all_types_long,
                                    learning_and_intercept_each_participants_y_hat,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'new_pa_status',
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
                 new_pa_status,
                 accuracy_type) %>%
        summarise(last_two_mean = mean(correct_mean),
                  last_two_sd   = sd(correct_mean)) %>%
        ungroup()

last_four_reps_stats <-
        mean_by_rep_all_types_long %>%
        filter(new_pa_img_row_number_across_sessions %in% c(5,6,7,8)) %>%
        group_by(ptp_trunk,
                 condition,
                 new_pa_status,
                 accuracy_type) %>%
        summarise(last_four_mean = mean(correct_mean),
                  last_four_sd   = sd(correct_mean)) %>%
        ungroup()

# Create one variable, that will have all the dependent variables
sum_stats_each_participant <- merge(last_two_reps_stats,
                                    last_four_reps_stats,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'new_pa_status',
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
                                           'new_pa_status',
                                           'accuracy_type'),
                                    all = TRUE)

## For each border distance -----------------------------------------------
last_four_reps_by_border_dist_stats <-
        mean_by_border_dist_rep_long %>%
        filter(new_pa_img_row_number_across_sessions %in% c(5,6,7,8)) %>% 
        group_by(ptp_trunk,
                 condition,
                 border_dist,
                 accuracy_type) %>%
        summarise(last_four_mean = mean(correct_mean, na.rm = T),
                  last_four_sd   = sd(correct_mean, na.rm = T),
                  n = n()) %>%
        ungroup()

# Across every 8 repetitions, so we can plot learning for each border dist
sum_stats_by_border_distance <-
        mean_by_border_dist_rep_long %>%
        group_by(ptp_trunk,
                 condition,
                 border_dist,
                 new_pa_img_row_number_across_sessions,
                 accuracy_type) %>%
        summarise(correct_mean = mean(correct_mean, na.rm = T),
                  correct_sd   = sd(correct_mean, na.rm = T)) %>%
        ungroup()

# Now, matlab computed learning rates ############################

ml_learning_rate <- import('./results/pilots/preprocessed_data/learning_rate_fits_matlab.csv')

sum_stats_each_participant <- merge(sum_stats_each_participant,
                                    ml_learning_rate,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'new_pa_status',
                                           'accuracy_type'),
                                    all.x = T)

sum_stats_each_participant <- sum_stats_each_participant %>%
        rename(sse = sse.x,
               sse_ml = sse.y,
               i_ml = intercept,
               c_ml = learning_rate)

## Log transform matlab learning rates --------------------------------
# sum_stats_each_participant <- sum_stats_each_participant %>%
#         mutate(c_ml_log = log(c_ml))


## Calculate predicted y values and merge with the long form data ------------
learning_and_intercept_each_participants_y_hat_ml <-
        ml_learning_rate %>%
        group_by(ptp_trunk,
                 condition,
                 new_pa_status,
                 accuracy_type) %>% 
        mutate(y_hat_i_c_ml = list(fit_learning_and_intercept(c(intercept,learning_rate),
                                                              seq(1:8),
                                                              seq(1:8),
                                                              'fit',
                                                              accuracy_type,
                                                              FALSE)),
               new_pa_img_row_number_across_sessions = list(seq(1:8))) %>%
        unnest(c(y_hat_i_c_ml,
                 new_pa_img_row_number_across_sessions)) %>% 
        select(c(ptp_trunk,
                 condition,
                 new_pa_status,
                 accuracy_type,
                 y_hat_i_c_ml,
                 new_pa_img_row_number_across_sessions)) %>%
        ungroup()

mean_by_rep_all_types_long <- merge(mean_by_rep_all_types_long,
                                    learning_and_intercept_each_participants_y_hat_ml,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'new_pa_status',
                                           'accuracy_type',
                                           'new_pa_img_row_number_across_sessions'),
                                    all = TRUE)

# Save everything ##################################

if (saveData){
        write_csv(mean_by_rep_all_types_long,'./results/pilots/preprocessed_data/mean_by_rep_all_types_long.csv')
}