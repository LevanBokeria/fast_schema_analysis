# Load libraries, data, define flags, etc #####################################

## Load all the libraries ----------------------------------------------------
source('./scripts/utils/load_all_libraries.R')
source('./scripts/utils/functions_for_fitting_learning_curves.R')

# options(error = recover)

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
        
        qc_filter <- T
        
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
        pivot_longer(cols = c(starts_with("correct_"),'mouse_error'),
                     names_to = 'accuracy_type',
                     values_to = 'accuracy_value') %>%
        mutate(accuracy_type = as.factor(accuracy_type)) %>%
        reorder_levels(accuracy_type, order = c(
                'mouse_error',
                'correct_exact',
                'correct_one_square_away'
        ))

## All data: long form for image repetitions --------------------------
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
        ungroup() %>% 
        mutate(border_dist = 'all')


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
        ungroup() %>%
        mutate(border_dist = 'all')

## Now the same for just border dist 3 and 4 -------------------
mean_by_rep_long_bord_dist_3_4 <- 
        session_results_all_ptp_long_accuracy %>%
        filter(border_dist %in% c(3,4)) %>%
        droplevels() %>% 
        group_by(ptp_trunk,
                 condition,
                 new_pa_img_row_number_across_sessions,
                 accuracy_type,
                 .drop = FALSE) %>% 
        summarise(correct_mean = mean(accuracy_value, na.rm = T),
                  correct_sd = sd(accuracy_value, na.rm = T),
                  correct_n = n()) %>%
        ungroup() %>% 
        mutate(border_dist = '3_4')


# Calculate mean for far_pa vs non far_pa
mean_by_landmark_rep_long_bord_dist_3_4 <-
        session_results_all_ptp_long_accuracy %>%
        filter(!condition %in% c('random_locations','no_schema'),
               border_dist %in% c(3,4)) %>%
        droplevels() %>%
        group_by(ptp_trunk,
                 condition,
                 near_pa,
                 new_pa_img_row_number_across_sessions,
                 accuracy_type,
                 .drop = FALSE) %>%
        summarise(correct_mean = mean(accuracy_value, na.rm = T),
                  correct_sd = sd(accuracy_value, na.rm = T),
                  correct_n = as.numeric(n())) %>%
        ungroup() %>% 
        mutate(border_dist = '3_4')

## Now the same for each border distance -------------------
mean_by_rep_long_all_bord_dist <- 
        session_results_all_ptp_long_accuracy %>%
        group_by(ptp_trunk,
                 condition,
                 border_dist,
                 new_pa_img_row_number_across_sessions,
                 accuracy_type,
                 .drop = FALSE) %>% 
        summarise(correct_mean = mean(accuracy_value, na.rm = T),
                  correct_sd = sd(accuracy_value, na.rm = T),
                  correct_n = n()) %>%
        ungroup() %>%
        mutate(border_dist = as.character(border_dist))

# Calculate mean for far_pa vs non far_pa
mean_by_landmark_rep_long_all_bord_dist <-
        session_results_all_ptp_long_accuracy %>%
        filter(!condition %in% c('random_locations','no_schema')) %>%
        droplevels() %>%
        group_by(ptp_trunk,
                 condition,
                 border_dist,
                 near_pa,
                 new_pa_img_row_number_across_sessions,
                 accuracy_type,
                 .drop = FALSE) %>%
        summarise(correct_mean = mean(accuracy_value, na.rm = T),
                  correct_sd = sd(accuracy_value, na.rm = T),
                  correct_n = as.numeric(n())) %>%
        ungroup() %>%
        mutate(border_dist = as.character(border_dist))

# Combine these
mean_by_rep_all_types_long <- bind_rows(mean_by_rep_long,
                                        mean_by_landmark_rep_long,
                                        mean_by_rep_long_bord_dist_3_4,
                                        mean_by_landmark_rep_long_bord_dist_3_4,
                                        mean_by_rep_long_all_bord_dist,
                                        mean_by_landmark_rep_long_all_bord_dist) %>%
        mutate(border_dist = as.factor(border_dist))

# Remove extra variables
rm(mean_by_rep_long,
   mean_by_landmark_rep_long,
   mean_by_rep_long_bord_dist_3_4,
   mean_by_landmark_rep_long_bord_dist_3_4,
   mean_by_rep_long_all_bord_dist,
   mean_by_landmark_rep_long_all_bord_dist)

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

# Rough measures for learning #########################################

## For both/near/far-PA -----------------------------------------------
sum_stats_each_participant <-
        mean_by_rep_all_types_long %>%
        filter(new_pa_img_row_number_across_sessions %in% c(5,6,7,8)) %>%
        group_by(ptp_trunk,
                 condition,
                 border_dist,
                 new_pa_status,
                 accuracy_type) %>%
        summarise(last_four_mean = mean(correct_mean, na.rm = T),
                  last_four_sd   = sd(correct_mean, na.rm = T)) %>%
        ungroup()

# Log transform session 2 average
sum_stats_each_participant <- sum_stats_each_participant %>%
        mutate(log_last_four_mean = log(last_four_mean))

# If any of them are Inf values, substitute with the lowest value.
print('accuracy data log transformed but could be inf!!')
# sum_stats_each_participant <- sum_stats_each_participant %>%
#         mutate(log_last_four_mean = case_when(
#                        is.infinite(log_last_four_mean) ~ min(log_last_four_mean*is.finite(log_last_four_mean),na.rm = T),
#                        TRUE ~ log_last_four_mean))

# Now, matlab computed learning rates ############################

ml_learning_rate <- import('./results/pilots/preprocessed_data/learning_rate_fits_matlab.csv')


if (qc_filter){
        ml_learning_rate <- ml_learning_rate %>%
                filter(ptp_trunk != '609478f...')
}

sum_stats_each_participant <- merge(sum_stats_each_participant,
                                    ml_learning_rate,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'border_dist',
                                           'new_pa_status',
                                           'accuracy_type'),
                                    all.x = T) 

sum_stats_each_participant <- sum_stats_each_participant %>%
        rename(sse_ml = sse,
               i_ml = intercept,
               c_ml = learning_rate,
               asymptote_ml = asymptote)

## Log transform matlab learning rates 
# sum_stats_each_participant <- sum_stats_each_participant %>%
#         mutate(c_ml_log = log(c_ml))

## Remove learning rates 1.5IQR away ---------------------------------

## Gaussianize matlab learning rates --------------------------------
sum_stats_each_participant <- sum_stats_each_participant %>%
        filter(border_dist %in% c('all','3_4')) %>%
        droplevels() %>%
        group_by(condition,
                 border_dist,
                 new_pa_status,
                 accuracy_type,
                 .drop = F) %>%
        mutate(c_ml_gauss = Gaussianize(c_ml, type = 's')) %>%
        ungroup()

# sum_stats_each_participant %>%
#         filter(accuracy_type == 'mouse_error',
#                border_dist %in% c('all','3_4'))
#         
# sum_stats_each_participant %>%
#         filter(accuracy_type == 'mouse_error',
#                border_dist %in% c('all','3_4')) %>%
#         group_by(condition,
#                  border_dist,
#                  new_pa_status,
#                  accuracy_type) %>%
#         mutate(c_ml_gauss = Gaussianize(c_ml, type = 's')) %>%
#         ungroup() %>%
#         ggplot(aes(x=c_ml_gauss)) +
#         geom_histogram() +
#         facet_grid(border_dist+condition~new_pa_status)

## Calculate predicted y values and merge with the long form data ------------
learning_and_intercept_each_participants_y_hat_ml <-
        ml_learning_rate %>%
        group_by(ptp_trunk,
                 condition,
                 border_dist,
                 new_pa_status,
                 accuracy_type) %>% 
        mutate(y_hat_i_c_ml = list(fit_learning_and_intercept(c(intercept,learning_rate,asymptote),
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
                 border_dist,
                 new_pa_status,
                 accuracy_type,
                 y_hat_i_c_ml,
                 new_pa_img_row_number_across_sessions)) %>%
        ungroup()

mean_by_rep_all_types_long <- merge(mean_by_rep_all_types_long,
                                    learning_and_intercept_each_participants_y_hat_ml,
                                    by = c('ptp_trunk',
                                           'condition',
                                           'border_dist',
                                           'new_pa_status',
                                           'accuracy_type',
                                           'new_pa_img_row_number_across_sessions'),
                                    all = TRUE)

# Save everything ##################################

if (saveData){
        write_csv(mean_by_rep_all_types_long,'./results/pilots/preprocessed_data/mean_by_rep_all_types_long.csv')
}
