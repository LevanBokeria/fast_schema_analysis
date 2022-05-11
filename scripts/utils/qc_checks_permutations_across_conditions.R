# Description #####################################################

# 1. For each participant, get the 120 trials of session 2 across all 5 conditions.
# 2. Then, shuffle the "label" aka which prompt was presented. Thats 1 permutation.
# 3. Do 10,000 permutations, calculating mean accuracy for each of them. 
#    Both, correct_exact and correct_one_square_away accuracy measures will be calculated.
#    This gives the participant-specific null distribution.
# 4. Compare the real participant accuracy to this data.

# The script allows to do such permutations on a subset of PAs, for example
# those only 3+ squares away from the border.


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

ptp_to_filter <- c() # so EXCLUDE these

border_away_to_analyze <- c(1,2,3,4,5) # so KEEP these

load_existing_data <- F

saveData <- F
# Start analysis ###########################################################
if (load_existing_data){
        
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
        
} else {
        # Permutation-based chance level ##############################################
        results <- list()
        
        # A giant matrix approach --------------------------------------------------
        df_all_ptp <- session_results_all_ptp %>%
                filter(!ptp_trunk %in% ptp_to_filter) %>% 
                filter(border_dist %in% border_away_to_analyze) %>%
                filter(!condition %in% c('practice','practice2'),
                       session == 2) %>%
                droplevels() %>%
                select(ptp_trunk,condition,
                       row,col,
                       corr_row,corr_col,
                       pa_center_x,
                       pa_center_y,
                       mouse_clientX,
                       mouse_clientY,
                       mouse_error,
                       correct_exact,
                       correct_one_square_away,
                       border_dist,
                       near_pa)
        
        # df_all_ptp <- df_all_ptp %>%
        #         filter(near_pa == FALSE)
        
        ctr <- 1
        
        niter <- 1000
        
        for (iPtp in levels(df_all_ptp$ptp_trunk)){
                
                print(iPtp)
                
                df <- df_all_ptp %>%
                        filter(ptp_trunk == iPtp) %>%
                        droplevels() %>%
                        as.data.frame(row.names = 1:nrow(.))
                
                # Remove missed trials, cause we don't have pa location info for those
                # so we cannot permute mouse location info
                df <- df[!is.na(df$row),]
                
                print(paste0('removed ', (120-nrow(df)), ' missing rows'))
                
                # Replicate
                df <- rbindlist(replicate(niter,df,simplify = F), idcol = 'id')
                
                # Create a column containing shuffling indices
                df <- df %>%
                        group_by(ptp_trunk,id) %>%
                        mutate(rand_idx = sample(n())) %>%
                        ungroup()
                
                # Shuffle the correct row col and calculate the accuracy
                df <- df %>%
                        group_by(ptp_trunk,id) %>%
                        mutate(corr_row_shuff = corr_row[rand_idx],
                               corr_col_shuff = corr_col[rand_idx],
                               rc_dist_euclid_shuff = sqrt(
                                       (corr_row_shuff-row)^2 + (corr_col_shuff-col)^2
                               ),
                               correct_exact_shuff = as.numeric(
                                       (corr_row_shuff == corr_row) & 
                                               (corr_col_shuff == corr_col)
                               ),
                               correct_one_square_away_shuff = case_when(
                                       abs(rc_dist_euclid_shuff) < 1.9 ~ 1,
                                       TRUE ~ 0
                               ),
                               pa_center_x_shuff = pa_center_x[rand_idx],
                               pa_center_y_shuff = pa_center_y[rand_idx],
                               mouse_error_shuff = sqrt(
                                       (mouse_clientX - pa_center_x_shuff)^2 +
                                       (mouse_clientY - pa_center_y_shuff)^2
                                       )
                               ) %>%
                        ungroup()
                
                # Now, just distill down to a summary statistic across trials
                df <- df %>%
                        group_by(ptp_trunk,id) %>%
                        summarise(mean_correct_one_square_away_shuff = mean(correct_one_square_away_shuff, na.rm = T),
                                  mean_correct_one_square_away       = mean(correct_one_square_away, na.rm = T),
                                  mean_correct_exact_shuff           = mean(correct_exact_shuff, na.rm = T),
                                  mean_correct_exact                 = mean(correct_exact, na.rm = T),
                                  mean_mouse_error_shuff             = mean(mouse_error_shuff, na.rm = T),
                                  mean_mouse_error                   = mean(mouse_error, na.rm = T)) %>%
                        ungroup()
                
                # Get the percentile, and distill even further
                # df_sum <- df %>%
                #         group_by(ptp_trunk,condition) %>%
                #         summarise(n_perm_less = sum(
                #                 mean_correct_one_square_away_shuff < mean(mean_correct_one_square_away, na.rm = T)
                #                 ),
                #                 n_perm = n(),
                #                 percentile = n_perm_less * 100 / n_perm) %>%
                #         ungroup()
                
                results[[ctr]] <- df
                
                ctr <- ctr + 1
                
        }
        
        results_bound <- rbindlist(results, idcol = 'id_ptp')
        
        # Get the percentile, and distill even further
        df_percentile <- results_bound %>%
                group_by(ptp_trunk) %>%
                summarise(n_perm = n(),
                          mean_correct_one_square_away = mean(mean_correct_one_square_away, na.rm = T),
                          mean_correct_exact           = mean(mean_correct_exact, na.rm = T),
                          mean_mouse_error             = mean(mean_mouse_error, na.rm = T),
                          n_perm_less_correct_one_square_away = sum(
                                  mean_correct_one_square_away_shuff < mean_correct_one_square_away
                          ),
                          n_perm_less_correct_exact = sum(
                                  mean_correct_exact_shuff < mean_correct_exact
                          ),            
                          n_perm_less_mouse_error = sum(
                                  mean_mouse_error_shuff < mean_mouse_error
                          ),                                      
                          percentile_sim_correct_one_square_away = 
                                  n_perm_less_correct_one_square_away * 100 / n_perm,
                          percentile_sim_correct_exact = 
                                  n_perm_less_correct_exact * 100 / n_perm,
                          percentile_sim_mouse_error = 
                                  n_perm_less_mouse_error * 100 / n_perm) %>%
                ungroup()
        
        
        # Did any fail? ---------------------------------------------------------------
        
        threshold <- 95
        
        df_percentile <- df_percentile %>%
                mutate(qc_fail_correct_one_square_away = percentile_sim_correct_one_square_away <= threshold,
                       qc_fail_correct_exact           = percentile_sim_correct_exact <= threshold,
                       qc_fail_mouse_error             = percentile_sim_mouse_error <= threshold)
        
        # Save the df
        if (saveData){
                results_bound %>% write_csv(
                        paste0(
                                './results/pilots/qc/qc_permutations_raw_across_conditions_border_dist_',
                                paste(border_away_to_analyze,collapse = '_'),
                                '.csv'
                        )
                )
                df_percentile %>% write_csv(
                        paste0(
                                './results/pilots/qc/qc_permutations_summary_across_conditions_border_dist_',
                                paste(border_away_to_analyze,collapse = '_'),
                                '.csv'
                        )
                )
        }
        
}


# Plot ------------------------------------------------------------------------

## Exact correct
fig1 <- results_bound %>%
        ggplot() +
        geom_histogram(aes(x = mean_correct_exact_shuff,
                           fill = ptp_trunk)) +
        geom_vline(data = df_percentile,
                   aes(xintercept = mean_correct_exact)) +
        geom_text(data = df_percentile,
                  aes(x = 0.1,
                      y = niter / 2,
                      label = percentile_sim_correct_exact)) +
        facet_wrap(~ptp_trunk) +
        ggtitle('Correct exact')

print(fig1)

## One Square away
fig2 <- results_bound %>%
        ggplot() +
        geom_histogram(aes(x = mean_correct_one_square_away_shuff,
                           fill = ptp_trunk)) +
        geom_vline(data = df_percentile,
                   aes(xintercept = mean_correct_one_square_away)) +
        geom_text(data = df_percentile,
                  aes(x = 0.1,
                      y = niter / 2,
                      label = percentile_sim_correct_one_square_away)) +
        facet_wrap(~ptp_trunk) +
        ggtitle('Correct one square away')

print(fig2)

## Mouse errror
fig3 <- results_bound %>%
        ggplot() +
        geom_histogram(aes(x = mean_mouse_error_shuff,
                           fill = ptp_trunk)) +
        geom_vline(data = df_percentile,
                   aes(xintercept = mean_mouse_error)) +
        geom_text(data = df_percentile,
                  aes(x = 0.1,
                      y = niter / 2,
                      label = percentile_sim_mouse_error)) +
        facet_wrap(~ptp_trunk) +
        ggtitle('Mouse error')

print(fig3)


