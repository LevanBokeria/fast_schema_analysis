# Description ####

# Loads the pilot data and checks for quality.
# Will plot RT distributions and clicking locations



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
        
# Permutation-based chance level ##############################################
results <- vector(mode = "list", length = length(levels(session_results_all_ptp$ptp_trunk)))

results <- list()

# A giant matrix approach --------------------------------------------------
df_all_ptp <- session_results_all_ptp %>%
        # filter(ptp_trunk == '609478f...') %>%
        filter(!condition %in% c('practice','practice2'),
               session == 2) %>%
        droplevels() %>%
        select(ptp_trunk,condition,
               row,col,corr_row,corr_col,
               correct_one_square_away)

ctr <- 1

niter <- 10000

for (iPtp in levels(df_all_ptp$ptp_trunk)){
        
        print(iPtp)
        
        df <- df_all_ptp %>%
                filter(ptp_trunk == iPtp) %>%
                droplevels() %>%
                as.data.frame(row.names = 1:nrow(.))
        
        # Replicate
        df <- rbindlist(replicate(niter,df,simplify = F), idcol = 'id')
        
        # Create a column containing shuffling indices
        df <- df %>%
                group_by(ptp_trunk,condition,id) %>%
                mutate(rand_idx = sample(n())) %>%
                ungroup()
                
        # Shuffle the correct row col and calculate the accuracy
        df <- df %>%
                group_by(ptp_trunk,condition,id) %>%
                mutate(corr_row_shuff = corr_row[rand_idx],
                       corr_col_shuff = corr_col[rand_idx],
                       rc_dist_euclid_shuff = sqrt(
                               (corr_row_shuff-row)^2 + (corr_col_shuff-col)^2
                       ),
                       correct_one_square_away_shuff = case_when(
                               abs(rc_dist_euclid_shuff) < 1.9 ~ 1,
                               TRUE ~ 0
                       )) %>%
                ungroup()
        
        # Now, just distill down to a summary statistic across trials
        df <- df %>%
                group_by(ptp_trunk,condition,id) %>%
                summarise(mean_correct_one_square_away_shuff = mean(correct_one_square_away_shuff, na.rm = T),
                          mean_correct_one_square_away = mean(correct_one_square_away, na.rm = T)) %>%
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
        group_by(ptp_trunk,condition) %>%
        summarise(mean_correct_one_square_away = mean(mean_correct_one_square_away, na.rm = T),
                n_perm_less = sum(
                mean_correct_one_square_away_shuff < mean_correct_one_square_away
                ),
                n_perm = n(),
                percentile_sim = n_perm_less * 100 / n_perm) %>%
        ungroup()

# Plot ------------------------------------------------------------------------

results_bound %>%
        ggplot() +
        geom_histogram(aes(x = mean_correct_one_square_away_shuff,
                           fill = condition)) +
        geom_vline(data = df_percentile,
                   aes(xintercept = mean_correct_one_square_away)) +
        geom_text(data = df_percentile,
                  aes(x = 0.1,
                      y = niter / 2,
                      label = percentile_sim)) +
        facet_grid(ptp_trunk~condition) 

# Did any fail? ---------------------------------------------------------------

threshold <- 95

df_percentile <- df_percentile %>%
        mutate(qc_fail = percentile_sim <= threshold)

# Save the df
results_bound %>% write_csv('./results/pilots/preprocessed_data/qc_permutations_raw.csv')
df_percentile %>% write_csv('./results/pilots/preprocessed_data/qc_permutations_summary.csv')


