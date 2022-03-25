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

ctr <- 1
for (iPtp in levels(session_results_all_ptp$ptp_trunk)){
        
        print(iPtp)
        
        egd <- session_results_all_ptp %>%
                filter(ptp_trunk == iPtp,
                       !condition %in% c('practice','practice2'),
                       session == 2) %>%
                select(row,col,corr_row,corr_col) %>%
                as.data.frame(row.names = 1:nrow(.))
        
        for (i in seq(1:100)){
                
                # permute
                rand_idx <- sample(120)
                
                egd <- egd %>% mutate(corr_row_shuff = corr_row[rand_idx],
                                      corr_col_shuff = corr_col[rand_idx],
                                      rc_dist_euclid_shuff = sqrt(
                                              (corr_row_shuff-row)^2 + (corr_col_shuff-col)^2
                                      ),
                                      correct_one_square_away_shuff = case_when(
                                              abs(rc_dist_euclid_shuff) < 1.9 ~ 1,
                                              TRUE ~ 0
                                      ))
                
                # Get the summary stat
                mean_acc <- mean(egd$correct_one_square_away_shuff,na.rm = T)
                
                # Record this
                results[[ctr]][i] <- mean_acc
                
        }
        
        ctr <- ctr + 1
        
        
}

# # Plot RT distributions
# session_results_all_ptp %>%
#         filter(!condition %in% c('practice','practice2')) %>%
#         reorder_levels(condition, order = c('schema_c',
#                                             'schema_ic',
#                                             'landmark_schema',
#                                             'random_locations',
#                                             'no_schema')) %>%
#         ggplot(aes(x=rt,fill=session)) +
#         geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity') + 
#         facet_grid(ptp_trunk~condition+session) + 
#         theme(legend.position = '') +
#         geom_vline(xintercept = c(0,500,1000),linetype='dashed')
# 
# # Plot responses
# session_results_all_ptp %>%
#         ggplot(aes(col,row)) +
#         geom_bin2d(bins=12) +
#         facet_grid(ptp_trunk~condition+session) +
#         theme(panel.grid.minor = element_blank()) +
#         scale_x_continuous(breaks=seq(0,12),limits = c(0,12)) + 
#         scale_y_continuous(breaks=seq(0,12),limits = c(0,12))        
# 
# # Plot responses as continuous response at the same location
# session_results_all_ptp %>%
#         # filter(ptp == '5e8e5058e690d70ebefeb033') %>% 
#         unite('responded_row_col',row:col,remove=FALSE) %>% 
#         mutate(responded_row_col = as.factor(responded_row_col)) %>%
#         ggplot(aes(x=session_trial_idx,y=responded_row_col)) +
#                 geom_line(aes(group=ptp)) +
#                 facet_grid(ptp_trunk~condition+session)
