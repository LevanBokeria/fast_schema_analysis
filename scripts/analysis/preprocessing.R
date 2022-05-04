# Description ####

# Rik's Mad Idea paradigm. Analyzing those pilots.
# This script will load the individual raw jatos data, and create a unified 
# long-form data containin participants responses.
# It will also create:
# - Variable with all the feedback test
# - Variable with information on how long they spent reading instructions
# - How long spent at the breaks.
# - Names they came up for the hidden items.



# Clean the environment and load libraries ############################

rm(list=ls())

source('./scripts/utils/load_all_libraries.R')

# Some global setup ###########################################################

saveDataCSV <- T

filenames <- c('jatos_results_20211027204610',
               'jatos_results_20211027204642',
               'jatos_results_20211027204700',
               'jatos_results_20211027204707',
               'jatos_results_20211027205947',
               'jatos_results_20211028173401',
               'jatos_results_20211028173430',
               'jatos_results_20211028173435',
               'jatos_results_20211028174128',
               'jatos_results_20211028204501',
               'jatos_results_20211028204526')

session_results_all_ptp <- NULL
feedback_all_ptp <- NULL
listings_all_ptp <- NULL
all_ptp_break_rt <- matrix(, nrow = length(filenames), ncol = 10)

# Start the loop ###########################################################
iPtp <-0
for (iName in filenames){
        print(iName)
        iPtp <- iPtp+1
        
        filepath <- paste('./data/pilots/gui_downloads/',iName,'.txt', sep='')
        
        my_data <- read_file(filepath)
        
        # Find the data submission module
        start_loc <- str_locate(my_data, 'data_submission_start---')[2]
        end_loc   <- str_locate(my_data, '---data_submission_end]')[1]
        
        json_content <- substr(my_data,start_loc+1,end_loc-1)
        
        json_decoded <- fromJSON(json_content)
        
        print(json_decoded$prolific_ID)
        
        session_results <- as_tibble(
                json_decoded$outputData$session_results_new_pa_learning
                )
        
        session_results %<>% 
                select(!c('internal_node_id','trial_index','trial_type'))
        
        # Sanity check
        # if (nrow(session_results) != 264){
        #         stop('Session results are wrong length!')
        # }
        
        n_trials_per_session <- session_results %>%
                filter(!condition %in% c('practice','practice2')) %>% 
                group_by(condition,session) %>%
                summarise(n = n())
        if (any(n_trials_per_session$n != 24)){
                stop('n trials per session is wrong!')
        }

        
        # All the mutations
        session_results <- session_results %>%
                mutate(session = as.factor(session),
                       condition = as.factor(condition),
                       correct = as.numeric(correct),
                       new_pa_img = as.factor(new_pa_img),
                       rc_dist_cb = abs(corr_row-row) + abs(corr_col-col),
                       rc_dist_euclid = sqrt(
                               (corr_row-row)^2 + (corr_col-col)^2
                       ),
                       mouse_dist_cb = abs(mouse_clientX - pa_center_x) +
                               abs(mouse_clientY - pa_center_y),
                       mouse_dist_euclid = sqrt(
                               (mouse_clientX - pa_center_x)^2 +
                               (mouse_clientY - pa_center_y)^2
                               ),
                       correct_exact = coalesce(correct,0L),
                       correct_one_square_away = case_when(
                               abs(rc_dist_euclid) < 1.9 ~ 1,
                               TRUE ~ 0
                       ),
                       correct_rad_21 = case_when(
                               abs(mouse_dist_euclid) <= 21 ~ 1,
                               TRUE ~ 0
                       ),                       
                       correct_rad_42 = case_when(
                               abs(mouse_dist_euclid) <= 42 ~ 1,
                               TRUE ~ 0
                       ),                                              
                       correct_rad_63 = case_when(
                               abs(mouse_dist_euclid) <= 63 ~ 1,
                               TRUE ~ 0
                       ),
                       correct_rad_84 = case_when(
                               abs(mouse_dist_euclid) <= 84 ~ 1,
                               TRUE ~ 0
                       ),                       
                       correct_rad_105 = case_when(
                               abs(mouse_dist_euclid) <= 105 ~ 1,
                               TRUE ~ 0
                       )
                )
        
        session_results <- session_results %>%
                mutate(ptp = json_decoded$prolific_ID, .before = rt,
                       ptp = as.factor(ptp))
        
        # If its participant lgffsg, then fix the problem with session counters
        if (json_decoded$prolific_ID == 'lgffsg'){
                session_results$session[
                        (session_results$condition == 'landmark_schema' |
                         session_results$condition == 'random_locations' |
                         session_results$condition == 'no_schema') &
                         session_results$session == 1] <- 2
                
                session_results$session[
                        (session_results$condition == 'landmark_schema' |
                         session_results$condition == 'random_locations' |
                         session_results$condition == 'no_schema') &
                         is.na(session_results$session)] <- 1                
        }
        
        # Add a row counter for each occurrence of a new_pa_img, 
        # within that condition and within that session
        session_results <- session_results %>%
                group_by(condition,session,new_pa_img) %>%
                mutate(new_pa_img_row_number = row_number()) %>%
                ungroup()
        
        # Add a row counter for each occurrence of a new_pa_img, 
        # within that condition, across the two sessions
        session_results <- session_results %>%
                group_by(condition,new_pa_img) %>%
                mutate(new_pa_img_row_number_across_sessions = row_number()) %>%
                ungroup()
        
        
        # Add trial index counter for each session
        session_results <- session_results %>%
                group_by(condition,session) %>%
                mutate(session_trial_idx = row_number(),
                       .after = session) %>%
                ungroup()
        
        
        # Mark which of the new PA items are the far_pas vs near_pas in all conditions
        all_conditions <- c('schema_c','schema_ic','landmark_schema')
        
        # - create the column with just NAs at first
        session_results$near_pa <- NA        
                
        for (iCond in all_conditions){
                
                print(iCond)
                
                # - what are all the new PAs for this ptp for this condition?
                all_new_pas <- 
                        json_decoded[["inputData"]][["stimuli"]][[iCond]][["new_pa_learning"]]
                
                # - what are the coordinates of these new PAs and of schema PAs?
                new_pa_coords <- 
                        json_decoded[["inputData"]][["condition_coords"]][[iCond]][["new_pa_learning"]]
                
                schema_pa_coords <- 
                        json_decoded[["inputData"]][["condition_coords"]][[iCond]][["schema_learning"]]
                
                # - Now, find the indices of those rows that are next to schema PAs:
                distances <- as.matrix(pdist(as.matrix(new_pa_coords),as.matrix(schema_pa_coords)))
                
                # - Find the coordinates of the far_pa
                near_pa_new_pas <- which(distances <= sqrt(2)+0.1,arr.ind=TRUE)
                near_pa_new_pas <- near_pa_new_pas[,'row'] 
                
                near_pa_new_pa_names <- all_new_pas[near_pa_new_pas]
                
                # Now, for this condition, wherever the newPA is a far_pa, mark TRUE. Else mark FALSE
                session_results$near_pa[
                        session_results$condition == iCond & 
                                session_results$new_pa_img %in% near_pa_new_pa_names] <- 
                        TRUE
                session_results$near_pa[
                        session_results$condition == iCond & 
                                !session_results$new_pa_img %in% near_pa_new_pa_names] <- 
                        FALSE                
                
        }
        
        # Mark which ones are close to the border of the board
        session_results <- session_results %>%
                mutate(dist_border_l = corr_col - 1,
                       dist_border_r = 12 - corr_col,
                       dist_border_t = corr_row - 1,
                       dist_border_b = 12 - corr_row) %>%
                rowwise() %>%
                mutate(border_dist = min(dist_border_l,
                                           dist_border_r,
                                           dist_border_t,
                                           dist_border_b))
        
        # Combine the data across participants
        session_results_all_ptp <- bind_rows(session_results_all_ptp,session_results)
        
        # Add up feedback
        curr_ptp_feedback <- 
                as_tibble(
                        json_decoded[["outputData"]][["debriefing"]][["response"]]
                        )
        
        curr_ptp_feedback <- curr_ptp_feedback %>%
                mutate(ptp = json_decoded$prolific_ID, .before = Q0,
                       ptp = as.factor(ptp))        
        # - concatenate
        feedback_all_ptp <- bind_rows(feedback_all_ptp,curr_ptp_feedback)
        
        
        # Congregate listing of the hidden and visible items
        curr_ptp_listings <- bind_rows(
                json_decoded$outputData$break_results[[1]][2,],
                json_decoded$outputData$break_results[[3]][2,],
                json_decoded$outputData$break_results[[5]][2,],
                json_decoded$outputData$break_results[[7]][2,],
                json_decoded$outputData$break_results[[9]][2,]
                )
        curr_ptp_listings <- curr_ptp_listings %>%
                select(rt,response) %>%
                flatten() %>%
                mutate(ptp = json_decoded$prolific_ID)
        curr_ptp_listings$boards <- unique(session_results$condition)[3:7]
        
        listings_all_ptp <- bind_rows(listings_all_ptp,curr_ptp_listings)
        
        # Get the times spent at each break
        curr_ptp_break_rt <- NULL
        curr_ptp_break_rt[1] <- json_decoded$prolific_ID
        for (iBreak in seq(1,9)){
                curr_ptp_break_rt[iBreak+1] <- json_decoded$outputData$break_results[[iBreak]]$rt[2]
        }
        
        all_ptp_break_rt[iPtp,] <- curr_ptp_break_rt
        
}

session_results_all_ptp <- session_results_all_ptp %>%
mutate(ptp_trunk = str_trunc(as.character(ptp), width = 10)) %>% 
        reorder_levels(condition, order = c('practice',
                                            'practice2',
                                            'schema_c',
                                            'schema_ic',
                                            'landmark_schema',
                                            'random_locations',
                                            'no_schema'))

# Add columnt names #############

names(feedback_all_ptp) <- c('ptp',
                             'Clear instructions?',
                              'Notice blue schema_C',
                              'Notice green schema_IC',
                              'Notice red landmarks',
                              'Notice yellow random',
                              'Strategy?',
                              'Did visible ones help or hinder?',
                              'Anything else')

# Create condition orders ################
condition_orders <- tibble(.rows = 7)

all_ptp <- unique(session_results_all_ptp$ptp)

for (iPtp in as.vector(all_ptp)){
        iPtp
        condition_orders[iPtp] <-
                unique(
                        session_results_all_ptp$condition[
                                session_results_all_ptp$ptp==iPtp
                                ])
}

# Save everything #######################
if (saveDataCSV){
        write_csv(session_results_all_ptp,'./results/pilots/preprocessed_data/session_results_long_form.csv')
        write_csv(feedback_all_ptp,'./results/pilots/preprocessed_data/feedback_all_ptp.csv')
        write_csv(listings_all_ptp,'./results/pilots/preprocessed_data/listings_all_ptp.csv')
        write.csv(all_ptp_break_rt,'./results/pilots/preprocessed_data/all_ptp_break_rt.csv',
          row.names = FALSE)
}
