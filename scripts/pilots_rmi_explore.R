# Description ####

# Rik's Mad Idea paradigm. Analyzing those pilots.

# Clean the environment and load libraries ############################

rm(list=ls())

pacman::p_load(pacman,
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
               pdist)

# Some global setup ###########################################################

writeInExcel <- F

filenames <- c('jatos_results_20210803132356',
               'jatos_results_20210806153107',
               'jatos_results_20210806153119')

# filenames <- c('jatos_results_20210708120252',
#                'jatos_results_20210708134815',
#                'jatos_results_20210708134829',
#                'jatos_results_20210708134836')

session_results_all_ptp <- NULL

# Start the loop ###########################################################

for (iName in filenames){
        # print(iName)
        
        filepath <- paste('./data/pilots/gui_downloads/',iName,'.txt', sep='')
        
        my_data <- read_file(filepath)
        
        # Find the data submission module
        start_loc <- str_locate(my_data, 'data_submission_start---')[2]
        end_loc   <- str_locate(my_data, '---data_submission_end]')[1]
        
        json_content <- substr(my_data,start_loc+1,end_loc-1)
        
        json_decoded <- fromJSON(json_content)
        
        session_results <- as_tibble(
                json_decoded$outputData$session_results_new_pa_learning
                )
        
        session_results %<>% 
                select(!c('internal_node_id','trial_index','trial_type'))
        
        # All the mutations
        session_results <- session_results %>%
                mutate(session = as.factor(session),
                       condition = as.factor(condition),
                       correct = as.numeric(correct),
                       new_pa_img = as.factor(new_pa_img),
                       rc_dist_cb = corr_row-row + corr_col-col,
                       rc_dist_euclid = sqrt(
                               (corr_row-row)^2 + (corr_col-col)^2
                       ),
                       mouse_dist_cb = abs(mouse_clientX - pa_center_x) +
                               abs(mouse_clientY - pa_center_y),
                       mouse_dist_euclid = sqrt((mouse_clientX - pa_center_x)^2 +
                                                        (mouse_clientY - pa_center_y)^2),
                       roughly_correct = case_when(
                               abs(rc_dist_euclid) < 2 ~ 1,
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
                       ),   
                       correct_rad_126 = case_when(
                               abs(mouse_dist_euclid) <= 126 ~ 1,
                               TRUE ~ 0
                       ),
                       correct_rad_147 = case_when(
                               abs(mouse_dist_euclid) <= 147 ~ 1,
                               TRUE ~ 0
                       ),
                       correct_rad_168 = case_when(
                               abs(mouse_dist_euclid) <= 168 ~ 1,
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
        
        # Mark which of the new PA items are the neighbors in the landmark_schema condition
        # - what are all the new PAs for this ptp for landmark condition?
        all_new_pas <- 
                json_decoded[["inputData"]][["stimuli"]][["landmark_schema"]][["new_pa_learning"]]
        
        # - what are the coorinates of these new PAs and of schema PAs?
        new_pa_coords <- 
                json_decoded[["inputData"]][["condition_coords"]][["landmark_schema"]][["new_pa_learning"]]
        
        schema_pa_coords <- json_decoded[["inputData"]][["condition_coords"]][["landmark_schema"]][["schema_learning"]]
        
        # - Now, find the indices of those rows that are next to schema PAs:
        distances <- as.matrix(pdist(as.matrix(new_pa_coords),as.matrix(schema_pa_coords)))
        
        # - Find the coordinates of the neighbor
        neighbor_new_pas <- which(distances <= sqrt(2)+0.1,arr.ind=TRUE)
        neighbor_new_pas <- neighbor_new_pas[,'row'] 
        
        neighbor_new_pa_names <- all_new_pas[neighbor_new_pas]
        
        session_results <- session_results %>%
                mutate(landmark_neighbor = case_when(
                        
                        condition == 'landmark_schema' & 
                                (new_pa_img == neighbor_new_pa_names[1] |
                                new_pa_img == neighbor_new_pa_names[2]) ~ TRUE,
                        TRUE ~ FALSE
                ))
                
        
        session_results_all_ptp <- bind_rows(session_results_all_ptp,session_results)
        
}

# Session by session learning ##################################################

# # Turn to long form with accuracy types as one column
# session_results_all_ptp_gathered <- 
#         session_results_all_ptp %>%
#         pivot_longer(cols = starts_with('correct_rad'),
#                      names_to = 'accuracy_type',
#                      values_to = 'accuracy',
#                      names_prefix = "correct_")
# 
# session_avg_gathered <- 
#         session_results_all_ptp_gathered %>%
#         mutate(accuracy = coalesce(accuracy,0)) %>%
#         group_by(ptp,condition,session,accuracy_type) %>%
#         summarize(avg_correct = mean(accuracy, na.rm=T)) %>%
#         ungroup()
# 
# 
# # Exact performance across sessions, C vs IC
# new_pa_img_avg <-
#         session_results_all_ptp %>%
#         mutate(correct = coalesce(correct,0)) %>%
#         group_by(ptp,condition,session,new_pa_img) %>%
#         summarize(avg_corr = mean(correct, na.rm=T)) %>% 
#         ungroup()
# 
# 
# session_avg <- new_pa_img_avg %>%
#         group_by(ptp,condition,session) %>%
#         summarize(avg_corr = mean(avg_corr, na.rm=T)) %>%
#         ungroup()
# 
# 
# ggplot(data = new_pa_img_avg, aes(x=session, y=avg_corr)) +
#         geom_point(aes(color=new_pa_img, group=new_pa_img)) +
#         geom_line(aes(color=new_pa_img, group=new_pa_img)) +
#         theme(legend.position = 'none') +
#         facet_grid(ptp~condition, labeller=label_both) + 
#         geom_line(data = session_avg, aes(group=condition),size = 1) +
#         geom_point(data = session_avg, aes(group=condition)) + 
#         ylim(0,1) + 
#         ggtitle('Exact accuracy')
# 
# # Roughly correct now:
# new_pa_img_avg <-
#         session_results_all_ptp %>%
#         filter(stage != 'practice') %>%
#         mutate(roughly_correct = coalesce(roughly_correct,0)) %>%
#         group_by(ptp,condition,session,new_pa_img) %>%
#         summarize(avg_corr = mean(roughly_correct, na.rm=T)) %>% 
#         ungroup()
# 
# 
# session_avg <- new_pa_img_avg %>%
#         group_by(ptp,condition,session) %>%
#         summarize(avg_corr = mean(avg_corr, na.rm=T)) %>%
#         ungroup()
# 
# 
# ggplot(data = new_pa_img_avg, aes(x=session, y=avg_corr)) +
#         geom_point(aes(color=new_pa_img, group=new_pa_img)) +
#         geom_line(aes(color=new_pa_img, group=new_pa_img)) +
#         theme(legend.position = 'none') +
#         facet_grid(ptp~condition, labeller=label_both) + 
#         geom_line(data = session_avg, aes(group=condition),size = 1) +
#         geom_point(data = session_avg, aes(group=condition)) + 
#         ylim(0,1) + 
#         ggtitle('Rough accuracy')
#         


# Within session learning #####################################################


# Whats the order of conditions for this participant?
cond_order <- unique(session_results$condition)

# - get the average performance within session across presentation number
trial_avg <- 
        session_results_all_ptp %>%
        filter(condition != 'practice') %>%
        mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
        group_by(ptp,condition,session,new_pa_img_row_number) %>%
        summarize(correct_rad_63 = mean(correct_rad_63, na.rm = T)) %>%
        ungroup()


session_results_all_ptp %>%
        reorder_levels(condition, order = cond_order) %>%
        filter(condition != 'practice') %>%
        mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
        group_by(ptp,condition,session,new_pa_img) %>%
        ggplot(aes(x=new_pa_img_row_number,y=correct_rad_63)) +
        geom_point(aes(color=new_pa_img, group=new_pa_img)) + 
        geom_line(aes(color=new_pa_img,group=new_pa_img)) + 

        
        geom_point(data = trial_avg, aes(group=condition)) + 
        geom_line(data = trial_avg, aes(group=condition),size=1) + 
        
        facet_grid(ptp~condition*session) + 
        ggtitle(paste('Accuracy type: 63px radius',sep='')) + 
        theme(legend.position = 'none') + 
        xlab('Image repetition') + 
        scale_x_continuous(breaks=c(1,2,3))
        
        




##############################################################################
# Try multiple radiuses

condition_to_plot <- 'schema_ic'

# - Radii as X axis
session_results_all_ptp_gathered %>%
        filter(stage != 'practice' & 
                       condition == condition_to_plot) %>%
        group_by(ptp,session,accuracy_type,new_pa_img) %>%
        summarize(avg_correct = mean(accuracy)) %>%
        reorder_levels(accuracy_type, order = c(
                'rad_21',
                'rad_42',
                'rad_63',
                'rad_84',
                'rad_105',
                'rad_126',
                'rad_147',
                'rad_168'
        )) %>%
        ungroup() %>% 
        ggplot(aes(x=accuracy_type,y=avg_correct)) + 
        geom_point(aes(group=new_pa_img,color=new_pa_img)) + 
        geom_line(aes(group=new_pa_img,color=new_pa_img)) +
        theme(legend.position = 'none',
              axis.text.x = element_text(angle=-90)) + 
        geom_point(data = filter(session_avg_gathered,condition == condition_to_plot)) +         
        geom_line(data = filter(session_avg_gathered,condition == condition_to_plot),
                  aes(group=condition)) +                 
        facet_grid(ptp~session, labeller=label_both) + 
        ylim(0,1) + 
        ggtitle(condition_to_plot)

# Session as the within line

session_results_all_ptp_gathered %>%
        filter(stage != 'practice' & 
                       condition == condition_to_plot) %>%
        group_by(ptp,session,accuracy_type,new_pa_img) %>%
        summarize(avg_correct = mean(accuracy)) %>%
        reorder_levels(accuracy_type, order = c(
                'rad_21',
                'rad_42',
                'rad_63',
                'rad_84',
                'rad_105',
                'rad_126',
                'rad_147',
                'rad_168'
        )) %>%
        ungroup() %>% 
        ggplot(aes(x=session,y=avg_correct)) + 
        geom_point(aes(group=new_pa_img,color=new_pa_img)) + 
        geom_line(aes(group=new_pa_img,color=new_pa_img)) +
        geom_point(data = filter(session_avg_gathered,condition == condition_to_plot)) +
        geom_line(data = filter(session_avg_gathered,condition == condition_to_plot),
                  aes(group=condition)) +          
        theme(legend.position = 'none') + 
        facet_grid(ptp~accuracy_type) + 
        ylim(0,1) + 
        ggtitle(condition_to_plot)

















