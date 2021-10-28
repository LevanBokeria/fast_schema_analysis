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
               pdist,
               gghighlight)

# Some global setup ###########################################################

writeInExcel <- F

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

# Start the loop ###########################################################

for (iName in filenames){
        print(iName)
        
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
                summarize(n = n())
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
        
        
        # Mark which of the new PA items are the neighbors in all conditions
        all_conditions <- c('schema_c','schema_ic','landmark_schema','random_locations')
        
        # - create the column with just NAs at first
        session_results$adjascent_neighbor <- NA        
                
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
                
                # - Find the coordinates of the neighbor
                neighbor_new_pas <- which(distances <= sqrt(2)+0.1,arr.ind=TRUE)
                neighbor_new_pas <- neighbor_new_pas[,'row'] 
                
                neighbor_new_pa_names <- all_new_pas[neighbor_new_pas]
                
                # Now, for this condition, wherever the newPA is a neighbor, mark TRUE. Else mark FALSE
                session_results$adjascent_neighbor[
                        session_results$condition == iCond & 
                                session_results$new_pa_img %in% neighbor_new_pa_names] <- 
                        TRUE
                session_results$adjascent_neighbor[
                        session_results$condition == iCond & 
                                !session_results$new_pa_img %in% neighbor_new_pa_names] <- 
                        FALSE                
                
        }
        
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
        
        
}

session_results_all_ptp <- session_results_all_ptp %>%
        filter(ptp != '609478fa9e5b4d075246cfaf')


## Add the factor of whether they saw grid lines or not ----------------------- 
session_results_all_ptp %<>%
        mutate(saw_grid_lines = case_when(
                ptp == 'lgffsg' | ptp == 'asdfg' | ptp == '32423' ~ FALSE,
                ptp == 'jhgf' | ptp == '1111111' ~ TRUE,
                TRUE ~ NA
        ))

names(feedback_all_ptp) <- c('ptp',
                             'Clear instructions?',
                              'Notice blue schema_C',
                              'Notice green schema_IC',
                              'Notice red landmarks',
                              'Notice yellow random',
                              'Strategy?',
                              'Did visible ones help or hinder?',
                              'Anything else')

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
cond_order <- c('schema_c','schema_ic','landmark_schema','random_locations',
                'no_schema')


## Plot separately for each participant ----------------------------------------

# get the average performance across the targets, by repetition
mean_by_rep <-
        session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
        group_by(ptp,condition,session,new_pa_img_row_number) %>%
        summarize(correct_rad_63_mean = mean(correct_rad_63, na.rm = T),
                  correct_rad_63_sd = sd(correct_rad_63, na.rm = T)) %>%
        ungroup()

# Calculate mean for neighbor vs non neighbor
mean_by_landmark_rep <-
        session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
        group_by(ptp,condition,session,adjascent_neighbor,new_pa_img_row_number) %>%
        summarize(correct_rad_63_mean = mean(correct_rad_63, na.rm = T),
                  correct_rad_63_sd = sd(correct_rad_63, na.rm = T)) %>%
        ungroup() %>%
        mutate(correct_rad_63_mean = 
                       case_when(
                               is.na(adjascent_neighbor) ~ as.numeric(NA),
                               TRUE ~ correct_rad_63_mean
                               ),
               correct_rad_63_sd =
                       case_when(
                               is.na(adjascent_neighbor) ~ as.numeric(NA),
                               TRUE ~ correct_rad_63_sd
                       ),
               )

fig <- session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        droplevels() %>%
        reorder_levels(condition, order = cond_order) %>%
        mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
        group_by(ptp,condition,session,new_pa_img) %>%
        
        ggplot(aes(x=new_pa_img_row_number,y=correct_rad_63)) +
        geom_point(aes(group=new_pa_img),
                   alpha=0.2) +
        geom_line(aes(group=new_pa_img),
                  alpha=0.2) +

        # Add the average across toys
        geom_point(data = mean_by_rep,
                   aes(y=correct_rad_63_mean)) +
        geom_line(data = mean_by_rep,
                  aes(y=correct_rad_63_mean),
                  size=2) +

        # Add the average across landmark or not
        geom_point(data = mean_by_landmark_rep, 
                   aes(group=adjascent_neighbor,
                       color=adjascent_neighbor,
                       y=correct_rad_63_mean)) +
        geom_line(data = mean_by_landmark_rep, 
                  aes(group=adjascent_neighbor,
                      color=adjascent_neighbor,
                      y=correct_rad_63_mean),
                  size=1) +
        
        facet_grid(ptp~condition*session) +
        ggtitle(paste('Accuracy type: 63px radius',sep='')) +
        # theme(legend.position = 'none') +
        xlab('Image repetition') +
        scale_x_continuous(breaks=c(1,2,3,4)) 
        # gghighlight(is.na(adjascent_neighbor) == FALSE,
        #             calculate_per_facet = TRUE,
        #             use_direct_label = FALSE)        

print(fig)

# Now plot averaged over participants
mean_by_rep_across_ptp <-
        session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
        group_by(condition,session,new_pa_img_row_number) %>%
        summarize(correct_rad_63_mean = mean(correct_rad_63, na.rm = T),
                  correct_rad_63_sd = sd(correct_rad_63, na.rm = T),
                  n = n(),
                  correct_rad_63_95_CI = 1.96*sd(correct_rad_63)/sqrt(n())) %>%
        ungroup()

# Calculate mean for neighbor vs non neighbor, across participants
mean_by_landmark_rep_across_ptp <-
        session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
        group_by(condition,session,adjascent_neighbor,new_pa_img_row_number) %>%
        summarize(correct_rad_63_mean = mean(correct_rad_63, na.rm = T),
                  correct_rad_63_sd = sd(correct_rad_63, na.rm = T),
                  n = n(),
                  correct_rad_63_95_CI = 1.96*sd(correct_rad_63)/sqrt(n())) %>%
        ungroup() %>%
        mutate(correct_rad_63_mean = 
                       case_when(
                               is.na(adjascent_neighbor) ~ as.numeric(NA),
                               TRUE ~ correct_rad_63_mean
                       ),
               correct_rad_63_sd =
                       case_when(
                               is.na(adjascent_neighbor) ~ as.numeric(NA),
                               TRUE ~ correct_rad_63_sd
                       ),
               correct_rad_63_95_CI =
                       case_when(
                               is.na(adjascent_neighbor) ~ as.numeric(NA),
                               TRUE ~ correct_rad_63_95_CI
                       ),               
        )

mean_diff_neighbor_non_neighbor <- 
        mean_by_landmark_rep %>%
        pivot_wider(id_cols = c(ptp,condition,session,new_pa_img_row_number),
                    names_from = adjascent_neighbor,
                    names_prefix = 'neighbor_',
                    values_from = correct_rad_63_mean) %>% 
        select(!neighbor_NA) %>% 
        mutate(neighbor_diff = neighbor_TRUE - neighbor_FALSE)
        
mean_diff_neighbor_non_neighbor_across_ptp <-
        mean_diff_neighbor_non_neighbor %>%
        group_by(condition,session,new_pa_img_row_number) %>%
        summarise(mean_diff_correct_rad_63 = mean(neighbor_diff),
                  n = n(),
                  ci_95 = 1.96*sd(neighbor_diff)/sqrt(n()))


fig_across_ptp <- mean_by_rep_across_ptp %>%
        reorder_levels(condition, order = cond_order) %>%
        ggplot(aes(x=new_pa_img_row_number,y=correct_rad_63_mean)) +
        geom_point(alpha=0.2) +
        geom_line(size=2) +
        geom_ribbon(aes(ymin = correct_rad_63_mean-correct_rad_63_95_CI,
                        ymax = correct_rad_63_mean+correct_rad_63_95_CI),
                    alpha=0.2) +        
        
        # # Add the average across toys
        # geom_point(aes(y=correct_rad_63_mean)) +
        # geom_line(aes(y=correct_rad_63_mean),
        #           size=2) +
        
        # # Add the average across landmark or not
        geom_point(data = mean_by_landmark_rep_across_ptp,
                   aes(group=adjascent_neighbor,
                       color=adjascent_neighbor,
                       y=correct_rad_63_mean)) +
        geom_line(data = mean_by_landmark_rep_across_ptp,
                  aes(group=adjascent_neighbor,
                      color=adjascent_neighbor,
                      y=correct_rad_63_mean),
                  size=1) +
        geom_ribbon(data = mean_by_landmark_rep_across_ptp,
                    aes(group=adjascent_neighbor,
                        color=adjascent_neighbor,
                        ymin = correct_rad_63_mean-correct_rad_63_95_CI,
                        ymax = correct_rad_63_mean+correct_rad_63_95_CI),
                    alpha=0.1,
                    linetype = "dotted") +

        facet_wrap(~condition*session, nrow = 1) +
        ggtitle(paste('Accuracy type: 63px radius',sep='')) +
        # theme(legend.position = 'none') +
        xlab('Image repetition') +
        scale_x_continuous(breaks=c(1,2,3,4)) +
        theme(legend.position = 'top')

print(fig_across_ptp)

# Plot the difference between landmark and non-landmark items
fig_neighbor_diff <- mean_diff_neighbor_non_neighbor_across_ptp %>%
        ggplot(aes(x=new_pa_img_row_number,y=mean_diff_correct_rad_63)) +
        geom_line(size=1) +
        geom_ribbon(aes(ymin = mean_diff_correct_rad_63 - ci_95,
                        ymax = mean_diff_correct_rad_63 + ci_95),
                    alpha = 0.2) +
        
        
        facet_wrap(~condition*session, nrow = 1) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        ggtitle('Difference between landmark and non-landmark ite')

print(fig_neighbor_diff)

# # What accuracy type are we using?
# 
# acc_to_use <- '63'
# 
# if (acc_to_use == '63'){
#         
#         # - get the average performance within session across presentation number
#         trial_avg <-
#                 session_results_all_ptp %>%
#                 filter(!condition %in% c('practice','practice2')) %>%
#                 mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
#                 group_by(condition,session,new_pa_img_row_number) %>%
#                 summarize(correct_rad_63 = mean(correct_rad_63, na.rm = T)) %>%
#                 ungroup()
# 
# 
#         fig <- session_results_all_ptp %>%
#                 reorder_levels(condition, order = cond_order) %>%
#                 filter(!condition %in% c('practice','practice2')) %>%
#                 mutate(correct_rad_63 = coalesce(correct_rad_63,0)) %>%
#                 group_by(condition,session,new_pa_img) %>%
# 
#                 ggplot(aes(x=new_pa_img_row_number,y=correct_rad_63)) +
#                 geom_point(aes(color=new_pa_img, group=new_pa_img)) +
#                 geom_line(aes(color=new_pa_img,group=new_pa_img)) +
# 
#                 geom_point(data = trial_avg, aes(group=condition)) +
#                 geom_line(data = trial_avg, aes(group=condition),size=1) +
# 
#                 facet_grid(~condition*session) +
#                 ggtitle(paste('Accuracy type: 63px radius',sep='')) +
#                 theme(legend.position = 'none') +
#                 xlab('Image repetition') +
#                 scale_x_continuous(breaks=c(1,2,3,4)) +
#                 gghighlight(is.na(adjascent_neighbor) == FALSE,
#                             calculate_per_facet = TRUE,
#                             use_direct_label = FALSE)
# } else if (acc_to_use == '42'){
#         # - get the average performance within session across presentation number
#         trial_avg <- 
#                 session_results_all_ptp %>%
#                 filter(!condition %in% c('practice','practice2')) %>%
#                 mutate(correct_rad_42 = coalesce(correct_rad_42,0)) %>%
#                 group_by(ptp,condition,session,new_pa_img_row_number) %>%
#                 summarize(correct_rad_42 = mean(correct_rad_42, na.rm = T)) %>%
#                 ungroup()
#         
#         
#         fig <- session_results_all_ptp %>%
#                 reorder_levels(condition, order = cond_order) %>%
#                 reorder_levels(ptp, order = c('lgffsg','asdfg','32423',
#                                               'jhgf','1111111')) %>%
#                 filter(!condition %in% c('practice','practice2')) %>%
#                 mutate(correct_rad_42 = coalesce(correct_rad_42,0)) %>%
#                 group_by(ptp,condition,session,new_pa_img) %>%
#                 
#                 ggplot(aes(x=new_pa_img_row_number,y=correct_rad_42)) +
#                 geom_point(aes(color=new_pa_img, group=new_pa_img)) + 
#                 geom_line(aes(color=new_pa_img,group=new_pa_img)) + 
#                 
#                 geom_point(data = trial_avg, aes(group=condition)) +
#                 geom_line(data = trial_avg, aes(group=condition),size=1) +
#                 
#                 facet_grid(ptp~condition*session) + 
#                 ggtitle(paste('Accuracy type: 42px radius',sep='')) + 
#                 theme(legend.position = 'none') + 
#                 xlab('Image repetition') + 
#                 scale_x_continuous(breaks=c(1,2,3,4)) +
#                 gghighlight(is.na(adjascent_neighbor) == FALSE,
#                             calculate_per_facet = TRUE,
#                             use_direct_label = FALSE)
# }
# print(fig)

# For each participant, make the same plot but ordered by condition order
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

# condition_orders <-
        # condition_orders[,c('lgffsg','asdfg','32423','jhgf','1111111')]









