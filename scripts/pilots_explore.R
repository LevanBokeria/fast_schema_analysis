# Clean the environment
rm(list=ls())

# Load libraries
library(tidyverse)
library(rstatix)
library(DT)
library(kableExtra)
# library(rjson)
library(readr)
library(writexl)
library(jsonlite)
library(stringr)
library(gridExtra)

writeInExcel <- F

filenames <- c('jatos_results_20210620174618',
               'jatos_results_20210620174558',
               'jatos_results_20210620174455')

session_results_all_ptp <- NULL

for (iName in filenames){
        # print(iName)
        
        filepath <- paste('./data/pilots/gui_downloads/',iName,'.txt', sep='')
        
        my_data <- read_file(filepath)
        
        # Find the data submission module
        start_loc <- str_locate(my_data, 'data_submission_start---')[2]
        end_loc   <- str_locate(my_data, '---data_submission_end]')[1]
        
        json_content <- substr(my_data,start_loc+1,end_loc-1)
        
        json_decoded <- fromJSON(json_content)
        
        session_results <- as_tibble(json_decoded$outputData$session_results)
        
        # All the mutations
        session_results <- session_results %>%
                mutate(session_global = as.factor(
                        case_when(stage == 'new_pa_learning' ~ 4,
                                  TRUE ~ as.double(session$schema_learning))),
                       session_condition = as.factor(
                               case_when(stage == 'new_pa_learning' ~ 1,
                                         TRUE ~ as.double(session$schema_learning))),
                       stage = as.factor(stage),
                       condition = as.factor(condition),
                       correct = as.numeric(correct),
                       stimulus = as.factor(stimulus),
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
        
        session_results_all_ptp <- bind_rows(session_results_all_ptp,session_results)
        
}


# Turn to long form with accuracy types as one column
session_results_all_ptp_gathered <- 
        session_results_all_ptp %>%
        pivot_longer(cols = starts_with('correct_rad'),
                     names_to = 'accuracy_type',
                     values_to = 'accuracy',
                     names_prefix = "correct_")

session_avg_gathered <- 
        session_results_all_ptp_gathered %>%
        filter(stage != 'practice') %>%
        mutate(accuracy = coalesce(accuracy,0)) %>%
        group_by(ptp,condition,session_global,accuracy_type) %>%
        summarize(avg_correct = mean(accuracy, na.rm=T)) %>%
        ungroup()


# Exact performance across sessions, C vs IC
stimulus_avg <-
        session_results_all_ptp %>%
        filter(stage != 'practice') %>%
        mutate(correct = coalesce(correct,0)) %>%
        group_by(ptp,condition,session_global,stimulus) %>%
        summarize(avg_corr = mean(correct, na.rm=T)) %>% 
        ungroup()


session_avg <- stimulus_avg %>%
        group_by(ptp,condition,session_global) %>%
        summarize(avg_corr = mean(avg_corr, na.rm=T)) %>%
        ungroup()


ggplot(data = stimulus_avg, aes(x=session_global, y=avg_corr)) +
        geom_point(aes(color=stimulus, group=stimulus)) +
        geom_line(aes(color=stimulus, group=stimulus)) +
        theme(legend.position = 'none') +
        facet_grid(ptp~condition, labeller=label_both) + 
        geom_line(data = session_avg, aes(group=condition),size = 1) +
        geom_point(data = session_avg, aes(group=condition)) + 
        ylim(0,1) + 
        ggtitle('Exact accuracy')

# Roughly correct now:
stimulus_avg <-
        session_results_all_ptp %>%
        filter(stage != 'practice') %>%
        mutate(roughly_correct = coalesce(roughly_correct,0)) %>%
        group_by(ptp,condition,session_global,stimulus) %>%
        summarize(avg_corr = mean(roughly_correct, na.rm=T)) %>% 
        ungroup()


session_avg <- stimulus_avg %>%
        group_by(ptp,condition,session_global) %>%
        summarize(avg_corr = mean(avg_corr, na.rm=T)) %>%
        ungroup()


ggplot(data = stimulus_avg, aes(x=session_global, y=avg_corr)) +
        geom_point(aes(color=stimulus, group=stimulus)) +
        geom_line(aes(color=stimulus, group=stimulus)) +
        theme(legend.position = 'none') +
        facet_grid(ptp~condition, labeller=label_both) + 
        geom_line(data = session_avg, aes(group=condition),size = 1) +
        geom_point(data = session_avg, aes(group=condition)) + 
        ylim(0,1) + 
        ggtitle('Rough accuracy')
        
##############################################################################
# Try multiple radiuses

# - Consistent
session_results_all_ptp_gathered %>%
        filter(stage != 'practice' & 
                       condition == 'schema_c') %>%
        group_by(ptp,session_global,accuracy_type,stimulus) %>%
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
        geom_point(aes(group=stimulus,color=stimulus)) + 
        geom_line(aes(group=stimulus,color=stimulus)) +
        theme(legend.position = 'none',
              axis.text.x = element_text(angle=-90)) + 
        geom_point(data = filter(session_avg_gathered,condition == 'schema_c')) +         
        geom_line(data = filter(session_avg_gathered,condition == 'schema_c'),
                  aes(group=condition)) +                 
        facet_grid(ptp~session_global, labeller=label_both) + 
        ylim(0,1) + 
        ggtitle('Consistent')

# - Inconsistent
session_results_all_ptp_gathered %>%
        filter(stage != 'practice' & 
                       condition == 'schema_ic') %>%
        group_by(ptp,session_global,accuracy_type,stimulus) %>%
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
        ggplot(aes(x=accuracy_type,y=avg_correct,group=stimulus,color=stimulus)) + 
        geom_point() + 
        geom_line() +
        theme(legend.position = 'none',
              axis.text.x = element_text(angle=-90)) + 
        facet_grid(ptp~session_global, labeller=label_both) + 
        ylim(0,1) + 
        ggtitle('Inconsistent')

# Session as the within line
# - Consistent
session_results_all_ptp_gathered %>%
        filter(stage != 'practice' & 
                       condition == 'schema_c') %>%
        group_by(ptp,session_global,accuracy_type,stimulus) %>%
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
        ggplot(aes(x=session_global,y=avg_correct,group=stimulus,color=stimulus)) + 
        geom_point() + 
        geom_line() +
        theme(legend.position = 'none') + 
        facet_grid(ptp~accuracy_type) + 
        ylim(0,1) + 
        ggtitle('Consistent')

# - Inconsistent
session_results_all_ptp_gathered %>%
        filter(stage != 'practice' & 
                       condition == 'schema_ic') %>%
        group_by(ptp,session_global,accuracy_type,stimulus) %>%
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
        ggplot(aes(x=session_global,y=avg_correct,group=stimulus,color=stimulus)) + 
        geom_point() + 
        geom_line() +
        theme(legend.position = 'none') + 
        facet_grid(ptp~accuracy_type) + 
        ylim(0,1) + 
        ggtitle('Inconsistent')




















