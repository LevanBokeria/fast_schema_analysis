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
                       )
                )
        
        session_results <- session_results %>%
                mutate(ptp = json_decoded$prolific_ID, .before = rt,
                       ptp = as.factor(ptp))
        
        session_results_all_ptp <- bind_rows(session_results_all_ptp,session_results)
        
}


# Just plot one per participant, performance across sessions, C vs IC
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
        ylim(0,1)

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
        ylim(0,1)
        





# Exact vs rough accuracy:

# Exact and rough accuracy across sessions
session_results_all_ptp %>%
        # reorder_levels(ptp, order = c('48652','zibzab','notmyname')) %>%
        filter(!stage == 'practice') %>%
        pivot_longer(c('correct','roughly_correct'),
                     names_to = 'accuracy_type',
                     values_to = 'accuracy') %>% 
        mutate(accuracy = coalesce(accuracy,0)) %>%
        group_by(ptp,condition,session_global,stimulus,accuracy_type) %>%
        summarize(avg_corr = mean(accuracy, na.rm = T)) %>%
        ggplot(aes(x=session_global,y=avg_corr, col=stimulus, group=stimulus)) +
        geom_point(aes(col=stimulus)) + 
        geom_line(show.legend = T) + 
        facet_grid(ptp ~ accuracy_type + condition,labeller = label_both) +
        # scale_x_discrete(labels = c('Exact','Rough')) + 
        theme(legend.position = 'none') +
        ggtitle('Session 4 is new PA. Each row is a participant')

# Rough vs exact accuracy
# session_results_all_ptp %>%
#         filter(!stage == 'practice') %>%
#         # reorder_levels(ptp, order = c('48652','zibzab','notmyname')) %>%
#         pivot_longer(c('correct','roughly_correct'),
#                      names_to = 'accuracy_type',
#                      values_to = 'accuracy') %>%
#         mutate(accuracy = coalesce(accuracy,0)) %>%
#         group_by(ptp,condition,session_global,stimulus,accuracy_type) %>%
#         summarize(avg_corr = mean(accuracy, na.rm = T)) %>%
#         ggplot(aes(x=accuracy_type,y=avg_corr, col = stimulus, group=stimulus)) +
#         geom_point(aes(col=stimulus)) + 
#         geom_line(show.legend = T) + 
#         facet_grid(ptp ~ condition + session_global,labeller = label_both) + 
#         scale_x_discrete(labels = c('Exact','Rough')) + 
#         theme(legend.position = 'none') + 
#         ggtitle('Session 4 is new PA. Each row is a participant')
