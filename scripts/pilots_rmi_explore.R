# Description ####

# Rik's Mad Idea paradigm. Analyzing those pilots.

rm(list=ls())

pacman::p_load(pacman,
               rio,
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
               pracma,
               psych)


# Load the data ##########################
session_results_all_ptp <- import(
        './results/pilots/preprocessed_data/session_results_long_form.csv'
)

session_results_all_ptp <- session_results_all_ptp %>%
        reorder_levels(condition, order = c('practice',
                                            'practice2',
                                            'schema_c',
                                            'schema_ic',
                                            'landmark_schema',
                                            'random_locations',
                                            'no_schema')
        )

# Whats the order of conditions for this participant?
cond_order <- unique(session_results_all_ptp$condition)
cond_order <- c('schema_c','schema_ic','landmark_schema','random_locations',
                'no_schema')

# Create the estimation functions #############################################


fit_learning_and_asymptote <- function(p,t,y){
        print(p)
        
        y_hat <- p[1]*(1-exp(-p[2]*(t-1)))
        sse <- sum((y-y_hat)^2)
        return(sse)
}

fit_learning_only <- function(p,t,y){
        print(p)
        
        y_hat <- 1-exp(-p*(t-1))
        sse <- sum((y-y_hat)^2)
        return(sse)
}


# Now plot averaged over participants
mean_by_rep_across_ptp <-
        session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        droplevels() %>%
        group_by(condition,new_pa_img_row_number_across_sessions) %>%
        summarize(correct_rad_63_mean = mean(correct_rad_63, na.rm = T),
                  correct_rad_63_sd = sd(correct_rad_63, na.rm = T),
                  n = n(),
                  correct_rad_63_95_CI = 1.96*sd(correct_rad_63)/sqrt(n())) %>%
        ungroup()

# Calculate mean for neighbor vs non neighbor, across participants
mean_by_landmark_rep_across_ptp <-
        session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        droplevels() %>%
        group_by(condition,adjascent_neighbor,new_pa_img_row_number_across_sessions) %>%
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

fig_across_ptp <- mean_by_rep_across_ptp %>%
        ggplot(aes(x=new_pa_img_row_number_across_sessions,
                   y=correct_rad_63_mean)) +
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
        # geom_ribbon(data = mean_by_landmark_rep_across_ptp,
        #             aes(group=adjascent_neighbor,
        #                 color=adjascent_neighbor,
        #                 ymin = correct_rad_63_mean-correct_rad_63_95_CI,
        #                 ymax = correct_rad_63_mean+correct_rad_63_95_CI),
        #             alpha=0.1,
        #             linetype = "dotted") +

        facet_wrap(~condition, nrow = 1) +
        ggtitle(paste('Accuracy type: 63px radius',sep='')) +
        xlab('Image repetition') +
        scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8)) +
        theme(legend.position = 'top') +
        geom_vline(xintercept = 4.5, linetype = 'dashed')


print(fig_across_ptp)













# y <- session_results_all_ptp %>%
#         filter(ptp == '5e8e5058e690d70ebefeb033',
#                condition == 'schema_c') %>% 
#         select(correct_rad_63) %>% .[[1]]

# m <- optimize(f=fit_learning_only,
#            interval = c(0,10),
#            seq(1,48),
#            y)
# 
# m1 <- optim(c(0.5,0.1),
#             fit_learning,
#             gr = NULL,
#             seq(1,48),
#             y,
#             method = 'L-BFGS-B',
#             lower = c(0,0),
#             upper = c(1,10))

# session_results_all_ptp %>%
#         filter(!condition %in% c('practice','practice2')) %>%
#         droplevels() %>%
#         group_by(ptp,condition) %>%
#         do(as.data.frame(
#                 fminsearch(fit_learning,
#                       c(1,0.1),
#                       seq(1,48),
#                       .$correct_rad_63)
#                 ) %>% 
#                    mutate(id = row_number()) %>%
#                    pivot_wider(names_from = id, 
#                                values_from = xmin,
#                                names_prefix = 'xmin_')) %>% 
#         View()


# 
# # get the average performance across the targets, by repetition
# mean_by_rep <-
#         session_results_all_ptp %>%
#         filter(!condition %in% c('practice','practice2')) %>%
#         droplevels() %>%
#         group_by(ptp_trunk,
#                  condition,
#                  new_pa_img_row_number_across_sessions) %>%
#         summarize(correct_rad_63_mean = mean(correct_rad_63, na.rm = T),
#                   correct_rad_63_sd = sd(correct_rad_63, na.rm = T)) %>%
#         ungroup()
# 
# 
# learning_and_asymptote <- mean_by_rep %>%
#         group_by(ptp_trunk,condition) %>%
#         do(as.data.frame(
#                 optim(c(0.5,0.1),
#                       fit_learning_and_asymptote,
#                       gr = NULL,
#                       seq(1,8),
#                       .$correct_rad_63_mean,
#                       method = 'L-BFGS-B',
#                       lower = c(0,0),
#                       upper = c(1,10)
#                 )) %>% 
#                    mutate(id = row_number()) %>%
#                    pivot_wider(names_from = id, 
#                                values_from = par,
#                                names_prefix = 'par_')) %>%
#         rename(sse = value,
#                n_iterations = counts,
#                a = par_1,
#                c = par_2)
# 
# learning_only <- mean_by_rep %>%
#         group_by(ptp_trunk,condition) %>%
#         do(as.data.frame(
#                 optimize(fit_learning_only,
#                       c(0,10),
#                       seq(1,8),
#                       .$correct_rad_63_mean
#                       )) %>% 
#         rename(sse = objective,
#                c = minimum))
# 
# 
# # Manually add predicted y_hats to the dataframe
# mean_by_rep$y_hat_learning_and_asymptote <- NA
# mean_by_rep$y_hat_learning_only <- NA
# 
# for (iRow in seq(1,nrow(mean_by_rep))){
#         print(iRow)
#         curr_ptp = mean_by_rep$ptp_trunk[iRow]
#         
#         curr_condition = as.character(mean_by_rep$condition[iRow])
#         
#         # Get the parameters from estimating both learning and asymptote
#         row_idx1 = which(learning_and_asymptote$ptp_trunk == curr_ptp & 
#                          learning_and_asymptote$condition == curr_condition)
#         
#         curr_c1 = learning_and_asymptote$c[row_idx1]
#         curr_a1 = learning_and_asymptote$a[row_idx1]        
#         
#         # Get the parameters from estimating learning only
#         row_idx2 = which(learning_only$ptp_trunk == curr_ptp & 
#                          learning_only$condition == curr_condition)
#         
#         curr_c2 = learning_only$c[row_idx2]
#         
#         for (iRep in seq(1,8)){
#                 
#                 idx1 = which(
#                         mean_by_rep$ptp_trunk == curr_ptp &
#                                 mean_by_rep$condition == curr_condition &
#                                 mean_by_rep$new_pa_img_row_number_across_sessions == iRep
#                 )
#                 
#                 mean_by_rep$y_hat_learning_and_asymptote[idx1] <- curr_a1*(1-exp(-curr_c1*(iRep-1)))
#                 mean_by_rep$y_hat_learning_only[idx1]      <- 1      *(1-exp(-curr_c2*(iRep-1)))                
#         }
# }
# 
# 
# 
# # Calculate mean for neighbor vs non neighbor
# mean_by_landmark_rep <-
#         session_results_all_ptp %>%
#         filter(!condition %in% c('practice','practice2')) %>%
#         group_by(ptp_trunk,
#                  condition,
#                  adjascent_neighbor,
#                  new_pa_img_row_number_across_sessions) %>%
#         summarize(correct_rad_63_mean = mean(correct_rad_63, na.rm = T),
#                   correct_rad_63_sd = sd(correct_rad_63, na.rm = T)) %>%
#         ungroup() %>%
#         mutate(correct_rad_63_mean = 
#                        case_when(
#                                is.na(adjascent_neighbor) ~ as.numeric(NA),
#                                TRUE ~ correct_rad_63_mean
#                        ),
#                correct_rad_63_sd =
#                        case_when(
#                                is.na(adjascent_neighbor) ~ as.numeric(NA),
#                                TRUE ~ correct_rad_63_sd
#                        ),
#         )
# 
# fig <- mean_by_rep %>%
#         ggplot(aes(x=new_pa_img_row_number_across_sessions,
#                    y=correct_rad_63_mean)) +
#         geom_point() +
#         geom_line() +
#         
#         # Add the average across landmark or not
#         # geom_point(data = mean_by_landmark_rep, 
#         #            aes(group=adjascent_neighbor,
#         #                color=adjascent_neighbor,
#         #                y=correct_rad_63_mean)) +
#         # geom_line(data = mean_by_landmark_rep, 
#         #           aes(group=adjascent_neighbor,
#         #               color=adjascent_neighbor,
#         #               y=correct_rad_63_mean),
#         #           size=1) +
#         
#         # Add the y_hat learning and asymptote
#         geom_line(aes(x=new_pa_img_row_number_across_sessions,
#                       y=y_hat_learning_and_asymptote),
#                   size=1,
#                   color='blue',
#                   linetype = 'dashed') +
#         # Add the y_hat learning only
#         geom_line(aes(x=new_pa_img_row_number_across_sessions,
#                       y=y_hat_learning_only),
#                   size=1,
#                   color='red',
#                   linetype = 'dashed') +        
#         
#         
#         facet_grid(ptp_trunk~condition) +
#         ggtitle(paste('Accuracy type: 63px radius',sep='')) +
#         theme(legend.position = 'none') +
#         xlab('Image repetition') +
#         scale_x_continuous(breaks=seq(1,8))        
# 
# print(fig)
# 
# 






