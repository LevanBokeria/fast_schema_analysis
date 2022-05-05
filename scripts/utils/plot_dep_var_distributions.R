# Description #########################

# Takes in the whole data and plots the distribution of our dependent variables
# for session 2. Then, fits with a gaussian, checks the skewness 

# Load libraries and data ####################
rm(list=ls())
source('./scripts/utils/load_all_libraries.R')

# Load an external script which contains functions for estimating either just the learning rate, or also the asymptote
source('./scripts/utils/functions_for_fitting_learning_curves.R')

# Create parameters as starting points for estimations
i_start <- 0.5
c_start <- 0.1

# Create lower and upper bound constraints on the asymptote and learning rate
c_lower <- 0
c_upper <- Inf
i_lower <- 0
i_upper <- Inf

qc_filter <- T

source('./scripts/analysis/analyze_dependent_variables.R')

# Calculate skews ########################################################

## Last 4 averages -----------------------------------------------------
skew_each_cond_last_4 <- sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('correct_exact',
                                    'correct_one_square_away',
                                    'mouse_dist_euclid')) %>%
        droplevels() %>%
        group_by(condition,
                 accuracy_type) %>%
        summarise(skew = skewness(last_four_mean, na.rm = T)) %>%
        ungroup()

skew_across_cond_last_4 <- sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('correct_exact',
                                    'correct_one_square_away',
                                    'mouse_dist_euclid')) %>%
        droplevels() %>%
        group_by(ptp_trunk,
                 accuracy_type) %>%
        summarise(last_four_mean = mean(last_four_mean,na.rm=T)) %>%
        ungroup() %>%
        group_by(accuracy_type) %>%
        summarise(skew = skewness(last_four_mean, na.rm = T)) %>%
        ungroup()

## Learning rates -----------------------------------------------------
skew_each_cond_c_ml <- sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('correct_exact',
                                    'correct_one_square_away',
                                    'mouse_dist_euclid')) %>%
        droplevels() %>%
        group_by(condition,
                 accuracy_type) %>%
        summarise(skew = skewness(c_ml, na.rm = T)) %>%
        ungroup()

skew_across_cond_c_ml <- sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('correct_exact',
                                    'correct_one_square_away',
                                    'mouse_dist_euclid')) %>%
        droplevels() %>%
        group_by(ptp_trunk,
                 accuracy_type) %>%
        summarise(c_ml = mean(c_ml,na.rm=T)) %>%
        ungroup() %>%
        group_by(accuracy_type) %>%
        summarise(skew = skewness(c_ml, na.rm = T)) %>%
        ungroup()


# Now all the plots ###################################################

## Last 4 average --------------------------------------------------------

### Accuracies ===============================

sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('correct_exact',
                                    'correct_one_square_away')) %>%
        droplevels() %>%
        ggplot(aes(x=last_four_mean)) +
        geom_histogram() +
        geom_density(size=1) +
        facet_grid(accuracy_type~condition) +
        ggtitle('By condition; Accuracies') +
        geom_text(data=filter(skew_each_cond_last_4, accuracy_type != 'mouse_dist_euclid'),
                  aes(x=0.4,y=4,
                      label = round(skew,2)))

sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('correct_exact',
                                    'correct_one_square_away')) %>%
        droplevels() %>%
        ggplot(aes(x=last_four_mean)) +
        geom_histogram() +
        geom_density(size=1) +
        facet_wrap(~accuracy_type, ncol = 1) +
        ggtitle('Across conditions; Accuracies.') +
        geom_text(data=filter(skew_across_cond_last_4, accuracy_type != 'mouse_dist_euclid'),
                  aes(x=0.4,y=6,
                      label = round(skew,2)))  


### Mouse eudlid dist ===============================

sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('mouse_dist_euclid')) %>%
        droplevels() %>%
        ggplot(aes(x=last_four_mean, after_stat(density))) +
        geom_histogram() +
        geom_density(size=1) +
        facet_grid(accuracy_type~condition) +
        ggtitle('By condition; Mouse dist') +
        geom_text(data=filter(skew_each_cond_last_4, accuracy_type == 'mouse_dist_euclid'),
                  aes(x=0.4,y=0.04,
                      label = round(skew,2)))

sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('mouse_dist_euclid')) %>%
        droplevels() %>%
        ggplot(aes(x=last_four_mean, after_stat(density))) +
        geom_histogram() +
        geom_density(size=1) +
        facet_wrap(~accuracy_type, ncol = 1) +
        ggtitle('Across conditions; Mouse dist') +
        geom_text(data=filter(skew_across_cond_last_4, accuracy_type == 'mouse_dist_euclid'),
                  aes(x=0.4,y=0.03,
                      label = round(skew,2)))  


## Learning rates --------------------------------------------------------

### Accuracies ===============================

sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('correct_exact',
                                    'correct_one_square_away')) %>%
        droplevels() %>%
        ggplot(aes(x=c_ml)) +
        geom_histogram() +
        geom_density(size=1) +
        facet_grid(accuracy_type~condition) +
        ggtitle('By condition; Accuracies') +
        geom_text(data=filter(skew_each_cond_c_ml, accuracy_type != 'mouse_dist_euclid'),
                  aes(x=0.4,y=4,
                      label = round(skew,2))) +
        coord_cartesian(ylim = c(0,15))

sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('correct_exact',
                                    'correct_one_square_away')) %>%
        droplevels() %>%
        ggplot(aes(x=c_ml,after_stat(density))) +
        geom_histogram() +
        geom_density(size=1) +
        facet_wrap(~accuracy_type, ncol = 1) +
        ggtitle('Across conditions; Accuracies.') +
        geom_text(data=filter(skew_across_cond_c_ml, accuracy_type != 'mouse_dist_euclid'),
                  aes(x=0.4,y=6,
                      label = round(skew,2)))  


### Mouse eudlid dist ===============================

sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('mouse_dist_euclid')) %>%
        droplevels() %>%
        ggplot(aes(x=c_ml)) +
        geom_histogram() +
        geom_density(size=1) +
        facet_grid(accuracy_type~condition) +
        ggtitle('By condition; Mouse dist') +
        geom_text(data=filter(skew_each_cond_c_ml, accuracy_type == 'mouse_dist_euclid'),
                  aes(x=0.4,y=4,
                      label = round(skew,2)))

sum_stats_each_participant %>%
        filter(new_pa_status == 'both',
               accuracy_type %in% c('mouse_dist_euclid')) %>%
        droplevels() %>%
        ggplot(aes(x=c_ml)) +
        geom_histogram() +
        geom_density(size=1) +
        facet_wrap(~accuracy_type, ncol = 1) +
        ggtitle('Across conditions; Mouse dist') +
        geom_text(data=filter(skew_across_cond_c_ml, accuracy_type == 'mouse_dist_euclid'),
                  aes(x=0.4,y=3,
                      label = round(skew,2)))




