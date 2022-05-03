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
c_upper <- 20
i_lower <- 0
i_upper <- 1

qc_filter <- F

source('./scripts/utils/analyze_dependent_variables.R')


# Calculate skewness ##########################################
skew_each_cond <- sum_stats_each_participant %>%
        filter(neighbor_status == 'both',
               accuracy_type %in% c('correct_exact','correct_one_square_away')) %>%
        droplevels() %>%
        group_by(condition,
                 accuracy_type) %>%
        summarise(skew = skewness(last_four_mean, na.rm = T))

skew_across_cond <- sum_stats_each_participant %>%
        filter(neighbor_status == 'both',
               accuracy_type %in% c('correct_exact','correct_one_square_away')) %>%
        droplevels() %>%
        group_by(accuracy_type) %>%
        summarise(skew = skewness(last_four_mean, na.rm = T))

# Plot last 4 PAs averaged ##########################

sum_stats_each_participant %>%
        filter(neighbor_status == 'both',
               accuracy_type %in% c('correct_exact','correct_one_square_away')) %>%
        droplevels() %>%
        ggplot(aes(x=last_four_mean)) +
        geom_histogram() +
        geom_density() +
        facet_grid(accuracy_type~condition) +
        ggtitle('all PAs; By condition') +
        geom_text(data=skew_each_cond,
                  aes(x=0.4,y=5,
                      label = round(skew,2)))

sum_stats_each_participant %>%
        filter(neighbor_status == 'both',
               accuracy_type %in% c('correct_exact','correct_one_square_away')) %>%
        droplevels() %>%
        ggplot(aes(x=last_four_mean)) +
        geom_histogram() +
        geom_density() +
        facet_wrap(~accuracy_type, ncol = 1) +
        ggtitle('all PAs; across conditions') +
        geom_text(data=skew_across_cond,
                  aes(x=0.4,y=6,
                      label = round(skew,2)))        

# Plot for learning fits now ###################################################

## Load the data -----------------------------------------------------------
ml_fits <- import('./results/pilots/preprocessed_data/learning_rate_fits_matlab.csv')

## Plots ---------------------------------------------------------------------

lr_skew_each_cond <- ml_fits %>%
        filter(neighbor_status == 'both',
               accuracy_type %in% c('correct_exact','correct_one_square_away')) %>%
        droplevels() %>%
        group_by(condition,
                 accuracy_type) %>%
        summarise(skew = skewness(learning_rate, na.rm = T))

lr_skew_across_cond <- ml_fits %>%
        filter(neighbor_status == 'both',
               accuracy_type %in% c('correct_exact','correct_one_square_away')) %>%
        droplevels() %>%
        group_by(accuracy_type) %>%
        summarise(skew = skewness(learning_rate, na.rm = T))


ml_fits %>%
        filter(neighbor_status == 'both',
               accuracy_type %in% c('correct_exact','correct_one_square_away')) %>%
        droplevels() %>%
        ggplot(aes(x=learning_rate)) +
        geom_histogram() +
        geom_density() +
        facet_grid(accuracy_type~condition) +
        ggtitle('all PAs; By condition') +
        geom_text(data=lr_skew_each_cond,
                  aes(x=0.4,y=5,
                      label = round(skew,2)))


ml_fits %>%
        filter(neighbor_status == 'both',
               accuracy_type %in% c('correct_exact','correct_one_square_away')) %>%
        droplevels() %>%
        ggplot(aes(x=learning_rate)) +
        geom_histogram() +
        geom_density() +
        facet_wrap(~accuracy_type, ncol = 1) +
        ggtitle('all PAs; across conditions') +
        geom_text(data=lr_skew_across_cond,
                  aes(x=0.4,y=6,
                      label = round(skew,2))) 



# Plot for mouse euclidean distance #####################################################









