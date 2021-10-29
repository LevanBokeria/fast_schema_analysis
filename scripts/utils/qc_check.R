# Description ####

# Loads the pilot data and checks for quality.
# Will plot RT distributions and clicking locations



# Clean the environment and load libraries ############################

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
               gghighlight)

# Load the data #########################################
session_results_all_ptp <- import('./results/pilots/preprocessed_data/session_results_long_form.csv')

session_results_all_ptp <- session_results_all_ptp %>%
        mutate(ptp_trunk = str_trunc(ptp, width = 10))


# Plot RT distributions
session_results_all_ptp %>%
        filter(!condition %in% c('practice','practice2')) %>%
        reorder_levels(condition, order = c('schema_c',
                                            'schema_ic',
                                            'landmark_schema',
                                            'random_locations',
                                            'no_schema')) %>%
        ggplot(aes(x=rt,fill=session)) +
        geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity') + 
        facet_grid(ptp_trunk~condition+session) + 
        theme(legend.position = '') +
        geom_vline(xintercept = c(0,500,1000),linetype='dashed')

