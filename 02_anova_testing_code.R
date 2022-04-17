# R code to develop Shiny app for running ANOVAs or K-W tests
# Part 2 of 3: ANOVA testing and post-hoc tests

#load packages
library(here)
library(tidyverse)
library(rstatix)

#source in code from part 1
source(here("01_anova_assump_transform_code.R"))
#pulls in functions data_transformer, residual_extracter, and samp_maker
#pulls in sampDF (sample data frame) (and other data frames)

#### Run ANOVA and post-hoc tests==========================================================================
### Run one-way ANOVA on data frame
anova(mod)


### Run post-hoc tests
## Tukey's HSD tests


### KEEP IN MIND
# experimentwise error rate
# CIs - return these
# choice of test?







