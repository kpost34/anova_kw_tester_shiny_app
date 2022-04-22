# R code to develop Shiny app for running ANOVAs or K-W tests
# Part 3 of 3: Kruskal-Wallis testing and post-hoc tests

#load packages
library(here)
library(tidyverse)
library(rstatix)

#source in code from part 2
source(here("02_anova_testing_code.R"))
#pulls in functions anova_tabler, data_transformer, residual_extracter, and samp_maker
#pulls in various DFs and model objects


#### Visualize data================================================================
### Bring back boxplot
sampDF %>%
  ggplot(aes(x=trmt,y=value)) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(aes(color=trmt),position=position_jitterdodge()) +
  scale_color_viridis_d(begin=0,end=0.65) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=12))

### Same but with custom function
boxplotter(df=sampDF,x=trmt,y=value)


#### Run Kruskal-Wallis and Post-hoc Tests=========================================
### Run test with base R
kruskal.test(value~trmt,data=sampDF)


### Run test with rstatix function
kruskal_test(sampDF,value~trmt)


#### Separate means
dunn_test(sampDF,value~trmt)




