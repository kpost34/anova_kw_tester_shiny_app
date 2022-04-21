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

#### Run ANOVA==========================================================================
### Run one-way ANOVA on model object to produce ANOVA table
anova(mod) %>% 
  rownames_to_column() %>%
  as_tibble() %>% 
  select(source="rowname",SS=`Sum Sq`,DF="Df",MS=`Mean Sq`,F=`F value`,p=`Pr(>F)`) -> anova_tab


### Create function to build ANOVA table-------------------------------------------------------------------
anova_tabler<-function(model){
  anova(model) %>%
    rownames_to_column() %>%
    as_tibble() %>% 
    select(source="rowname",SS=`Sum Sq`,DF="Df",MS=`Mean Sq`,F=`F value`,p=`Pr(>F)`) -> anova_tab
  anova_tab
}


#### Perform Tukey's HSD post-hoc tests and graph results============================================
### Run Tukey's tests
tukey_hsd(mod) %>%
  select(-null.value)


### Graph results
tukey_hsd(mod) %>%
  select(-null.value) %>%
  mutate(comp=paste(group1,group2,sep="-"),
         comp=fct_relevel(comp,c("B-C","A-C","A-B"))) %>%
  ggplot() +
  geom_linerange(aes(x=comp,ymin=conf.low,ymax=conf.high)) +
  geom_hline(yintercept=0,linetype=2,color="blue") +
  geom_point(aes(x=comp,y=estimate),size=3) +
  coord_flip() +
  labs(x="Mean comparison",
       y="Mean difference") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))
  
  







