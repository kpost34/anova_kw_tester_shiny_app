# Functions for ANOVA Kruskal-Wallis Tester Shiny App

#load packages
library(here)
library(tidyverse)
library(broom)
library(rstatix)


### I. Functions for testing ANOVA assumptions and transforming data======================================
## Function to make tibble from simulated data
samp_maker<-function(g=3,n=10){
  tibble(
    A=rnorm(n,10,1),
    B=rnorm(n,14,3),
    C=rnorm(n,18,5),
    D=rnorm(n,15,0.5),
    E=rnorm(n,20,3)
  ) %>% 
    select(1:g) %>%
    pivot_longer(cols=everything(),names_to="trmt",values_to="value")
}


## Functions to make boxplots and bar plots
# Boxplots
boxplotter<-function(df,x,y){
  df %>%
    ggplot(aes(x={{x}},y={{y}})) +
    geom_boxplot(outlier.shape=NA) +
    geom_point(aes(color={{x}}),position=position_jitterdodge()) +
    scale_color_viridis_d(begin=0,end=0.65) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13),
          legend.position="bottom",
          legend.title=element_blank(),
          legend.text=element_text(size=12))
}


# Barplots
barplotter<-function(df,x,y){
  df %>%
    ggplot(aes(x={{x}},y={{y}},fill={{x}})) +
    stat_summary(fun="mean",geom="bar") +
    stat_summary(fun.data="mean_se",geom="errorbar",width=0.3) +
    scale_fill_viridis_d(begin=0,end=0.65) +
    scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13),
          legend.title=element_blank(),
          legend.text=element_text(size=12))
}


## Function to draw qqplot in ggplot2
qqplotter<-function(model){
  resid(model) %>%
  as_tibble() %>%
  ggplot(aes(sample=value)) +
  stat_qq(color="steelblue") + 
  stat_qq_line() +
  labs(x="Theoretical quantiles",
       y="Standardized residuals") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))
}


## Function to calculate reciprocal value
recip<-function(x) {
  1/x
}



### II. Functions for performing ANOVA and post-hoc Tukey HSD tests======================================
## Function to build ANOVA table
anova_tabler<-function(model){
  anova(model) %>%
    rownames_to_column() %>%
    as_tibble() %>% 
    select(source="rowname",SS=`Sum Sq`,DF="Df",MS=`Mean Sq`,F=`F value`,p=`Pr(>F)`) -> anova_tab
  anova_tab
}


## Function to graph Tukey HSD results
tukey_plotter<-function(model){
  tukey_hsd(model) %>%
    select(-null.value) %>%
    mutate(comp=paste(group1,group2,sep="-"),
           comp=fct_rev(fct_inorder(comp))) %>%
    ggplot() +
    geom_linerange(aes(x=comp,ymin=conf.low,ymax=conf.high)) +
    geom_hline(yintercept=0,linetype=2,color="blue") +
    geom_point(aes(x=comp,y=estimate),size=3) +
    geom_text(aes(x=comp,y=estimate,label=paste0("p=",p.adj)),nudge_x=.1) +
    coord_flip() +
    labs(x="Mean comparison",
         y="Mean difference") +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13))
}



### III. Functions to run Kruskal-Wallis and post-hoc tests===============================================
## none needed (as of this moment)