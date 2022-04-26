# R code to develop Shiny app for running ANOVAs or K-W tests
# Part 1 of 3: exploratory tables and plots, ANOVA assumptions, and transformations

#load packages
library(tidyverse)
library(rstatix)

#### Simulate data-=======================================================================================
### Create three vectors from random normal distribution
set.seed(101)
A<-rnorm(10,5,1)
B<-rnorm(10,8,2)
C<-rnorm(10,10,4)

### Combine vectors into a tibble (long format)
tibble(A,B,C) %>% 
  pivot_longer(cols=everything(),names_to="trmt",values_to="value") -> sampDF


### Create function to make tibble-------------------------------------------------------------------------
samp_maker<-function(n=10){
  tibble(
    A=rnorm(n,10,1),
    B=rnorm(n,14,3),
    C=rnorm(n,18,5)
  ) %>% 
    pivot_longer(cols=everything(),names_to="trmt",values_to="value")
}


#### Exploratory data analysis============================================================================
#create list for summary tables
stat_list<-list(n=length,min=min,median=median,mean=mean,max=max,sd=sd,se=function(x) sd(x)/sqrt(length(x)))

### Summary data table
sampDF %>%
  group_by(trmt) %>%
  summarize(across(value, stat_list,.names="{.fn}"))

### Exploratory plots
## Boxplot
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

## Barplot
sampDF %>%
  ggplot(aes(x=trmt,y=value,fill=trmt)) +
  stat_summary(fun="mean",geom="bar") +
  stat_summary(fun.data="mean_se",geom="errorbar",width=0.3) +
  scale_fill_viridis_d(begin=0,end=0.65) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.title=element_blank(),
        legend.text=element_text(size=12))


### Create functions to build boxplots and barplots
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

boxplotter(sampDF,trmt,value)
barplotter(sampDF,trmt,value)


#### ANOVA assumptions=====================================================================================
### Pull residuals
mod<-lm(value~trmt,data=sampDF)
mod_residDF<-bind_cols(sampDF,resid=resid(mod))


### Test normality assumption
## Graphically
mod_residDF %>%
  ggplot(aes(sample=resid)) +
  stat_qq(color="steelblue") + 
  stat_qq_line() +
  labs(x="Theoretical quantiles",
       y="Standardized residuals") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))
#many outliers at the tails

## Statistically
mod_residDF %>%
  shapiro_test(resid) 
#p=0.0235; non-normal


### Test equal variance assumption
## Graphically 
plot(mod,which=3)
#line is clearly not horizontal, so variance unequal

## Statistically
mod_residDF %>%
  levene_test(value~trmt)
#p=0.0113; unequal variance


### Create functions to draw qq plot in ggplot2------------------------------------------------------------------
## Draw qqplot in ggplot2
qqplotter<-function(df,resid){
mod_residDF %>%
  ggplot(aes(sample={{resid}})) +
  stat_qq(color="steelblue") + 
  stat_qq_line() +
  labs(x="Theoretical quantiles",
       y="Standardized residuals") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))
}

qqplotter(mod_residDF,resid)

#### Data transformations & ANOVA assumptions===========================================================
### Transform data
## Log 
sampDF %>%
  mutate(log_value=log(value)) -> sampDF_log

sampDF %>%
  mutate(log_value=log(value)) %>%
  lm(log_value~trmt,.) -> log_mod

sampDF_log %>%
  bind_cols(resid_log=resid(log_mod)) -> mod_log_residDF

mod_log<-lm(log_value~trmt,data=sampDF_log)
augment(mod_log)[,c("trmt","log_value",".resid")] %>%
  rename(resid=".resid") -> mod_log_residDF


## Square-root
sampDF %>%
  mutate(sqrt_value=sqrt(value)) -> sampDF_sqrt

mod_sqrt<-lm(sqrt_value~trmt,data=sampDF_sqrt)
augment(mod_sqrt)[,c("trmt","sqrt_value",".resid")] %>%
  rename(resid=".resid") -> mod_sqrt_residDF


## Reciprocal
sampDF %>%
  mutate(recip_value=1/value) -> sampDF_recip

mod_recip<-lm(recip_value~trmt,data=sampDF_recip)
augment(mod_recip)[,c("trmt","recip_value",".resid")] %>%
  rename(resid=".resid") -> mod_recip_residDF


### Test assumptions (using log-transform as an example)
## Normality
# Graphically
#hard-coded
mod_log_residDF %>%
  ggplot(aes(sample=resid)) +
  stat_qq(color="steelblue") + 
  stat_qq_line() +
  labs(x="Theoretical quantiles",
       y="Standardized residuals") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))
#highly skewed tail; appears non-normal

#using function
qqplotter(mod_log_residDF,resid)

# Statistically
mod_log_residDF %>%
  shapiro_test(resid) 
#p=0.0054; non-normal


## Equal variance
# Graphically
plot(mod_log,which=3)
#non horizontal; appears unequal

# Statistically
mod_log_residDF %>%
  levene_test(log_value~trmt)
#equal variance


### Create function to build DF of transformed values and residuals-----------------------------------------
data_transformer<-function(data,x,y,func){
  ind<-enquo(x)
  data %>%
    mutate(trans_value=func({{y}})) %>%
    lm(trans_value~ind,.) %>%
    augment() %>%
    select(ind,trans_value,resid=".resid") -> sampDF_trans
  sampDF_trans
}

data_transformer(sampDF,trmt,value,log)




