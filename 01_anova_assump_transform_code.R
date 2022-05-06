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

samp_maker(3,10)
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
#create and store model
mod<-lm(value~trmt,data=sampDF)


### Test normality assumption
## Graphically
resid(mod) %>%
  as_tibble() %>%
  ggplot(aes(sample=value)) +
  stat_qq(color="steelblue") + 
  stat_qq_line() +
  labs(x="Theoretical quantiles",
       y="Standardized residuals") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))
#many outliers at the tails

## Statistically
shapiro_test(resid(mod))
#p=0.0235; non-normal


### Test equal variance assumption
## Graphically 
plot(mod,which=3,caption=NULL)
#line is clearly not horizontal, so variance unequal

## Statistically
levene_test(sampDF,value~trmt)
#p=0.0113; unequal variance


### Create functions to draw qq plot in ggplot2------------------------------------------------------------------
## Draw qqplot in ggplot2
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

qqplotter(mod)

#### Data transformations & ANOVA assumptions===========================================================
### Transform data
## Log 
# Create DF with transformed response variable
sampDF %>%
  mutate(log_value=log(value)) -> sampDF_log

# Create model object
sampDF_log %>%
  lm(log_value~trmt,.) -> mod_log


## Square-root
sampDF %>%
  mutate(sqrt_value=sqrt(value)) -> sampDF_sqrt

sampDF_sqrt %>%
  lm(sqrt_value~trmt,.) -> mod_sqrt


## Reciprocal
sampDF %>%
  mutate(recip_value=1/value) -> sampDF_recip

sampDF_recip %>%
  lm(recip_value~trmt,.) -> mod_recip


### Test assumptions (using log-transform as an example)
## Normality
# Graphically
#hard-coded
resid(mod_log) %>%
  as_tibble() %>%
  ggplot(aes(sample=value)) +
  stat_qq(color="steelblue") + 
  stat_qq_line() +
  labs(x="Theoretical quantiles",
       y="Standardized residuals") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))
#highly skewed tail; appears non-normal

#using function
qqplotter(mod_log)

# Statistically
shapiro_test(resid(mod_log)) 
#p=0.0054; non-normal


## Equal variance
# Graphically
plot(mod_log,which=3)
#non horizontal; appears unequal

# Statistically
levene_test(sampDF_log,log_value~trmt)
#equal variance


### Create function to calculate reciprocal----------------------------------------------------------------
recip<-function(x) {
  1/x
}


