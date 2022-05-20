####################################################
### META DATA
# Purpose: Shell syntax for One-Way ANCOVA Models
# Version: R 4.0.4
# Author: Shauna McManus
# Date Last Updated: 06/15/2021
####################################################

####################################################
### INSTALL AND LOAD RELEVANT PACKAGES
#install.packages("data.table")
library(data.table)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("rstatix")
library(rstatix)
#install.packages("broom")
library(broom)
#install.packages("VIM")
library(VIM)
#install.packages("finalfit")
library(finalfit)
#install.packages("emmeans")
library(emmeans)
####################################################

####################################################
### LOAD AND EXAMINE DATA STRUCTURE

dat<-load()

str(dat)

# Graphic
# Creates a plot of variable x against variable y, by a grouping variable Group in a dataset data (e.g. score over time by treatment group)
# Adds mean trend lines and 95% CI
time_trend_fig <- function(Data, X, Y, Group, Xtitle, Ytitle, Maintitle) {
  p<-ggplot(Data, aes(x={{X}}, y={{Y}}, color={{Group}}))
  plot<-p + geom_point()+geom_smooth(method=lm)+labs(x=Xtitle,y=Ytitle,
                                                     title=Maintitle,color="Mean Trend Line (95% CI)")+theme(legend.position="bottom")
  return(plot)
}

# Example
#time_trend_fig(Data=dat, X=Time, Y=Score, Group=Treatment, Xtitle="Time", Ytitle="Score", Maintitle="Score vs. Time")

# full sample size: n = 
####################################################

####################################################
### MISSING DATA EXAMINATION

dat %>% missing_plot()
explanatory = c("") # add list of explanatory variables here (e.g. c("SITEID", "TXA", "BIOMARKER", "AGE", "SEX"))
dependent = "" # add name of dependent variable here (e.g. "TWOHR_PAIN_FREED")
dat %>% missing_pattern(dependent, explanatory)
dat %>% missing_pairs(dependent, explanatory)
dat %>% missing_compare(dependent, explanatory) %>% knitr::kable(row.names=FALSE,align=c("l","l","r","r","r"))

# missing data notes:
# sample size with complete data: n = 
####################################################

####################################################
### CHECK ANCOVA ASSUMPTIONS

# Assumption 1 - Linearity
linearity_fig <- function(Data, X, Y, Group) {
  p<-ggscatter(Data, x = X, y = Y, color = Group, add = "reg.line")
  plot<-p+stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group))
  return(plot)
}
# Example
# Include X, Y, and Group variable names in double quotes
# X variable is covariate, Y is dependent variable of interest, Group is grouping variable
#linearity_fig(Data=dat, X=Time, Y=Score, Group=Treatment)

# Assumption 2 - Homogeneity of Slopes / No Interaction
dat %>% anova_test(DV ~ Group*Covariate) # fill in group variable, covariate, and dependent variable of interest
# Homogeneity of slopes if interaction term is not significant

# Assumption 3 - Normality of Residuals
# Fit the model, the covariate goes first
model <- lm(DV ~ Covariate + Group, data = dat) # fill in group variable, covariate, and dependent variable of interest
# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>% select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
head(model.metrics, 3)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)
# If Shapiro Wilk test is not significant (p > 0.05), we can assume normality of residuals

# Assumption 4 - Homogeneity of Variances
model.metrics %>% levene_test(.resid ~ Group) # fill in group variable
# If Levene's test is not significant, we can assume homogeneity of variances

# Assumption 5 - Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
# Any outliers in the data can be determined as cases with standardized residuals greater than 3 in absolute value

####################################################


####################################################
### ANCOVA COMPUTATION

# Reminder - order matters!  You want to remove the effect of the covariate first.
# That is, you want to control for it - prior to entering your main variable or interest.

res.aov <- anxiety %>% anova_test(posttest ~ pretest + group)
get_anova_table(res.aov)

####################################################

####################################################
### POST-HOC COMPARISONS
# Pairwise comparisons
library(emmeans)
pwc <- anxiety %>% 
  emmeans_test(
    posttest ~ group, covariate = pretest,
    p.adjust.method = "bonferroni"
  )
pwc

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc)
####################################################

####################################################
### GRAPHIC REPRESENTATION OF RESULTS
# Visualization: line plots with p-values
pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
ggline(get_emmeans(pwc), x = "group", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

####################################################

####################################################
### REFERENCES
citation("data.table")
citation("ggplot2")
citation("tidyverse")
citation("ggpubr")
citation("rstatix")
citation("broom")
citation("VIM")
citation("finalfit")
citation("emmeans")
####################################################
