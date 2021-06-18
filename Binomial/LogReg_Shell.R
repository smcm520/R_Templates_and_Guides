#####################################################################################################
### META DATA
# Topic: Table and graphic representation of results from logistic/orginal regression models (using glm command)
# Version: 3.6.1 
# Author: Shauna McManus
# Date: 2020-03-20
# Additional information: http://www.danieldsjoberg.com/gtsummary/articles/gallery.html
# Additional information: http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html
# Additional information: https://gt.rstudio.com/index.html

#####################################################################################################


#####################################################################################################
### INSTALL AND LOAD PACKAGES AS NEEDED
#install.packages("gtsummary")
#install.packages("dplyr")
#install.packages("flextable")
#install.packages("ggplot2")
library(dplyr)
library(gtsummary)
library(flextable)
library(ggplot2)

#####################################################################################################


#####################################################################################################
#### EXAMPLE
# Trial data set, included in gtsummary package
dat<-trial
str(dat)

#####################################################################################################


#####################################################################################################
### LOAD AND EXAMINE DATA
dat <- readRDS(file = '') #change command as needed to match file format
str(dat) # QC check: make sure variables that should be continuous are numeric, variables that should be factors as factors, etc.

# Factor any variables as needed; as labels, levels as needed
# dat$VAR <- as.factor(dat$VAR, labels=c(), levels=c())

#####################################################################################################


#####################################################################################################
### FIT MODEL

# Use glm command, change model, dataset, family as needed

# Example with trial data:
m1 <- glm(response ~ age + stage, trial, family = binomial)
summary(m1)

#####################################################################################################


#####################################################################################################
### RESULTS TABLE, BASIC
# Use tbl_regression command
# Exponentiate = TRUE indicates that coefficients (log-odds) should be exponentiated, so that odds ratios are reported

# Example with trial data:
m1 %>% tbl_regression(exponentiate = TRUE)

#####################################################################################################


#####################################################################################################
### RESULTS TABLE, MODIFICATIONS

# Other modifications to add inside parenthetical in tbl_regression() command
# label=  = list(var1 ~ "Label for Var 1", var2 ~ "Label for Var 2"); modify variable labels in table (fill in, expand list as needed)
# exponentiate= exponentiate model coefficients (almost always want to use TRUE)
# include= names of variables to include in output. Default is all variables
# show_single_row= By default, categorical variables are printed on multiple rows; if a variable is dichotomous and you wish to print the regression coefficient on a single row, include the variable name(s) here.
# conf.level= confidence level of confidence interval
# intercept= indicates whether to include the intercept
# estimate_fun= function to round and format coefficient estimates
#pvalue_fun= function to round and format p-values

# Other modifications to add after tbl_regression command
# %>% add_global_p() adds the global p-value for a categorical variables
# %>% add_glance_source_note() adds statistics from `broom::glance()` as source note
# %>% add_vif() adds column of the variance inflation factors (VIF)
# %>% add_q() add a column of q values to control for multiple comparisons
# %>% modify_header(label = "**VARIABLE**") update column headers (fill in text for VARIABLE)
# %>% modify_footnote() update column footnote
# %>% modify_spanning_header(all_stat_cols() ~ "**HEADER**") update spanning headers (fill in text for HEADER)
# %>% modify_caption() update table caption/title
# %>% bold_labels() bold variable labels
# %>% bold_levels() bold variable levels
# %>% italicize_labels() italicize variable labels
# %>% italicize_levels() italicize variable levels
# %>% bold_p() bold significant p-values
# %>% add_significance_stars() to add ** code for significant p-vals such that *p<0.05; **p<0.01; ***p<0.001

# Example with trial data:
m1 %>% tbl_regression(exponentiate = TRUE) %>% italicize_levels() %>% add_global_p() %>% add_significance_stars() 
#####################################################################################################



#####################################################################################################
### SAVING TABLES
tb1name %>% as_gt() %>% gt::gtsave(filename = ".html") # Replace "tblname" with table object name, use extensions .html .tex .ltx .rtf, add file name before "."
tb1name %>% as_flex_table() %>% flextable::save_as_docx(path=".docx") # Replace "tblname" with table object name, add file name before ".docx"

#####################################################################################################

#####################################################################################################
### GRAPHIC
# Example using trial data
# Start at 2 to remove interecept
# Change m1 to reflect named model output
coef<-cbind(summary(m1)$coefficients[2:length(m1$coefficients),1])
SEs<-cbind(summary(m1)$coefficients[2:length(m1$coefficients),2])
cilow<-cbind(summary(m1)$coefficients[2:length(m1$coefficients),1]-qnorm(0.975)*summary(m1)$coefficients[2:length(m1$coefficients),2])
cihigh<-cbind(summary(m1)$coefficients[2:length(m1$coefficients),1]+qnorm(0.975)*summary(m1)$coefficients[2:length(m1$coefficients),2])

#Create labels for plot
boxLabels = c("Age", "StageT2", "StageT3", "StageT4") # Fill in as needed for each separate variable (each level included separately for factor variables)

df <- data.frame(yAxis = length(boxLabels):1, boxOdds = coef, boxCILow = cilow, boxCIHigh = cihigh)

p <- ggplot(df, aes(x = boxOdds, y = boxLabels))

# Fill in title under "ggtitle" sections
p + geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") + geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +geom_point(size = 3.5, color = "orange") + coord_trans(x = scales:::exp_trans(10)) +scale_x_continuous(breaks = log(x = 0.5 * (1:10)), minor_breaks = NULL, labels = (0.5 * (1:10))) + theme_bw()+theme(panel.grid.minor = element_blank()) +ylab("") +xlab("Odds ratio") + ggtitle("Title")

#####################################################################################################


#####################################################################################################
### SAVING GRAPHIC
# As jpeg
# Run jpeg(file="filename.jpeg") PRIOR to running plot in R
# Run dev.off() after running plot

# As png
# Run png(file="filename.png", width=600, height=350) PRIOR to running plot in R, can change width and height
# Run dev.off() after running plot

# Example using trial data, saving as jpeg:
jpeg(file="exampleplot.jpeg")
p <- ggplot(df, aes(x = boxOdds, y = boxLabels))
p + geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") + geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +geom_point(size = 3.5, color = "orange") + coord_trans(x = scales:::exp_trans(10)) +scale_x_continuous(breaks = log(x = 0.5 * (1:10)), minor_breaks = NULL, labels = (0.5 * (1:10))) + theme_bw()+theme(panel.grid.minor = element_blank()) +ylab("") +xlab("Odds ratio") + ggtitle("Odds of Treatment Response")
dev.off()

# Example using trial data, saving as png:
png(file="exampleplot.png",width=600, height=350)
p <- ggplot(df, aes(x = boxOdds, y = boxLabels))
p + geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") + geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +geom_point(size = 3.5, color = "orange") + coord_trans(x = scales:::exp_trans(10)) +scale_x_continuous(breaks = log(x = 0.5 * (1:10)), minor_breaks = NULL, labels = (0.5 * (1:10))) + theme_bw()+theme(panel.grid.minor = element_blank()) +ylab("") +xlab("Odds ratio") + ggtitle("Odds of Treatment Response")
dev.off()

#####################################################################################################


#####################################################################################################
### REFERENCES
citation("dplyr")
citation("gtsummary")
citation("flextable")
citation("ggplot2")

#####################################################################################################
