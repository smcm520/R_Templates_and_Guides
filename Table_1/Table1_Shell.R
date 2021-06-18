#####################################################################################################
### META DATA
# Version: 3.6.1 
# Author: Shauna McManus
# Date: 2020-04-21
# Additional information: http://www.danieldsjoberg.com/gtsummary/articles/gallery.html
#####################################################################################################

#####################################################################################################
### INSTALL AND LOAD PACKAGES AS NEEDED
#install.packages("gtsummary")
#install.packages("dplyr")
#install.packages("flextable")
library(dplyr)
library(gtsummary)
library(flextable)
#####################################################################################################

#####################################################################################################
#### EXAMPLE
# DATASET: dat <- readRDS(file = '/Users/Shauna/Desktop/dat_long_01.rds')
#####################################################################################################

#####################################################################################################
### LOAD AND EXAMINE DATA
dat <- readRDS(file = 'dat_long_01.rds') #change command as needed to match file format
str(dat) # QC check: make sure variables that should be continuous are numeric, variables that should be factors as factors, etc.

# Factor any variables as needed; as labels, levels as needed
# dat$VAR <- as.factor(dat$VAR, labels=c(), levels=c())
#####################################################################################################

#####################################################################################################
### BASIC TABLE 1
dat2 <- dat %>% select(Treatment, Score, Biomarker, Time) # Fill in desired variables here if subsetting full dataset
dat2 %>% tbl_summary()

# Common modification - do not show missing data as separate categories
dat2 %>% tbl_summary(missing = "no")
#####################################################################################################

#####################################################################################################
### STRATIFIED TABLE 1
dat2 %>% tbl_summary(by=) # Basic stratified
dat2 %>% tbl_summary(by=) %>% add_p() # Adds in tests for group differences
dat2 %>% tbl_summary(by=) %>% add_n() # add column with total number of non-missing observations

# Can assign tables to a named object, e.g.: tb1name<-dat2 %>% tbl_summary(by=) %>% add_n()
# EXAMPLE: dat2 %>% tbl_summary(by=Treatment) %>% add_p() 
#####################################################################################################

#####################################################################################################
### OTHER HELPFUL MODIFICATIONS
# Make the variable labels bold font
# %>% bold_labels()

# Change variable labels (fill in, expand list as needed)
# %>% label = list(var1 ~ "Label for Var 1", var2 ~ "Label for Var 2")

# Update the column header for the far right column (fill in for "VARIABLE")
# %>% modify_header(label = "**VARIABLE**") 

# When stratifying, add a merged column label over all stratified stat columns (fill in for "HEADER")
# %>% modify_spanning_header(all_stat_cols() ~ "**HEADER**")
#####################################################################################################

#####################################################################################################
### SAVING TABLES
tb1name %>% as_gt() %>% gt::gtsave(filename = ".html") # Replace "tblname" with table object name, use extensions .html .tex .ltx .rtf, add file name before "."
tb1name %>% as_flex_table() %>% flextable::save_as_docx(path=".docx") # Replace "tblname" with table object name, add file name before ".docx"
#####################################################################################################

#####################################################################################################
### REFERENCES
citation("dplyr")
citation("gtsummary")
citation("flextable")
#####################################################################################################
