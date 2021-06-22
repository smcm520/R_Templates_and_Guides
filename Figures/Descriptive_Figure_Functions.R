#####################################################################################################
### META DATA
# Purpose: Define functions to create descriptive figures 
# Version: 3.6.1 
# Author: Shauna McManus
# Date: 2020-11-09
#####################################################################################################

#install.packages("data.table")
library(data.table)
#install.packages("ggplot2")
library(ggplot2)

#### FIGURE 1
# Raw data
# Creates a plot of variable x against variable y, by a grouping variable Group in a dataset data (e.g. score over time by treatment group)
# Adds mean trend lines and 95% CI
time_trend_fig <- function(Data, X, Y, Group, Xtitle, Ytitle, Maintitle) {
	p<-ggplot(Data, aes(x={{X}}, y={{Y}}, color={{Group}}))
    plot<-p + geom_point()+geom_smooth(method=lm)+labs(x=Xtitle,y=Ytitle,
    title=Maintitle,color="Mean Trend Line (95% CI)")+theme(legend.position="bottom")
    return(plot)
}

# EXAMPLE
# DATASET: dat <- readRDS(file = '/Users/Shauna/Desktop/dat_long_01.rds')
time_trend_fig(Data=dat, X=Time, Y=Score, Group=Treatment, Xtitle="Time", Ytitle="Score", Maintitle="Score vs. Time")


#### FIGURE 2
# Raw data
# Creates a plot of variable x against variable y, stratifies into panels by a grouping variable Group in a dataset data (e.g. time against score by treatment group)
# Adds mean trend lines and 95% CI
time_trend_fig_strat <- function(Data, X, Y, Group, Xtitle, Ytitle, Maintitle) {
	p<-ggplot(Data, aes(x={{X}}, y={{Y}}, color={{Group}}))
    plot<-p + geom_point()+geom_smooth(method=lm)+facet_grid(cols=vars({{Group}}))+labs(x=Xtitle,y=Ytitle,title=Maintitle,color="Mean Trend Line (95% CI)")+theme(legend.position="bottom")
    return(plot)
}

# EXAMPLE
# DATASET: dat <- readRDS(file = '/Users/Shauna/Desktop/dat_long_01.rds')
time_trend_fig_strat(Data=dat, X=Time, Y=Score, Group=Treatment, Xtitle="Time", Ytitle="Score", Maintitle="Score vs. Time")

#### FIGURE 3 - Box plot 1
# Overall success vs. failure counts by treatment assignment
# Outcome is success/failure variable
# Group is treatment assignment

outcome_fig_counts <- function(Data, Group, GroupLevels, GroupLabels, Outcome, OutcomeLevels, OutcomeLabels, Xtitle, Ytitle, Maintitle) {
	Data[,{{Group}}]<-factor(Data[,{{Group}}],levels=GroupLevels,labels=GroupLabels)
	Data[,{{Outcome}}]<-factor(Data[,{{Outcome}}],levels=OutcomeLevels,labels=OutcomeLabels)
	p<-ggplot(Data, aes(x=Data[,{{Outcome}}]))
	plot<-p + geom_bar(aes(fill= Data[,{{Outcome}}]))+facet_grid(cols=vars(Data[,{{Group}}]))+labs(x= Xtitle, y= Ytitle, title= Maintitle)+scale_fill_discrete(name = Xtitle)
    return(plot)
}

# EXAMPLE
# DATASET: dat <- readRDS(file="dat_exploratory_01.rds")
# Include variable names in double quotes
outcome_fig_counts(Data=dat, Group="TXA", GroupLevels=c(0,1), GroupLabels=c("Placebo", "Treatment"), OutcomeLevels=c(0,1), OutcomeLabels=c("Failure", "Success"), Outcome="TWOHR_PAIN_FREED", Xtitle="Treatment Outcome", Ytitle="", Maintitle="Outcome, Stratified by Treatment Arm")

#### FIGURE 4 - Box plot 2
# Counts of each treatment assignment within outcome buckets
# NOTE: For function to properly work, success/failure variable and treatment variable must be factors
# Outcome is success/failure variable
# Group is treatment assignment

outcome_fig_txcounts <- function(Data, Group, GroupLevels, GroupLabels, Outcome, OutcomeLevels, OutcomeLabels, Xtitle, Ytitle, Maintitle, Grouptitle) {
	Data[,{{Group}}]<-factor(Data[,{{Group}}],levels=GroupLevels,labels=GroupLabels)
	Data[,{{Outcome}}]<-factor(Data[,{{Outcome}}],levels=OutcomeLevels,labels=OutcomeLabels)
	p<-ggplot(Data, aes(x=Data[,{{Group}}]))
	plot<-p + geom_bar(aes(fill= Data[,{{Group}}]))+facet_grid(cols=vars(Data[,{{Outcome}}]))+labs(x= Xtitle, y= Ytitle, title=Maintitle)+scale_fill_discrete(name = Grouptitle)
    return(plot)
}

# EXAMPLE
# DATASET: dat <- readRDS(file="dat_exploratory_01.rds")
# Include variable names in double quotes
outcome_fig_txcounts(Data=dat, Group= "TXA", GroupLevels=c(0,1), GroupLabels=c("Placebo", "Treatment"), Outcome ="TWOHR_PAIN_FREED", OutcomeLevels=c(0,1), OutcomeLabels=c("Failure", "Success"), Xtitle="", Ytitle="", Maintitle="Outcome, Stratified by Treatment Arm", Grouptitle="Treatment")


#### FIGURE 5 - Change in Score from Baseline Over Time
# Creates a plot of variable x against summary variable y by a grouping variable Group in a dataset data (e.g. time against mean score change by treatment group)

# Add baseline to all rows
dat_change <- as.data.table(dat)
dat_change[,change_from_base:= Score-Score[1],by= USUBJID]

time_trend_fig_mean <- function(Data, ID, Y, X, Group, Xtitle, Ytitle, Maintitle, Grouptitle) {
	#mean_change_from_base<-
	p<-ggplot(Data, aes(x={{X}}, y={{Y}}, color={{Group}}))
    plot<-p + geom_point()+geom_smooth(method=lm)+labs(x=Xtitle,y=Ytitle,
    title=Maintitle,color=Grouptitle)+theme(legend.position="bottom")
    return(plot) 
    }

# EXAMPLE
# DATASET: dat <- readRDS(file = '/Users/Shauna/Desktop/dat_long_01.rds')
time_trend_fig_mean(Data=dat_change, ID= USUBJID, Y= change_from_base, X=Time, Group=Treatment, Xtitle="Time", Ytitle="Change in Score from Baseline", Maintitle="Change in Score from Baseline vs. Time", Grouptitle="Treatment")

#### FIGURE 6 - Mean Change in Score from Baseline Over Time - NOTE: Still working on getting this into a function
# Summary Data

# EXAMPLE
# DATASET: dat <- readRDS(file = '/Users/Shauna/Desktop/dat_long_01.rds')

# Add baseline to all rows
dat_change <- as.data.table(dat)
dat_change[,change_from_base:= Score-Score[1],by= USUBJID]

# Stratify by treatment
TX1<-dat_change[dat_change$Treatment=="Tx_1"]
TX2<-dat_change[dat_change$Treatment=="Tx_2"]

summ_stats_tx1<-do.call(rbind , by(TX1$Score, TX1$Time, summary))
summ_stats_tx2<-do.call(rbind , by(TX2$Score, TX2$Time, summary))

means_tx1<-summ_stats_tx1[,4]
means_tx2<-summ_stats_tx2[,4]
times_tx1<-unique(TX1$Time)
times_tx2<-unique(TX2$Time)
means<-c(means_tx1, means_tx2)
times<-c(times_tx1, times_tx2)
treatment<-c("Tx_1","Tx_1","Tx_1","Tx_1","Tx_1","Tx_2","Tx_2","Tx_2","Tx_2","Tx_2")
dat_sum<-data.frame(times, treatment, means)

p4 <- ggplot(dat_sum, aes(x= times, y= means,  color=treatment))
# Add labels and mean trend lines, stratify by treatment using faceting
p4 + geom_point()+geom_smooth(method=lm)+labs(x="Time",y="Mean Change in Score", title="Mean Change in Score vs. Time", color="Treatment")+theme(legend.position="bottom")
