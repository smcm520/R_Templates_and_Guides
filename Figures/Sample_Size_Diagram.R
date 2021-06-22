#####################################################################################################
### META DATA
# Purpose: Generate flow charts to illustrate sample size 
# Version: 3.6.1 
# Author: Shauna McManus
# Date: 2020-04-21
# Additional Info: https://cran.r-project.org/web/packages/Gmisc/vignettes/Grid-based_flowcharts.html
#####################################################################################################

#####################################################################################################
### INSTALL AND LOAD PACKAGES AS NEEDED
#install.packages("Gmisc")
#install.packages("glue")
#install.packages("htmlTable")
#install.packages("grid")
#install.packages("magrittr")
library(Gmisc)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)
#####################################################################################################

#####################################################################################################
### STEP 1 - DEFINE EACH SAMPLE SIZE COHORT WITHIN TEXTBOXES
# Define each step in the flow chart using boxGrob(glue()) command combination
# boxGrob creates a text box
# Use glue() within first (label) argument to add multiple pieces of text
# Within glue() command, include .sep="\n" to force each part of text onto different lines
# Want basic forumala to look like this:
	# boxGrob(glue("INSERT OVERALL BOX LABEL", "n={pop}", pop=txtInt(INSERT SAMPLE SIZE),.sep = "\n")
	# Can manually enter sample size, or put in assigned R variable
	# Note: can add more lines of text (e.g. subpopulation info) by adding additional labels and population 			indicators, such as: boxGrob(glue("INSERT OVERALL BOX LABEL: (n={pop})", "SUB POPULATION LEVEL 1 {pop1}", SUB 	POPULATION LEVEL 2 {pop2}", pop=txtInt(INSERT SAMPLE SIZE), pop1=txtInt(INSERT SUBSAMPLE SIZE 1), 				pop1=txtInt(INSERT SUBSAMPLE SIZE 2), .sep = 	"\n")
# Default is to have center-justified text; can adjust this by adding just = "left" or just = "right" to end of boxGrob() command (i.e., after the glue() command, within boxGrob()

# Example
# Defines an original overall population size, eligible population size, analytic sample size, the sample size of the excluded participants, sample size assigned to treatment a, and sample size assigned to treatment b
# The sample size of the excluded participants includes 2 subsample populations giving exclusion reasons, and has left justified text

org_cohort <- boxGrob(glue("Overall Population", "n = {pop}", pop = txtInt(1632798), .sep = "\n"))

eligible <- boxGrob(glue("Eligible", "n = {pop}", pop = txtInt(10032), .sep = "\n"))

included <- boxGrob(glue("Analytic Sample", "n = {pop}", pop = txtInt(122), .sep = "\n"))

excluded <- boxGrob(glue("Excluded (n = {tot}):", " - Non-Compliant: {uninterested}", " - Lost to Dropout: {contra}", tot = 30, uninterested = 12, contra = 30 - 12, .sep = "\n"), just = "left") # note: left justified text

grp_a <- boxGrob(glue("Treatment A", "n = {pop}", pop = txtInt(43), .sep = "\n"))

grp_b <- boxGrob(glue("Treatment B", "n = {pop}", pop = txtInt(122 - 43 - 30), .sep = "\n"))

#####################################################################################################

#####################################################################################################
### STEP 2 - SET UP GRID LAYOUT FOR FIGURE

# Generally want to align boxes in either a horizontal or a vertical row
# Functions used for this are alignHorizontal() / alignVertical() and spreadHorizontal() / spreadVertical()
# Basic steps:
	# 1. Use grid.newpage() to set up a new blank grid template
	# 2. Set up the basic figure first (e.g. the main horizontal trend or the main vertical trend)
	# 3. Add in any levels with multiple boxes (e.g., a level with 2 horizontal boxes side by side when the main 		figure is vertical) using alignVertical %>% spreadVertical (or alignHorizontal %>% spreadHorizontal)
	# 4. Add in any figures that are offshoots between other boxes

# Example
# 1. Cast grid to blank
grid.newpage()

# 2. Set up basic up/down figure
vert <- spreadVertical(org_cohort,
                       eligible = eligible,
                       included = included,
                       grps = grp_a)
						
# 3. Add in the second treatment group horizontally across from and on the same level as the first group (Esentially you are creating a new variable with 2 horizontal boxes, in the same location as the initial reference box from the vertical figure, and then resetting the initial 1-box level from step 2 to be null)
grps <- alignVertical(reference = vert$grps,
                      grp_a, grp_b) %>%
  spreadHorizontal()
vert$grps <- NULL
 

# 4. Add excluded textbox to the side between eligible and analytic samples
excluded <- moveBox(excluded,
                    x = .8,
                    y = coords(vert$included)$top + distance(vert$eligible, vert$included, half = TRUE, center = FALSE))

#####################################################################################################

#####################################################################################################
### STEP 3 - CONNECT TABLE WITH ARROWS AND PRINT

# Use connectGrob() command to add arrows to figures
# Basic use:
	# connectGrob(start, end, type)
	# start is name of starting box
	# end is name of ending box
# connectGrob() includes a type argument:
	# Options are "vertical", "horizontal", "L", "-", "Z", "N"
	# The L generates a straight line up/down and then turns to right/left for connecting with the end
	# The - generates a straight horizontal arrow
	# The Z creates a horizontal line that looks like a Z with 90 degree turns
	# The option N allows for vertical lines from one box to multiple boxes at the same level
# Print by calling the main box objects

# Example
# Adds vertical lines between each box in the main vertical arrangement
for (i in 1:(length(vert) - 1)) {
	connectGrob(vert[[i]], vert[[i + 1]], type = "vertical") %>% print }
	
# Adds dual vertical lines down from the bottom of the main vertical arrangement to each treatment group
connectGrob(vert$included, grps[[1]], type = "N")
connectGrob(vert$included, grps[[2]], type = "N")

# Adds a horizzontal line out to the excluded box
connectGrob(vert$eligible, excluded, type = "L")

# Print to add textboxes
vert
grps 
excluded
#####################################################################################################

#####################################################################################################
### STEP 4 - SAVING FIGURES
# As jpeg
# Run jpeg(file="filename.jpeg") PRIOR to running figure in R
# Run dev.off() after running plot

# As png
# Run png(file="filename.png", width=600, height=350) PRIOR to running figure in R, can change width and height
# Run dev.off() after running plot


# Example with jpeg
# Example using trial data, saving as jpeg:
jpeg(file="exampleplot.jpeg")
for (i in 1:(length(vert) - 1)) {
	connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>% print }
	
connectGrob(vert$included, grps[[1]], type = "N")
connectGrob(vert$included, grps[[2]], type = "N")
connectGrob(vert$eligible, excluded, type = "L")
vert
grps 
excluded
dev.off()

# Example with png:
png(file="exampleplot.png",width=600, height=350)
for (i in 1:(length(vert) - 1)) {
	connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>% print }
	
connectGrob(vert$included, grps[[1]], type = "N")
connectGrob(vert$included, grps[[2]], type = "N")
connectGrob(vert$eligible, excluded, type = "L")
vert
grps 
excluded
dev.off()

#####################################################################################################

#####################################################################################################
### OTHER COMMON MODIFICATIONS

# Change textbox color
# Use box_gp = gpar(fill = "", col = "") argument within boxGrob()
# fill = full color; col = line color
# See sample color list here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# Example
boxGrob("Sample Text", box_gp = gpar(fill = "lavender", col = "darkorchid4"))

# Change color and size of text within textboxes
# Use txt_gp(col="", cex=) argument within boxGrob()
# See sample color list here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# cex sizes range where 1=default, 1.5 is 50% larger, 0.5 is 50% smaller, etc.
# Example
boxGrob("Sample Text", txt_gp = gpar(col = "darkblue", cex = 2))

# Change arrow color and thickness
# Use lty_gp=gpar(lwd=, col="", fill="") argument within connectGrob() command
# col=line color; fil= fill color; lwd = line thickness
# See sample color list here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# Example
connectGrob(vert$eligible, excluded, type = "L", lty_gp = gpar(lwd = 2, col = "darkred", fill = "darkred"))
#####################################################################################################

#####################################################################################################
### REFERENCES
citation("Gmisc")
citation("glue")
citation("htmlTable")
citation("grid")
citation("magrittr")

#####################################################################################################
