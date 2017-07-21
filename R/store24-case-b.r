# **********************************************************************************
#   File        : store24-case-a.r  <--- this should be the exact name of THIS document
#   Author      : Kristina Tobio 
#   Created     : 15 Nov 2016
#   Modified    : 20 Jul 2017
#   Description : .r file for Store24 Case Part B 
#   **********************************************************************************/

# This clears R's data memory so new data can be loaded

rm(list = ls())

#& these commands prepare your computer for the data and analysis
#& this finds and closes open log files
#  NOT NEEDED IN R, as we get automatic transcripts
#  but we can implement a logging function if needed.
# To do so, remove the comments from the following line:

#sink(file = "./logs/store24-case-b", append = FALSE, type = c("output", "message"), split = TRUE)
sink(file = paste0("./logs/store24-case-b_", Sys.Date(),".log"), append = FALSE, type = c("output", "message"), split = TRUE)
#filename <-paste0("./logs/store24-case-b_", Sys.Date(),".log")
#sink(filename)

#& this makes it so you don't have to keep pressing return/enter to scroll through results
#& set more off 
#  NOT NEEDED IN R, but can output truncated results through head() or 

#& this keeps everything visible on a normal monitor or laptop screen
#&set linesize 200    
#  NOT NEEDED IN R

#& cd means "change directory"
#& changes to the current directory 
#& cd .
# NA: In R, working directory set by use of R project. 
# Will use following to list working directory and contents: 

getwd()
list.files()

#install.packages("dplyr")
library(dplyr)

#install.packages('Hmisc')
library('Hmisc')

#install.packages("haven")
library(haven)

#install.packages("readstata13")
library("readstata13")


#PA_Store24B_data.data <-read_dta("data/PA_Store24B_data.dta")
#load("data/PA_Store24B_data.dta")
#PA_Store24B_data <- read.dta13("data/PA_Store24B_data.dta")
#save("PA_Store24B_data.data", file = "data/PA_Store24B_data.Rdata")
#save("PA_Store24B_data", file = "data/PA_Store24B_data")


# The following command reads the PA_Store24B_data data frame, previously saved to file in the 
# "store24-prep.r" script.

load("data/PA_Store24B_data.Rdata")

# The following four commands list the variables, labels and dimensionality of the data frame,
# the print the first 5 rows of the data frame.

# The following command lists the contents of PA_Store24B_data.

contents(PA_Store24B_data.Rdata)

# The following command lists the first 5 rows of the PA_Store24B_data.

head(PA_Store24B_data.Rdata,n=5L)

## install.packages("psych")
library("psych")

#&// RECREATE ALL THE VARIABLES WE CREATED IN PART A
#&// Benefit of Experience - Creating Standardized Coefficients
#&// Re-run the profit regression
#&regress profit mtenure ctenure pop comp visibility ped res hours24
#fit <- lm(profit ~ mtenure, data = PA_Store24B_data)
fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24B_data.Rdata)
summary(fit)

#&// Summarize the variable mtenure, then capture the standard deviation in a local (temporary) variable
#&// Note that "sum" is the same command as "summarize"
#&sum mtenure
#&local a=r(sd)
#&// Here, we create a standardized coefficient for mtenure 
#&// The command _b[VARIABLE] captures the coefficient of VARIABLE in the most recent regression
#&g sco_mtenure=`a'*_b[mtenure]
#&// We could create standardized coefficients for each of the remaining variables in the regression by retyping these 3 lines 7 more times
#&// Or, we could use a loop, which saves us time and reduced visual clutter in our .do file
#&// This loop takes each variable in the list and runs it through the three lines of code automatically
#&foreach var in ctenure pop comp visibility ped res hours24 {
#&sum `var'
#&local a=r(sd)
#&g sco_`var'=`a'*_b[`var']
#&}
#&// Here, we list the values of each our standardized coefficients
#&// A "*" is a wild card, so this command tells Stata to list all the variables that begin with "sco_"
#&list sco_* if _n==1

#  Let's store the model formula in a variable

modelformula <- profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24

# We can use the fact that all.vars gives us a vector of the variable names.

all.vars(modelformula)

# We can use this to subset the dataset accordingly. For instance, we can use the R scale() function to
# "center" and "scale" (i.e., subtract the mean, and divide by their standard deviations) the coefficients
# of the regressed model,

# Note: the lapply() function referenced below will simply apply the scale() function to each variable in the 
# model formula.

PA_Store24B_data_scaled <- lapply(PA_Store24B_data.Rdata[, all.vars(modelformula)], scale) 
PA_Store24B_data.Rdata

# Now we can fit the LM() function to the scaled data 

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24B_data_scaled)
summary(fit)



#& //PART TWO: RELATIVE IMPORTANCE
#& // The incremental R2 can help us to understand the relative importance of variables in the regression
#& regress profit mtenure ctenure pop comp visibility ped res hours24
#& // The "e()" command allows you to capture regression results and put them into a variable.
#& // Here, we capture the adjusted R2, but you can also capture statistics like the number of observations or the F-statistic
#& // Type "help regress" for more detail
#& g r2_full=e(r2_a)

# Using the results of our standardized regression baove, we can store the r-squared of the "full" proft model

r2_full <- summary(fit)$r.squared
r2_full

#& // We will use it to calculate the incremental R2 of mtenure
#& regress profit ctenure pop comp visibility ped res hours24
#& g ir2_mtenure=r2_full-e(r2_a)

# Re-running th emodel while dropping mtenure, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24B_data_scaled)
summary(fit)
ir2_mtenure <- r2_full - summary(fit)$r.squared
ir2_mtenure

#& // We continue to drop variables from the regression in turn, and calculate each incremental R2
#& regress profit mtenure pop comp visibility ped res hours24
#& g ir2_ctenure=r2_full-e(r2_a)

# Re-running th emodel while dropping ctenure, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24B_data_scaled)
summary(fit)
ir2_ctenure <- r2_full - summary(fit)$r.squared
ir2_ctenure

#& regress profit mtenure ctenure comp visibility ped res hours24
#& g ir2_pop=r2_full-e(r2_a)

# Re-running th emodel while dropping pop, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + comp + visibility + pedcount + res + hours24, data = PA_Store24B_data_scaled)
summary(fit)
ir2_pop <- r2_full - summary(fit)$r.squared
ir2_pop

#& regress profit mtenure ctenure pop visibility ped res hours24
#& g ir2_comp=r2_full-e(r2_a)

# Re-running th emodel while dropping comp, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + visibility + pedcount + res + hours24, data = PA_Store24B_data_scaled)
summary(fit)
ir2_comp <- r2_full - summary(fit)$r.squared
ir2_comp

#& regress profit mtenure ctenure pop comp ped res hours24
#& g ir2_visibility=r2_full-e(r2_a)

# Re-running th emodel while dropping visibility, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24B_data_scaled)
summary(fit)
ir2_visibility <- r2_full - summary(fit)$r.squared
ir2_visibility

#& regress profit mtenure ctenure pop comp visibility res hours24
#& g ir2_ped=r2_full-e(r2_a)


# Re-running th emodel while dropping pedcount, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + res + hours24, data = PA_Store24B_data_scaled)
summary(fit)
ir2_pedcount <- r2_full - summary(fit)$r.squared
ir2_pedcount

#& regress profit mtenure ctenure pop comp visibility ped hours24
#& g ir2_res=r2_full-e(r2_a)


# Re-running th emodel while dropping res, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + hours24, data = PA_Store24B_data_scaled)
summary(fit)
ir2_res <- r2_full - summary(fit)$r.squared
ir2_res

#& regress profit mtenure ctenure pop comp visibility ped res
#& g ir2_hours24=r2_full-e(r2_a)

# Re-running th emodel while dropping hours24, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res, data = PA_Store24B_data_scaled)
summary(fit)
ir2_hours24 <- r2_full - summary(fit)$r.squared
ir2_hours24


#& // We can examine the incremental R2 for each variable with the folloing command
#& list ir2* if _n==1

# We can summarize the r-squared values for the full model vs. the incremenAL MODELS:

r2_full
ir2_ctenure
ir2_pop
ir2_comp
ir2_visibility
ir2_pedcount
ir2_res
ir2_hours24

# And we may find it convenient to put all the incremental r-squared values into a common vector"

ir2_matrix <- mapply(get, ls(pattern='ir2*'))
ir2_matrix

#& // Explaining Variation in Profit
#& // To determine the percent of variation in profit NOT explined by our model, we subtract the adjusted R2 from 1
#& g unexplained=1-r2_full

# Explaining Variation in Profit
# To determine the percent of variation in profit NOT explined by our model, we subtract the adjusted R2 from 1
# unexplained=1-r2_full

unexplained <- 1-r2_full
unexplained

#& // To determine the percent of variation in profit explained by people we add the incremental R2 of manager and crew tenures
#& g people=ir2_mtenure+ir2_ctenure

# To determine the percent of variation in profit explained by people we add the incremental R2 of manager and crew tenures

people <- ir2_mtenure+ir2_ctenure

#& // To determine the percent of variation in profit explained by site, we add the incremental R2 of population, competition,
#& // store front visibility, pedestrian foot traffic, residential location, and open 24 hours.
#& g site=ir2_pop+ir2_comp+ir2_visibility+ir2_ped+ir2_res+ir2_hours24

# To determine the percent of variation in profit explained by site, we add the incremental R2 of population, competition,
# store front visibility, pedestrian foot traffic, residential location, and open 24 hours.

site <- ir2_pop+ir2_comp+ir2_visibility+ir2_pedcount+ir2_res+ir2_hours24
site

#& // To determine overlap, we subtract people and site from the full adjusted R2
#& g overlap=r2_full-people-site

# To determine overlap, we subtract people and site from the full adjusted R2

overlap <- r2_full-people-site

#& 
#& // We can examine these results visually with a pie chart
#& graph pie overlap people site unexplain, plabel(_all percent) title(Explaining Variation in Profit)
#& graph save "figures/pie", replace

# We can examine these results visually with a pie chart

# Simple Pie Chart
slices <- c(overlap, people, site, unexplained)
lbls <- c("Overlap", "People", "Site", "Unexplained")
pie(slices, labels = lbls, main="Explaining Variation in Profit")

# Pie Chart with Percentages
slices <- c(overlap, people, site, unexplained)
lbls <- c("Overlap", "People", "Site", "Unexplained")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Explaining Variation in Profit") 

# 3D Exploded Pie Chart
install.packages("plotrix")
library(plotrix)
slices <- c(overlap, people, site, unexplained)
lbls <- c("         Overlap", "People", "Site", "Unexplained")
pie3D(slices,labels=lbls,explode=0.1,
      main="Explaining Variation in Profit")


#& // PART THREE: DIMINISHING RETURNS
# // Create a new variable called MT2 that squares the manager tenure term
#g MT2=mtenure*mtenure

# PART THREE: DIMINISHING RETURNS
# Create a new variable called MT2 that squares the manager tenure term

PA_Store24B_data$MT2 <- PA_Store24B_data$mtenure^2
label(PA_Store24B_data$MT2) <- "Squared Manager Tenure"
contents(PA_Store24B_data)
head(PA_Store24B_data,n=5L)

#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2

# Running the regression while adding the MT2 variable as an explanantory variable:

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24 + MT2, data = PA_Store24B_data)
summary(fit)


#& // because we created some new variables, we want to save this dataset in case we want to use it later
#& save "data/store24-case-data-new-variables.dta", replace

# The following command saves this new version of the data frame as a new RData file

#save(PA_Store24B_data, file = "data/PA_Store24B_data_new_variables.RData")


# // Re-run the profit regression from Part A
#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2
#// Add the service quality variable to the original profit regression
#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2 servqual
#& // Add the manager skill variable to the original profit regression
#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2 mgrskill
#& // Add both variables to the original profit regression
#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2 servqual mgrskill

#& // Part 4: Mediation
#& // Note Parts 1-3 can be found in store24-case-a.do
#& // Step 1: Show that the initial variable is correlated with the income
#&regress profit mtenure ctenure pop comp visibility ped res hours24 MT2 mgrskill
fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24 + MT2 + mgrskill, data = PA_Store24B_data)
summary(fit)
#& // Step 2: Show that the initial variable is correlated with the mediator
#& regress servqual mtenure ctenure pop comp visibility ped res hours24 MT2 mgrskill
#& // Step 3: Show that the mediator affects the outcome variable, controlling for the initial variable.
#& // Step 4: Note whether the initial variable correlates with the outcome variable, controlling for the mediator.
#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2 servqual mgrskill

#& // Part 5: Controlling for Fixed Effects - Time and Site Variables
#& // This requires the use of a new dataset, so we clear the current dataset from Stata's memory, then insheet the new dataset
#& clear
#& insheet using "data/store24-case-data-small-sample.csv", names
#& // Determine the relationship between store quality and profit
#& regress profit quality
#& // Control for the time trend
#& regress profit quality year
#& // Control for the three stores
#& regress profit quality year s1 s2 s3
#& // Note that with dummy varibles, one dummy variable must drop out. 
#& // If we want to control which variable drops out, we can simply drop it from the regression
#& regress profit quality year s2 s3
#& // What happens when we add population?
#& regress profit quality year s1 s2 s3 population
#& // Creating a variable showing the linear relationship between the dummies and population
#& generate example=(s1*pop)+(s2*pop)+(s3*pop)
#& // If we take a glance our data with browse, we see population and example are equal
#& browse
#& *close browse

#& // Close the browse window by clicking the red X in the upper righthand corner
#& // Alternatively, we can use check they are equal without opening the browse window
#& count
#& count if population==example
#& // Testing how regression analysis treats redundant variables
#& generate q2=quality*2
#& regress profit quality year q2
#& // Showing how "quality squared" is treated differently than "quality times two"
#& replace q2=quality*quality
#& regress profit quality year q2



#& // closes your log
#& log close
#& 
#& // drops all data from Stata's memory
#& clear

# This clears R's data memory so new data can be loaded

rm(list = ls())

# The following command redirects output from the log file back to the console, if necessary.

sink()


    

