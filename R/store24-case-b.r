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


#PA_Store24B_data <- read.dta13("data/PA_Store24B_data.dta")
#save("PA_Store24B_data", file = "data/PA_Store24B_data.Rdata")


# The following command reads the PA_Store24B_data data frame, previously saved to file in the 
# "store24-prep.r" script.

load("data/PA_Store24B_data.Rdata")

# The following four commands list the variables, labels and dimensionality of the data frame,
# the print the first 5 rows of the data frame.

# The following command lists the contents of PA_Store24B_data.

#contents("data/PA_Store24B_data.Rdata")

# The following command lists the first 5 rows of the PA_Store24B_data.

head("PA_Store24B_data",n=5L)

## install.packages("psych")
library("psych")

# creating some variables from the Part A case
# Create a new variable called MT2 that squares the manager tenure term

PA_Store24B_data$MT2 <- PA_Store24B_data$mtenure^2
label(PA_Store24B_data$MT2) <- "Squared Manager Tenure"
contents(PA_Store24B_data)
head(PA_Store24B_data,n=5L)

#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2

# Running the regression while adding the MT2 and service quality variable as an explanantory variable:

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24 + MT2 + servqual, data = PA_Store24B_data)
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
fit <- lm(servqual ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24 + MT2 + mgrskill, data = PA_Store24B_data)
summary(fit)
#& // Step 3: Show that the mediator affects the outcome variable, controlling for the initial variable.
#& // Step 4: Note whether the initial variable correlates with the outcome variable, controlling for the mediator.
#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2 servqual mgrskill
fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24 + MT2 + mgrskill + servqual, data = PA_Store24B_data)
summary(fit)

#& // Part 5: Controlling for Fixed Effects - Time and Site Variables
#& // This requires the use of a new dataset, so we clear the current dataset from Stata's memory, then insheet the new dataset
#& clear
store24.small.data <- read.csv("data/store24-case-data-small-sample.csv")
head(store24.small.data,n=5L)
#& // Determine the relationship between store quality and profit
#& regress profit quality
fit <- lm(profit ~ quality, data = store24.small.data)
summary(fit)
#& // Control for the time trend
#& regress profit quality year
fit <- lm(profit ~ quality + year, data = store24.small.data)
summary(fit)
#& // Control for the three stores
#& regress profit quality year s1 s2 s3
fit <- lm(profit ~ quality + year + s1 + s2 + s3, data = store24.small.data)
summary(fit)
#& // Note that with dummy varibles, one dummy variable must drop out. 
#& // If we want to control which variable drops out, we can simply drop it from the regression
#& regress profit quality year s2 s3
fit <- lm(profit ~ quality + year + s2 + s3, data = store24.small.data)
summary(fit)
#& // What happens when we add population?
#& regress profit quality year s1 s2 s3 population
fit <- lm(profit ~ quality + year + s1 + s2 + s3 + population, data = store24.small.data)
summary(fit)
#& // Creating a variable showing the linear relationship between the dummies and population
#& generate example=(s1*pop)+(s2*pop)+(s3*pop)
store24.small.data$example <- (store24.small.data$s1*store24.small.data$population)+(store24.small.data$population)+(store24.small.data$s3*store24.small.data$population) 
#label(PA_Store24B_data$MT2) <- "Squared Manager Tenure"
#contents(PA_Store24B_data)
#head(PA_Store24B_data,n=5L)
head(store24.small.data,n=5L)

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


    

