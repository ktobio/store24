# **********************************************************************************
#   File        : store24-case-a.r  <--- this should be the exact name of THIS document
#   Author      : Troy Adair 
#   Created     : 15 Nov 2016
#   Modified    : 07 Jul 2017
#   Description : .r file for Store24 Case Part A 
#   **********************************************************************************/

#& setwd("C:/Users/troya/Documents/Git Repos/jpolzer__mba_class_stata2r/R/Store24/data")

# NA: In R, working directory set by use of R project 

# This clears R's data memory so new data can be loaded

rm(list = ls())

#& these commands prepare your computer for the data and analysis
#& this finds and closes open log files
#  NOT NEEDED IN R, as we get automatic transcripts
#  but we can implement a logging function if needed.
# To do so, remove the comments from the following line:

sink(file = "./logs/store24-case-a", append = FALSE, type = c("output", "message"), split = TRUE)

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

#& use "data/PA_Store24A_data.dta"

# The following command reads the PA_Store24A_data data frame, previously saved to file in the 
# "store24-prep.r" script.

load("data/PA_Store24A_data.RData")

# The following four commands list the variables, labels and dimensionality of the data frame,
# the print the first 5 rows of the data frame.

# The following command lists the contents of PA_Store24A_data.

contents(PA_Store24A_data)

# The following command lists the first 5 rows of the PA_Store24A_data.

head(PA_Store24A_data,n=5L)


#& // A FIRST CUT AT THE DATA 
#& 
#& // displays summary statistics for a subset of variables
#& summarize sales profit mtenure ctenure pop comp visibility pedcount res hours24

# To facilite the summarization and description of variables, we will be makig use of the "psych"
# package in this script. The following commands download & install, then load, this package.

# Note 1: If you have not already previously downlaoded and installed this package, please
# uncomment the "install" command below.

# Note 2: If you have previosuly downloaded and installed this package, but the "install" command below is
# uncommented, you will be prompted that the package already exists, and asked if you want to re-start R. There
# is no need to restart R, if so prompted, and you may ignore any warnings about the package not being installed
# due to being in use if it is # already installed.

## install.packages("psych")
library(psych)

# The following command uses the describe() function from "psych" to list descriptove statistics for the 2nd
# through 11th columns (i.e., variables) in the data frame.

describe(PA_Store24A_data[,c(2:11)])

# Alternatively, you can use describe(PA_Store24A_data[,-1]) 

#& // display summary statistics for the the most profitable and least profitable stores


#& // because we have sorted profit in descending order, observations 1-10 are the most profitable stores

# The following command sorts the PA_Store24A_data data fram in descending order based on the column 3 variable, profit.

PA_Store24A_data <- PA_Store24A_data[order(-PA_Store24A_data[,3]),]

#& list store profit mtenure ctenure if _n<=10

# The following command uses the head() function to list the values for the first (i.e., "top", becuase the data is 
# sorted in descending order) 10 stores.

head(PA_Store24A_data[,c(1,3:5)],n=10L)

#& display summary statistics for top 10 stores

# We once again make use of the "psych" describe() function, subsetting to include only the first 10 rows of the data frame,
# and excluding descriptive statistics on column 1 (i.e., the )

describe(PA_Store24A_data[1:10,-1]) 

#& // because we have sorted profit in descending order, observations 1-10 are the least profitable stores

# The following command sorts the PA_Store24A_data data fram in ascending order based on the column 3 variable, profit.

PA_Store24A_data <- PA_Store24A_data[order(PA_Store24A_data[,3]),]

#& list store profit mtenure ctenure if _n<=10

# The following command uses the head() function to list the values for the first (i.e., "bottom", becuase the data is 
# sorted in descending order) 10 stores.

head(PA_Store24A_data[,c(1,3:5)],n=10L)

#& display summary statistics for bottom 10 stores

# We once again make use of the "psych" describe() function, subsetting to include only the first 10 rows of the data frame,
# and excluding descriptive statistics on column 1 (i.e., the )

describe(PA_Store24A_data[1:10,-1])

#& 
#& // PART ONE: TENURE MATTERS
#& // Does manager tenure have an effect on profit? On sales?
#& regress profit mtenure

# The following command use the base R lm() function to regress profit against mtenure, storing the results in variable "fit"

fit <- lm(profit ~ mtenure, data = PA_Store24A_data)

# simply listing "fit" will igve the intercept and slope of the fitted regression... 

fit

# Using the summary() function on fit provides descriptive and goodness fo fit statistics... 
summary(fit)


#& regress sales mtenure

# To regress sales on mtenure

fit <- lm(sales ~ mtenure, data = PA_Store24A_data)
summary(fit)


#& // What about crew tenure?
#& regress profit ctenure

# To regress profit on ctenure

fit <- lm(profit ~ ctenure, data = PA_Store24A_data)
summary(fit)

#& regress sales ctenure

# To regress sales on ctenure

fit <- lm(sales ~ ctenure, data = PA_Store24A_data)
summary(fit)

#& // What about both?
#& regress profit mtenure ctenure

# To regress profit on both mtenure and ctenure

fit <- lm(profit ~ mtenure + ctenure, data = PA_Store24A_data)
summary(fit)

#& regress sales mtenure ctenure

# To regress sales on both mtenure and ctenure

fit <- lm(sales ~ mtenure + ctenure, data = PA_Store24A_data)
summary(fit)

#& // What happens when we add control variables?
#& regress profit mtenure ctenure pop comp visibility pedcount res hours24

# To regress profit on all explanatory varibales from PA_Store24A_data

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24A_data)
summary(fit)

#& regress sales mtenure ctenure pop comp visibility pedcount res hours24

# To regress sales on all explanatory varibales from PA_Store24A_data

fit <- lm(sales ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24A_data)
summary(fit)

#& 
#& // Benefit of Experience - Creating Standardized Coefficients
#& // Re-run the profit regression
#& regress profit mtenure ctenure pop comp visibility ped res hours24
#& // Summarize the variable mtenure, then capture the standard deviation in a local (temporary) variable
#& // Note that "sum" is the same command as "summarize"
#& sum mtenure
#& local a=r(sd)
#& // Here, we create a standardized coefficient for mtenure 
#& // The command _b[VARIABLE] captures the coefficient of VARIABLE in the most recent regression
#& g sco_mtenure=`a'*_b[mtenure]
#& // We could create standardized coefficients for each of the remaining variables in the regression by retyping these 3 lines 7 more times
#& // Or, we could use a loop, which saves us time and reduced visual clutter in our .do file
#& // This loop takes each variable in the list and runs it through the three lines of code automatically
#& foreach var in ctenure pop comp visibility ped res hours24 {
#& sum `var'
#& local a=r(sd)
#& g sco_`var'=`a'*_b[`var']
#& }
#& // Here, we list the values of each our standardized coefficients
#& // A "*" is a wild card, so this command tells Stata to list all the variables that begin with "sco_"
#& list sco_* if _n==1

#  Let's store the model formula in a variable
  
modelformula <- profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24

# We can use the fact that all.vars gives us a vector of the variable names.

all.vars(modelformula)

# We can use this to subset the dataset accordingly. For instance, we can use the R scale() function to
# "center" and "scale" (i.e., subtract the mean, and divide by their standard deviations) the coefficients
# of the regressed model,

# Note: the lapply() function referenced below will simply apply the scale() function to each variable in the 
# model formula.

PA_Store24A_data_scaled <- lapply(PA_Store24A_data[, all.vars(modelformula)], scale) 
PA_Store24A_data

# Now we can fit the LM() function to the scaled data 

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24A_data_scaled)
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

fit <- lm(profit ~ ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24A_data_scaled)
summary(fit)
ir2_mtenure <- r2_full - summary(fit)$r.squared
ir2_mtenure

#& // We continue to drop variables from the regression in turn, and calculate each incremental R2
#& regress profit mtenure pop comp visibility ped res hours24
#& g ir2_ctenure=r2_full-e(r2_a)

# Re-running th emodel while dropping ctenure, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24A_data_scaled)
summary(fit)
ir2_ctenure <- r2_full - summary(fit)$r.squared
ir2_ctenure

#& regress profit mtenure ctenure comp visibility ped res hours24
#& g ir2_pop=r2_full-e(r2_a)

# Re-running th emodel while dropping pop, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + comp + visibility + pedcount + res + hours24, data = PA_Store24A_data_scaled)
summary(fit)
ir2_pop <- r2_full - summary(fit)$r.squared
ir2_pop

#& regress profit mtenure ctenure pop visibility ped res hours24
#& g ir2_comp=r2_full-e(r2_a)

# Re-running th emodel while dropping comp, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + visibility + pedcount + res + hours24, data = PA_Store24A_data_scaled)
summary(fit)
ir2_comp <- r2_full - summary(fit)$r.squared
ir2_comp

#& regress profit mtenure ctenure pop comp ped res hours24
#& g ir2_visibility=r2_full-e(r2_a)

# Re-running th emodel while dropping visibility, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24, data = PA_Store24A_data_scaled)
summary(fit)
ir2_visibility <- r2_full - summary(fit)$r.squared
ir2_visibility

#& regress profit mtenure ctenure pop comp visibility res hours24
#& g ir2_ped=r2_full-e(r2_a)


# Re-running th emodel while dropping pedcount, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + res + hours24, data = PA_Store24A_data_scaled)
summary(fit)
ir2_pedcount <- r2_full - summary(fit)$r.squared
ir2_pedcount

#& regress profit mtenure ctenure pop comp visibility ped hours24
#& g ir2_res=r2_full-e(r2_a)


# Re-running th emodel while dropping res, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + hours24, data = PA_Store24A_data_scaled)
summary(fit)
ir2_res <- r2_full - summary(fit)$r.squared
ir2_res

#& regress profit mtenure ctenure pop comp visibility ped res
#& g ir2_hours24=r2_full-e(r2_a)

# Re-running th emodel while dropping hours24, then calculating the incremental difference in r-squared between that
# model and the full model

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res, data = PA_Store24A_data_scaled)
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
# g MT2=mtenure*mtenure

# PART THREE: DIMINISHING RETURNS
# Create a new variable called MT2 that squares the manager tenure term

PA_Store24A_data$MT2 <- PA_Store24A_data$mtenure^2
label(PA_Store24A_data$MT2) <- "Squared Manager Tenure"
contents(PA_Store24A_data)
head(PA_Store24A_data,n=5L)

#& regress profit mtenure ctenure pop comp visibility ped res hours24 MT2

# Running the regression while adding the MT2 variable as an explanantory variable:

fit <- lm(profit ~ mtenure + ctenure + pop + comp + visibility + pedcount + res + hours24 + MT2, data = PA_Store24A_data)
summary(fit)


#& // because we created some new variables, we want to save this dataset in case we want to use it later
#& save "data/store24-case-data-new-variables.dta", replace

# The following command saves this new version of the data frame as a new RData file

save(PA_Store24A_data, file = "data/PA_Store24A_data_new_variables.RData")

#& // closes your log
#& log close
#& 
#& // drops all data from Stata's memory
#& clear

# This clears R's data memory so new data can be loaded

rm(list = ls())

# The following command redirects output from the log file back to the console, if necessary.

sink()


    

