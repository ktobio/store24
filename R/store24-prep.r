# **********************************************************************************
#   File        : store24-prep.r  <--- this should be the exact name of THIS document
#   Author      : Troy Adair 
#   Created     : 15 Nov 2016
#   Modified    : 07 Jul 2017
#   Description : .r file for preparing data for Store24 Case; will output data files store24_case_data.RData, 
#                 PA_Store24A_data.RData, and PA_Store24B_data.RData
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

sink(file = "./logs/store24-case-prepare-data", append = FALSE, type = c("output", "message"), split = TRUE)

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


#& data should be placed in a folder named "data" in your directory 
#& this pulls in the data in its exact Excel form from HBP
#& this command reads the data into Stata
#& import excel using "data/Pilgrim_ABC_Spreadsheet.xls", sheet(Data) firstrow

# To facilitate renaming and labeling of variables, we will use two packages in R:
# "dplyr" and "Hmisc". The following commands download & install, then load these two respective packages.

# Note 1: If you have not already previously downlaoded and installed these two packages, please
# uncomment the two "install" commands below.

# Note 2: If you have previosuly downloaded and installed these packages, but the "install" commands below are
# uncommented, you will be prompted that the package already exists, and asked if you want to re-start R. There
# is no need to restart R, if so prompted, and you may ignore any warnings about the packages not being installed
# due to being in use if it is # already installed.

## install.packages("dplyr")
library(dplyr)

## install.packages('Hmisc')
library('Hmisc')

# The following command assumes that your data file is stored in a folder named "data" 
# off your main (i.e., project) directory, and will read the csv file from HBP into an R data frame
# named "store24.case.data.


store24.case.data <- read.csv("data/store24-case-data.csv")



# The following command lists the contents (including name, type and numbers of variables, as well as
# number of observations, of the store.24.case.data data frame.

contents(store24.case.data)

# The following command lists the first 5 rows of the store.24.case.data data frame.

head(store24.case.data,n=5L)

#& label var store "Store number"
#& label var sales "FY 2000 sales"
#& label var profit "FY 2000 profit before corp. overhead alloc, rent, deprec"
#& label var mtenure "Avg manager tenure FY 2000"
#& label var ctenure "Avg crew tenure FY 2000"
#& label var pop "Pop w/in 1/2 mile radius"
#& label var comp "# Competitors w/in 1/2 mile radius"
#& label var visibility "5 pt rating visibility storefront, 5=highest"
#& label var pedcount "5 pt rating foot traffic, 5=highest"
#& label var res "Indicator for location in residental/urban area, 1=res"
#& label var hours24 "Indicator for open 24 hrs, 1=open"

# The following lines use the "label" function from the Hmisc package to attach more descriptive labels
# to each of the variable sin the data set.

label(store24.case.data$store) <- "Store number"
label(store24.case.data$Sales) <- "FY 2000 sales"
label(store24.case.data$Profit) <- "FY 2000 profit before corp. overhead alloc, rent, deprec"
label(store24.case.data$MTenure) <- "Avg manager tenure FY 2000"
label(store24.case.data$CTenure) <- "Avg crew tenure FY 2000"
label(store24.case.data$Pop) <- "Pop w/in 1/2 mile radius"
label(store24.case.data$Comp) <- "# Competitors w/in 1/2 mile radius"
label(store24.case.data$Visibility) <- "5 pt rating visibility storefront, 5=highest"
label(store24.case.data$PedCount) <- "5 pt rating foot traffic, 5=highest"
label(store24.case.data$Res) <- "Indicator for location in residental/urban area, 1=res"
label(store24.case.data$Hours24) <- "Indicator for open 24 hrs, 1=open"
label(store24.case.data$CrewSkill) <- "Avg crew skill rating FY 2000"
label(store24.case.data$MgrSkill) <- "Avg mgr skill rating FY 2000"
label(store24.case.data$ServQual) <- "Avg service quality rating FY 2000"

# The following command lists the lables stored in the store.24.case.data data frame.

label(store24.case.data)

# The following command saves the store.24.case.data data frame in an RData file in the "data" folder for future reuse.

save(store24.case.data, file = "data/store24_case_data.RData")

#& keep store sales profit mtenure ctenure pop comp visibility pedcount res hours24

# The following command uses the dplyr() "select" function to select a subset of variables in the store24_case_data
# data frame, dynamically naming some of them "on the fly"  to all lower case (note: R is case-sensitive) for 
# ease of future typing, then saving the results in a new data fram, PA_Store24A_data.

PA_Store24A_data <- select(store24.case.data, store, "sales" = Sales, "profit" = Profit, "mtenure" = MTenure,
                          "ctenure" = CTenure, "pop" = Pop, "comp" = Comp, "visibility" = Visibility,
                          "pedcount" = PedCount, "res" = Res, "hours24" = Hours24)


# The following command lists the contents of PA_Store24A_data.

contents(PA_Store24A_data)

# The following command lists the first 5 rows of the PA_Store24A_data.

head(PA_Store24A_data,n=5L)

# The following command saves the PA_Store24A_data data frame in an RData file in the "data" folder for future reuse.

save(PA_Store24A_data, file = "data/PA_Store24A_data.RData")

# The following command uses the dplyr() "select" function to select a different subset of variables in the store24_case_data
# data frame, dynamically naming some of them "on the fly"  to all lower case (note: R is case-sensitive) for 
# ease of future typing, then saving the results in a new data fram, PA_Store24B_data.

PA_Store24B_data = select(store24.case.data, store, "sales" = Sales, "profit" = Profit, "mtenure" = MTenure,
                          "ctenure" = CTenure, "pop" = Pop, "comp" = Comp, "visibility" = Visibility,
                          "pedcount" = PedCount, "res" = Res, "hours24" = Hours24, "servqual" = ServQual,
                          "mgrskill" = MgrSkill, "crewskill" = CrewSkill)


# The following command lists the contents of PA_Store24B_data.

contents(PA_Store24B_data)

# The following command lists the first 5 rows of the PA_Store24B_data.

head(PA_Store24B_data,n=5L)


# The following command saves the PA_Store24B_data data frame in an RData file in the "data" folder for future reuse.

save(PA_Store24B_data, file = "data/PA_Store24B_data.RData")

# This clears R's data memory so new data can be loaded

rm(list = ls())

# The following command redirects output from the log file back to the console, if necessary.

sink()
