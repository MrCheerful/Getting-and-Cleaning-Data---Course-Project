##
##  Course Project for John Hopkins - Getting and Cleaning Data
##  by Ken Melax  (aka Mr Cheerful on GitHub)
##  14 October 2016  
##
##  -- FILE LOCATIONS --
##  This script assumes that the session working directory
##  is "../UCI HAR Dataset" where the datasets are in sub directories
##  where the naming is as follows
##    <setname>/subject_<setname>  - contains vector of subject of each observation
##    <setname>/X_<setname>     - contains table with variables in columns and rows of individual observations
##    <setname>/y_<setname>     - contains vector of the activity corresponding to each observation
##
##  Also required for this script are the following files in the working directory
##    activity_labels.txt  - contains table of activity index to activity description
##    features.txt   - contains vector listing the discriptions of each variable/column
##
##  -- Script --
##
##  Load any libraries necessary
library(plyr)
library(dplyr)
library(reshape2)

## Verify existance of datasets and required files
## This step is not required, but will make it clear if there
## is a problem with the script cannot find the necessary files

## Specify Datasets as vector for merging
#datasets <- c("dev1","dev2")    # Test datasets for code development
datasets <- c("test","train")   # Production datasets for report

## Specify column headings file
#featurefile <- "features_dev.txt"    # Test features file for development
featurefile <- "features.txt"        # Production features file

## Specify data storage file naming convention as list for test
fileforms <- c("subject_", "X_", "y_")

## Set up indicator for stopping for file error.
fileerror <- FALSE

## Test for existance of the files containing the metadata
if(!file.exists("activity_labels.txt")){
      print("activity_labels.txt not found")
      fileerror <- TRUE
      }
if(!file.exists(featurefile)){
      print("features.txt not found")
      fileerror <- TRUE
      }

## Set up function for checking that all necessary files exist in a data folder
## Function loops through vector, testing that each of the files exists for a dataset.
datafilestest <- function(setname, ff){
      for (xx in ff){
            if(!file.exists(paste(setname, "/", xx, setname,".txt", sep=""))){
                  message(setname, "/", xx, setname, ".txt not found")
                  fileerror <- TRUE
            }
      }
      return
}      

## Loop through the datasets, check that the folder exists, and if so check the files.
for (ds in datasets){
      if(!file.exists(ds)){
            message("folder ",ds," not found")
            fileerror <- TRUE
      }
      else {datafilestest(ds, fileforms)     }
}

## If the files are not correctly located, raise an alert
if (fileerror == TRUE){ print(
      "Warning - issues exist regarding file locations.  See notes to correct."
      )}

## Load in the activity metadata, assign column names and set as factors.
activityMeta <- read.table("activity_labels.txt",header = FALSE, as.is = TRUE)
names(activityMeta) <- c("id","activity")
activityMeta$activity <- factor(activityMeta$activity,levels = activityMeta$activity)

## Load in the variable (aka feature) names list, assign column names
variablesMeta <- read.table(featurefile,header = FALSE, as.is = TRUE)
names(variablesMeta) <- c("id","variable") 

## -- PROJECT REQUIREMENT #2 --
## Determine the variables / measures for extraction and reporting
## The project assignment indicates to extract "ONLY the measurements 
## on the mean and standard deviation for each measurement."
## Hence only mean() and std() for each measurement vector is required
## (The assignment does not ask for the meanFreq, gravityMean, etc. )

## A column is added to the metadata file which contains a boolean 
## to indicate whether the variable in the row is to be kept or not.
variablesMeta$extract <- grepl("mean\\(\\)|std\\(\\)",variablesMeta$variable)

## A vector is created listing only the kept variables, to be used when melting.
varsExtracted <- variablesMeta[variablesMeta$extract==TRUE, "variable" ]

## -- PROJECT REQUIREMENT #1 --
## Load and merge the datasets

## Load datasets - note on this implementation.
## the dataset names are loaded by looping over the files and folders
## and assigning each load into a object whose name is constructed 
## using an paste0() statement inside an assign() function. 
for (ff in fileforms){
      for (ds in datasets){
            assign(paste0(ff,ds),read.table(paste0(ds,"/",ff,ds,".txt"),
                                            header = FALSE, as.is = TRUE))
      }
}

## Merge observation datasets.  
## (I couldn't determine how to get rbind to accept a construction of 
## the dataset names, hence the merges are explicitly defined)
#x_c <- rbind(x_dev1, x_dev2)      # row bind / combine the development measurement data
#y_c <- rbind(y_dev1, y_dev2)      # row bind / combine the development activity data
#subject_c <- rbind(subject_dev1, subject_dev2)  # combine the development subject data

x_c <- rbind(X_test, X_train)
y_c <- rbind(y_test, y_train)
subject_c <- rbind(subject_test, subject_train)

## -- PROJECT REQUIREMENT #4 --
## Note: I chose keep the variable names identical to the raw data names.
## Given the complexity of the names, introducing shortened versions
## of the names would, in my opinion, create confusion for tracing the data
## from source to summary.
##
## Apply the variable names as column labels to the combined measurement data set.
names(x_c) <- variablesMeta$variable

## Remove the unnecessary measurement columns
for (x in length(variablesMeta$variable):1){
      if (variablesMeta$extract[x] == FALSE) {
            x_c[,x]<- NULL
      }
}

## Combine columns for subject and activity with the measurement data
## Dataset "datac" is the compiled dataset in "wide" form
datac <- cbind(subject_c$V1, y_c$V1, x_c)

## Fix the column names for newly added columns
names(datac)[names(datac)=="y_c$V1"] = "activity"
names(datac)[names(datac)=="subject_c$V1"] = "subject"

## -- PROJECT REQUIREMENT #3 --
## Set up the activity column as factor with descriptive labels applied
datac$activity <- factor(datac$activity, labels = activityMeta$activity)

## -- PROJECT REQUIREMENT #5 --
## Convert to long form data
datalong <- melt(datac, c("subject","activity"), varsExtracted, 
                 variable.name = "variable", value.name = "value" )

## Make a summary dataset with averages by variable & activity & subject
## Note: the summary dataset is sorted by subject, then activity and variable
datasummary <- ddply( datalong, .(subject, activity, variable), 
                      summarize, avg = mean(value) )

## Save the tidy, long dataset.  (First sorted for convenience of user.)
#NOT REQUIRED FOR ASSIGNMENT, HENCE THE FOLLOWING COMMANDS COMMENTED OUT
#datalongsorted <- arrange(datalong, subject, activity, variable)
#write.table(datalongsorted, file = "data_long.txt", row.names = FALSE)

## Save the summary dataset.
write.table(datasummary, file = "data_summary.txt", row.names = FALSE)
