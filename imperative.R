## An imperative example script for traditional linear statistical coding analysis

## Steps:
# Generate fake data
#   Create IDs
#   Create groups
#   Create regressor values
#   Create Y from regressor values
#   Generate Yhat by adding errors
#   Add additional errors for 'cleaning'
#   Collate into dataframe
#   Save to data sub directory
# Regressions
#   Load data
#   Specify and estimate core regressions
#   Check and observe issues
#   Clean data
#   New regressions with clean data
# Regression specification and clean data tests
#   ANOVA for nested models
#   AIC for raw vs clean data models
# Generate summary tables
# Save tables/results to file

# load required libraries
#require(here)

## instead set up local paths
base_path = getwd()

# source local utilities
source(paste(base_path,"/createPath.r", sep=""))
source(createPath("/generateData.r"))
source(createPath("/writeData.r"))
source(createPath("/generateAnalysis.R"))
source(createPath("/makeSummaryTables.r"))

####### Generate data ########
generateData()

######## Regression tests ########
generateAnalysis()

makeSummaryTables()

######## Summary tables ##########
