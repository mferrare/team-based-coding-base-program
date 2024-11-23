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
require(here)

########## Generate fake dataset ###############

#Start by creating an ID vector based on 'regions' (group)
group_vals = c("A", "B", "C", "D", "E") #alphanumeric component of id - represents a group
obs_group = c(13, 2, 18, 22, 13) #number of individuals in each group (letter)
years = 5 # number of years each individual observed for
ints = seq(1,100,1)

#create ID vector
IDs = c()
for (lll in 1:length(group_vals)){
    for(iii in 1:obs_group[lll]){
        ints_group = sample(ints,obs_group[lll])
        for(yyy in 1:years){
            id = paste(group_vals[lll], ints_group[iii], sep="")
            IDs = append(IDs, id)
        }
    }
}

#create groups associated with ID for use in a regions variable
groups = c(
    rep(group_vals[1], obs_group[1]*years),
    rep(group_vals[2], obs_group[2]*years),
    rep(group_vals[3], obs_group[3]*years),
    rep(group_vals[4], obs_group[4]*years),
    rep(group_vals[5], obs_group[5]*years)
)

# Generate values for regressors 
X1 = runif(length(IDs),0,1)*100
X2 = rnorm(length(IDs),10,2)
X3 = rbinom(length(IDs),1, 0.4)
FE = c(
        rep(1, obs_group[1]*years),
        rep(3, obs_group[2]*years),
        rep(5, obs_group[3]*years),
        rep(7, obs_group[4]*years),
        rep(10, obs_group[5]*years)
    )
year_effect = 2

# Generate Y and Yhat
Y = 0.1 * X1 + 
    0.8 * X2 + 
    0.2 * X3 + 
    FE + 
    2 * rep(seq(1,years),length(IDs)/5)

Yhat = Y + rnorm(length(IDs), 0, mean(Y)*0.15)

# Add additional errors to group B
Yhat[which(groups=="B")] = Yhat[which(groups=="B")] + rnorm(obs_letters[which(group_vals=="B")], 0, 100)


# build data frame and re-name variables to intuitive names
data = data.frame(
    "ID" = IDs,
    "YEAR" = rep(seq(1,years), length(IDs)/5),
    "OUTPUT_TONNES" = Yhat,
    "REGION" = as.factor(groups),
    "RAINFALL_PERCENTILE" = X1,
    "LABOUR_DAYS" = X2,
    "IRRIGATION_BOOLEAN" = X3
)

write.csv(data, here("data", "data.csv"))
####### Regressions ##########

# Base regressions
reg_base = lm(OUTPUT_TONNES ~ RAINFALL_PERCENTILE + LABOUR_DAYS + IRRIGATION_BOOLEAN, data=data)
reg_time = lm(OUTPUT_TONNES ~ YEAR + RAINFALL_PERCENTILE + LABOUR_DAYS + IRRIGATION_BOOLEAN, data=data)
reg_fe = lm(OUTPUT_TONNES ~ REGION + RAINFALL_PERCENTILE + LABOUR_DAYS + IRRIGATION_BOOLEAN, data=data)
reg_time_fe = lm(OUTPUT_TONNES ~ YEAR + REGION + RAINFALL_PERCENTILE + LABOUR_DAYS + IRRIGATION_BOOLEAN, data=data)

#remove region B as outlier
data_clean = data[-which(data$REGION=="B"),]

# Clean regressions
reg_base_clean = lm(OUTPUT_TONNES ~ RAINFALL_PERCENTILE + LABOUR_DAYS + IRRIGATION_BOOLEAN, data=data_clean)
reg_time_clean = lm(OUTPUT_TONNES ~ YEAR + RAINFALL_PERCENTILE + LABOUR_DAYS + IRRIGATION_BOOLEAN, data=data_clean)
reg_fe_clean = lm(OUTPUT_TONNES ~ REGION + RAINFALL_PERCENTILE + LABOUR_DAYS + IRRIGATION_BOOLEAN, data=data_clean)
reg_time_fe_clean = lm(OUTPUT_TONNES ~ YEAR + REGION + RAINFALL_PERCENTILE + LABOUR_DAYS + IRRIGATION_BOOLEAN, data=data_clean)

######## Regression tests ########

# Anova tests (nested models)
base__time_raw = anova(reg_base, reg_time)
base__fe_raw = anova(reg_base, reg_fe)
base__time_fe_raw = anova(reg_base, reg_time_fe)
fe__time_fe_raw = anova(reg_fe, reg_time_fe)
time__time_fe_raw = anova(reg_time, reg_time_fe)

base__time_clean = anova(reg_base_clean, reg_time_clean)
base__fe_clean = anova(reg_base_clean, reg_fe_clean)
base__time_fe_clean = anova(reg_base_clean, reg_time_fe_clean)
fe__time_fe_raw_clean = anova(reg_fe_clean, reg_time_fe_clean)
time__time_fe_raw_clean = anova(reg_time_clean, reg_time_fe_clean)

#AIC and BIC values for cleaned versus raw
aic_reg_base_raw = AIC(reg_base)
aic_reg_time_raw = AIC(reg_time)
aic_reg_fe_raw = AIC(reg_fe)
aic_reg_time_fe_raw = AIC(reg_time_fe)

aic_reg_base_clean = AIC(reg_base_clean)
aic_reg_time_clean = AIC(reg_time_clean)
aic_reg_fe_clean = AIC(reg_fe_clean)
aic_reg_time_fe_clean = AIC(reg_time_fe_clean)

bic_reg_base_raw = BIC(reg_base)
bic_reg_time_raw = BIC(reg_time)
bic_reg_fe_raw = BIC(reg_fe)
bic_reg_time_fe_raw = BIC(reg_time_fe)

bic_reg_base_clean = BIC(reg_base_clean)
bic_reg_time_clean = BIC(reg_time_clean)
bic_reg_fe_clean = BIC(reg_fe_clean)
bic_reg_time_fe_clean = BIC(reg_time_fe_clean)

######## Summary tables ##########

stars = function(pval){
    return (ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))))
}



######## Write results to file ##########

write.csv