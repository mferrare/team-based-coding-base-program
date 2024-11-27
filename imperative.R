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
source(paste(base_path,"/generateData.r", sep=""))
source(paste(base_path,"/writeData.r", sep=""))
source(paste(base_path,"/generateAnalysis.R", sep=""))

####### Generate data ########
generateData()

######## Regression tests ########
generateAnalysis()

######## Summary tables ##########

# Regression tables need to include all possible vars in data
# Can check this by getting the model matrices and building a unique set of vars

varnames_base = colnames(model.matrix(reg_base))
varnames_time = colnames(model.matrix(reg_time))
varnames_fe = colnames(model.matrix(reg_fe))
varnames_time_fe = colnames(model.matrix(reg_time_fe))

varnames = unique(c(
    varnames_base,
    varnames_time,
    varnames_fe,
    varnames_time_fe
))

#specify empty space and R-squared, N, K, DF, AIC and BIC
model_params = c(
    "",
    "# observations",
    "# parameters",
    "degrees of freedom",
    "AIC",
    "BIC"
)

#join into tablenames
table_names = c(
    varnames,
    model_params
)

#regression table (base)
outmat_raw = matrix(NA,nrow=length(table_names), ncol=4)
rownames(outmat_raw) = table_names
colnames(outmat_raw) = c(
    "Base regression",
    "Including time effects",
    "Including Fixed effects",
    "Including both time and FE"
)

coefmat_base = summary(reg_base)$coef
for(var in 1:length(varnames_base)){
    coef_val = round(coefmat_base[,1][which(rownames(coefmat_base) %in% varnames_base[var])],3)
    pval_val = coefmat_base[,4][which(rownames(coefmat_base) %in% varnames_base[var])]
    stars = ifelse(pval_val>0.1,"",ifelse(pval_val>0.05,"*", ifelse(pval_val>0.01,"**", "***")))
    outmat_raw[which(rownames(outmat_raw) %in% varnames_base[var]),1] = paste(coef_val, stars, sep="")

    N = nrow(model.matrix(reg_base))
    K = ncol(model.matrix(reg_base)) + 1
    DF = N - K
    aic = aic_reg_base_raw
    bic = bic_reg_base_raw
    
    outmat_raw[which(rownames(outmat_raw) %in% "# observations"),1] = N
    outmat_raw[which(rownames(outmat_raw) %in% "# parameters"),1] = K
    outmat_raw[which(rownames(outmat_raw) %in% "degrees of freedom"),1] = DF
    outmat_raw[which(rownames(outmat_raw) %in% "AIC"),1] = round(aic,0)
    outmat_raw[which(rownames(outmat_raw) %in% "BIC"),1] = round(bic,0)
}

coefmat_time = summary(reg_time)$coef
for(var in 1:length(varnames_time)){
    coef_val = round(coefmat_time[,1][which(rownames(coefmat_time) %in% varnames_time[var])],3)
    pval_val = coefmat_time[,4][which(rownames(coefmat_time) %in% varnames_time[var])]
    stars = ifelse(pval_val>0.1,"",ifelse(pval_val>0.05,"*", ifelse(pval_val>0.01,"**", "***")))
    outmat_raw[which(rownames(outmat_raw) %in% varnames_time[var]),2] = paste(coef_val, stars, sep="")

    N = nrow(model.matrix(reg_time))
    K = ncol(model.matrix(reg_time)) + 1
    DF = N - K
    aic = aic_reg_time_raw
    bic = bic_reg_time_raw

    outmat_raw[which(rownames(outmat_raw) %in% "# observations"),2] = N
    outmat_raw[which(rownames(outmat_raw) %in% "# parameters"),2] = K
    outmat_raw[which(rownames(outmat_raw) %in% "degrees of freedom"),2] = DF
    outmat_raw[which(rownames(outmat_raw) %in% "AIC"),2] = round(aic,0)
    outmat_raw[which(rownames(outmat_raw) %in% "BIC"),2] = round(bic,0)
}

coefmat_fe = summary(reg_fe)$coef
for(var in 1:length(varnames_fe)){
    coef_val = round(coefmat_fe[,1][which(rownames(coefmat_fe) %in% varnames_fe[var])],3)
    pval_val = coefmat_fe[,4][which(rownames(coefmat_fe) %in% varnames_fe[var])]
    stars = ifelse(pval_val>0.1,"",ifelse(pval_val>0.05,"*", ifelse(pval_val>0.01,"**", "***")))
    outmat_raw[which(rownames(outmat_raw) %in% varnames_fe[var]),3] = paste(coef_val, stars, sep="")

    N = nrow(model.matrix(reg_fe))
    K = ncol(model.matrix(reg_fe)) + 1
    DF = N - K
    aic = aic_reg_fe_raw
    bic = bic_reg_fe_raw

    outmat_raw[which(rownames(outmat_raw) %in% "# observations"),3] = N
    outmat_raw[which(rownames(outmat_raw) %in% "# parameters"),3] = K
    outmat_raw[which(rownames(outmat_raw) %in% "degrees of freedom"),3] = DF
    outmat_raw[which(rownames(outmat_raw) %in% "AIC"),3] = round(aic,0)
    outmat_raw[which(rownames(outmat_raw) %in% "BIC"),3] = round(bic,0)
}

coefmat_time_fe = summary(reg_time_fe)$coef
for(var in 1:length(varnames_time_fe)){
    coef_val = round(coefmat_time_fe[,1][which(rownames(coefmat_time_fe) %in% varnames_time_fe[var])],3)
    pval_val = coefmat_time_fe[,4][which(rownames(coefmat_time_fe) %in% varnames_time_fe[var])]
    stars = ifelse(pval_val>0.1,"",ifelse(pval_val>0.05,"*", ifelse(pval_val>0.01,"**", "***")))
    outmat_raw[which(rownames(outmat_raw) %in% varnames_time_fe[var]),4] = paste(coef_val, stars, sep="")

    N = nrow(model.matrix(reg_time_fe))
    K = ncol(model.matrix(reg_time_fe)) + 1
    DF = N - K
    aic = aic_reg_time_fe_raw
    bic = bic_reg_time_fe_raw

    outmat_raw[which(rownames(outmat_raw) %in% "# observations"),4] = N
    outmat_raw[which(rownames(outmat_raw) %in% "# parameters"),4] = K
    outmat_raw[which(rownames(outmat_raw) %in% "degrees of freedom"),4] = DF
    outmat_raw[which(rownames(outmat_raw) %in% "AIC"),4] = round(aic,0)
    outmat_raw[which(rownames(outmat_raw) %in% "BIC"),4] = round(bic,0)
}

#now do the same for the cleaned data regressions

varnames_base = colnames(model.matrix(reg_base_clean))
varnames_time = colnames(model.matrix(reg_time_clean))
varnames_fe = colnames(model.matrix(reg_fe_clean))
varnames_time_fe = colnames(model.matrix(reg_time_fe_clean))

varnames = unique(c(
    varnames_base,
    varnames_time,
    varnames_fe,
    varnames_time_fe
))

#join into tablenames
table_names = c(
    varnames,
    model_params
)

#regression table (base)
outmat_clean = matrix(NA,nrow=length(table_names), ncol=4)
rownames(outmat_clean) = table_names
colnames(outmat_clean) = c(
    "Base regression",
    "Including time effects",
    "Including Fixed effects",
    "Including both time and FE"
)

coefmat_base = summary(reg_base_clean)$coef
for(var in 1:length(varnames_base)){
    coef_val = round(coefmat_base[,1][which(rownames(coefmat_base) %in% varnames_base[var])],3)
    pval_val = coefmat_base[,4][which(rownames(coefmat_base) %in% varnames_base[var])]
    stars = ifelse(pval_val>0.1,"",ifelse(pval_val>0.05,"*", ifelse(pval_val>0.01,"**", "***")))
    outmat_clean[which(rownames(outmat_clean) %in% varnames_base[var]),1] = paste(coef_val, stars, sep="")

    N = nrow(model.matrix(reg_base_clean))
    K = ncol(model.matrix(reg_base_clean)) + 1
    DF = N - K
    aic = aic_reg_base_clean
    bic = bic_reg_base_clean
    
    outmat_clean[which(rownames(outmat_clean) %in% "# observations"),1] = N
    outmat_clean[which(rownames(outmat_clean) %in% "# parameters"),1] = K
    outmat_clean[which(rownames(outmat_clean) %in% "degrees of freedom"),1] = DF
    outmat_clean[which(rownames(outmat_clean) %in% "AIC"),1] = round(aic,0)
    outmat_clean[which(rownames(outmat_clean) %in% "BIC"),1] = round(bic,0)
}

coefmat_time = summary(reg_time_clean)$coef
for(var in 1:length(varnames_time)){
    coef_val = round(coefmat_time[,1][which(rownames(coefmat_time) %in% varnames_time[var])],3)
    pval_val = coefmat_time[,4][which(rownames(coefmat_time) %in% varnames_time[var])]
    stars = ifelse(pval_val>0.1,"",ifelse(pval_val>0.05,"*", ifelse(pval_val>0.01,"**", "***")))
    outmat_clean[which(rownames(outmat_clean) %in% varnames_time[var]),2] = paste(coef_val, stars, sep="")

    N = nrow(model.matrix(reg_time_clean))
    K = ncol(model.matrix(reg_time_clean)) + 1
    DF = N - K
    aic = aic_reg_time_clean
    bic = bic_reg_time_clean

    outmat_clean[which(rownames(outmat_clean) %in% "# observations"),2] = N
    outmat_clean[which(rownames(outmat_clean) %in% "# parameters"),2] = K
    outmat_clean[which(rownames(outmat_clean) %in% "degrees of freedom"),2] = DF
    outmat_clean[which(rownames(outmat_clean) %in% "AIC"),2] = round(aic,0)
    outmat_clean[which(rownames(outmat_clean) %in% "BIC"),2] = round(bic,0)
}

coefmat_fe = summary(reg_fe_clean)$coef
for(var in 1:length(varnames_fe)){
    coef_val = round(coefmat_fe[,1][which(rownames(coefmat_fe) %in% varnames_fe[var])],3)
    pval_val = coefmat_fe[,4][which(rownames(coefmat_fe) %in% varnames_fe[var])]
    stars = ifelse(pval_val>0.1,"",ifelse(pval_val>0.05,"*", ifelse(pval_val>0.01,"**", "***")))
    outmat_clean[which(rownames(outmat_clean) %in% varnames_fe[var]),3] = paste(coef_val, stars, sep="")

    N = nrow(model.matrix(reg_fe_clean))
    K = ncol(model.matrix(reg_fe_clean)) + 1
    DF = N - K
    aic = aic_reg_fe_clean
    bic = bic_reg_fe_clean

    outmat_clean[which(rownames(outmat_clean) %in% "# observations"),3] = N
    outmat_clean[which(rownames(outmat_clean) %in% "# parameters"),3] = K
    outmat_clean[which(rownames(outmat_clean) %in% "degrees of freedom"),3] = DF
    outmat_clean[which(rownames(outmat_clean) %in% "AIC"),3] = round(aic,0)
    outmat_clean[which(rownames(outmat_clean) %in% "BIC"),3] = round(bic,0)
}

coefmat_time_fe = summary(reg_time_fe_clean)$coef
for(var in 1:length(varnames_time_fe)){
    coef_val = round(coefmat_time_fe[,1][which(rownames(coefmat_time_fe) %in% varnames_time_fe[var])],3)
    pval_val = coefmat_time_fe[,4][which(rownames(coefmat_time_fe) %in% varnames_time_fe[var])]
    stars = ifelse(pval_val>0.1,"",ifelse(pval_val>0.05,"*", ifelse(pval_val>0.01,"**", "***")))
    outmat_clean[which(rownames(outmat_clean) %in% varnames_time_fe[var]),4] = paste(coef_val, stars, sep="")

    N = nrow(model.matrix(reg_time_fe_clean))
    K = ncol(model.matrix(reg_time_fe_clean)) + 1
    DF = N - K
    aic = aic_reg_time_fe_clean
    bic = bic_reg_time_fe_clean

    outmat_clean[which(rownames(outmat_clean) %in% "# observations"),4] = N
    outmat_clean[which(rownames(outmat_clean) %in% "# parameters"),4] = K
    outmat_clean[which(rownames(outmat_clean) %in% "degrees of freedom"),4] = DF
    outmat_clean[which(rownames(outmat_clean) %in% "AIC"),4] = round(aic,0)
    outmat_clean[which(rownames(outmat_clean) %in% "BIC"),4] = round(bic,0)
}

#tests table
#an upper triangular matrix of p-values for ANOVA tests

tests_mat_raw = tests_mat_clean = matrix(
    NA,
    nrow = 2,
    ncol = 3
)
colnames(tests_mat_clean) = colnames(tests_mat_raw) = c(
    "Time effects",
    "Fixed effects (region)",
    "Time and FE"
)
rownames(tests_mat_clean) = rownames(tests_mat_raw) = c(
    "Base (no time or fixed effects)",
    "All time and fixed effects"
)

val = anova_base__time_raw[5][[1]][2] 
pval = anova_base__time_raw[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="")     
entry_base__time = entry

val = anova_base__fe_raw[5][[1]][2]
pval = anova_base__fe_raw[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="") 
entry_base__fe = entry

val = anova_base__time_fe_raw[5][[1]][2]
pval = anova_base__time_fe_raw[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="")     
entry_base__time_fe = entry

val = anova_time__time_fe_raw[5][[1]][2] 
pval = anova_time__time_fe_raw[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="")     
entry_time__time_fe = entry

val = anova_fe__time_fe_raw[5][[1]][2] 
pval = anova_fe__time_fe_raw[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="")     
entry_fe__time_fe = entry

tests_mat_raw[1, 1:3] = c(
    entry_base__time,
    entry_base__fe,
    entry_base__time_fe
)

tests_mat_raw[2, 1:2] = c(
    entry_time__time_fe,
    entry_fe__time_fe
)

# do for clean data too

val = anova_base__time_clean[5][[1]][2] 
pval = anova_base__time_clean[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="")     
entry_base__time = entry

val = anova_base__fe_clean[5][[1]][2]
pval = anova_base__fe_clean[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="") 
entry_base__fe = entry

val = anova_base__time_fe_clean[5][[1]][2]
pval = anova_base__time_fe_clean[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="")     
entry_base__time_fe = entry

val = anova_time__time_fe_clean[5][[1]][2] 
pval = anova_time__time_fe_clean[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="")     
entry_time__time_fe = entry

val = anova_fe__time_fe_clean[5][[1]][2] 
pval = anova_fe__time_fe_clean[6][[1]][2]
entry = paste(round(val,2), ifelse(pval>0.1,"",ifelse(pval>0.05,"*", ifelse(pval>0.01,"**", "***"))),sep="")     
entry_fe__time_fe = entry

tests_mat_clean[1, 1:3] = c(
    entry_base__time,
    entry_base__fe,
    entry_base__time_fe
)

tests_mat_clean[2, 1:2] = c(
    entry_time__time_fe,
    entry_fe__time_fe
)

######## Write results to file ##########

#check directory is present first
if(!dir.exists(here("results"))){
    dir.create(here("results"))
}

file_path = here("results", "regression_table_raw.csv")
if(file.exists(file_path)){
    file.remove(file_path)
} 
write.csv(outmat_raw,file_path, row.names=TRUE)

file_path = here("results", "regression_table_clean.csv")
if(file.exists(file_path)){
    file.remove(file_path)
} 
write.csv(outmat_clean,file_path, row.names=TRUE)

file_path = here("results", "anova_table_raw.csv")
if(file.exists(file_path)){
    file.remove(file_path)
} 
write.csv(tests_mat_raw,file_path, row.names=TRUE)

file_path = here("results", "anova_table_clean.csv")
if(file.exists(file_path)){
    file.remove(file_path)
} 
write.csv(tests_mat_clean,file_path, row.names=TRUE)
