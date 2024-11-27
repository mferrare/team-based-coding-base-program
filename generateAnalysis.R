
generateAnalysis = function() {
    
    # read in data
    data = read.csv(here("data", "data.csv"))

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
    
    # Anova tests (nested models)
    anova_base__time_raw = anova(reg_base, reg_time)
    anova_base__fe_raw = anova(reg_base, reg_fe)
    anova_base__time_fe_raw = anova(reg_base, reg_time_fe)
    anova_fe__time_fe_raw = anova(reg_fe, reg_time_fe)
    anova_time__time_fe_raw = anova(reg_time, reg_time_fe)

    anova_base__time_clean = anova(reg_base_clean, reg_time_clean)
    anova_base__fe_clean = anova(reg_base_clean, reg_fe_clean)
    anova_base__time_fe_clean = anova(reg_base_clean, reg_time_fe_clean)
    anova_fe__time_fe_clean = anova(reg_fe_clean, reg_time_fe_clean)
    anova_time__time_fe_clean = anova(reg_time_clean, reg_time_fe_clean)

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

}