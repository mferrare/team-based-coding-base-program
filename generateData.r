


generateData = function() {
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
    Yhat[which(groups=="B")] = Yhat[which(groups=="B")] + rnorm(obs_group[which(group_vals=="B")], 0, 100)


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

    # Write data
    writeData(data)

}
