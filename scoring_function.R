
# FUNCTIONS FOR DIFFERENT CURVES ------------------------------------------

# Gompertz
fit.gompertz <- function(group, df){
  # df = model.df
  # group = unique(df.raw$GROUP)[1]
  
  # Filtering the data for the group
  df.1 <- df[df$GROUP == group,]
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(GROUP = group,
                               TIME = 0,
                               PREDICTION = 0,
                               TYPE = "GOMPERTZ")
  
  # creating subsets
  
    df.2 <- df.1
    
    # Initializing the factors
    alpha = mean(df$ENROLLMENT)
    beta = 9.1618 
    k = 0.0028
    
    tryCatch({
      nls.gompertz <- minpack.lm::nlsLM(df.2$ENROLLMENT ~ alpha*exp(-beta*exp(-k*df.2$TIME)), 
                                        data = df.2, 
                                        start = list(alpha = alpha, beta = beta, k = k), 
                                        control = list(maxiter = 500))
      coef(nls.gompertz) 
      
      predict.gompertz <-growthmodels::gompertz(c(1:n_period), 
                                                alpha = coef(nls.gompertz)[["alpha"]], 
                                                beta = coef(nls.gompertz)[["beta"]], 
                                                k = coef(nls.gompertz)[["k"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP = group,
                                                             TIME = c(1:n_period),
                                                             PREDICTION = predict.gompertz,
                                                             TYPE = "GOMPERTZ"))
    }, error = function(e){print(paste0("MODEL FAILED ")) }
    ) 

  print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
  return(df.predictions)
}

# Weibull
fit.weibull <- function(group, df){
  # df = df.raw
  # group = unique(df.raw$GROUP)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(GROUP = group,
                               TIME = 0,
                               PREDICTION = 0,
                               TYPE = "WEIBULL")
  
  
  # creating subsets

    df.2 <- df.1
    
    # Initializing the factors
    alpha = max(df.2$ENROLLMENT)
    beta = mean(df.2$ENROLLMENT) 
    k = 0.001
    m = 1
    
    tryCatch({
      nls.weibull <- minpack.lm::nlsLM(df.2$ENROLLMENT ~ alpha -beta*exp(-k*(df.2$TIME)^m), 
                                       data = df.2, 
                                       start = list(alpha = alpha, beta = beta, k = k, m = m), 
                                       control = list(maxiter = 1000))
      coef(nls.weibull) 
      
      predict.weibull <-growthmodels::weibull(c(1:n_period), 
                                              alpha = coef(nls.weibull)[["alpha"]], 
                                              beta = coef(nls.weibull)[["beta"]], 
                                              k = coef(nls.weibull)[["k"]],
                                              m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP = group,
                                                             TIME = c(1:n_period),
                                                             PREDICTION = predict.weibull,
                                                             TYPE = "WEIBULL"))
    }, error = function(e){print(paste0("MODEL FAILED ")) }
    ) 
    
    print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
    return(df.predictions)
}


# LogLogistic
fit.loglogit <- function(group, df){
  # df = df.raw
  # group = unique(df.raw$GROUP)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(GROUP = group,
                               TIME = 0,
                               PREDICTION = 0,
                               TYPE = "LOGLOGIT")
  
  
  # creating subsets
  
    df.2 <- df.1
    
    # Initializing the factors
    alpha = max(df.2$ENROLLMENT)
    beta = mean(df.2$ENROLLMENT)
    # k = 0.001
    # alpha = 10
    # beta = 10
    k = 0.001
    # m = 1
    
    tryCatch({
      nls.loglogit <- minpack.lm::nlsLM(df.2$ENROLLMENT ~ alpha/(1+beta*exp(-k*(df.2$TIME))), 
                                        data = df.2, 
                                        start = list(alpha = alpha, beta = beta, k = k), 
                                        control = list(maxiter = 1000))
      coef(nls.loglogit) 
      
      predict.loglogit <-growthmodels::logistic(c(1:n_period), 
                                                alpha = coef(nls.loglogit)[["alpha"]], 
                                                beta = coef(nls.loglogit)[["beta"]], 
                                                k = coef(nls.loglogit)[["k"]])
      # m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP = group,
                                                             TIME = c(1:n_period),
                                                             PREDICTION = predict.loglogit,
                                                             TYPE = "LOGLOGIT"))
    }, error = function(e){print(paste0("MODEL FAILED ")) }
    ) 
    
    print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
    return(df.predictions)
}

# Negative Exponential
fit.negexp <- function(group, df){
  # df = df.raw
  # group = unique(df.raw$GROUP)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(GROUP = group,
                               TIME = 0,
                               PREDICTION = 0,
                               TYPE = "NEGEXP")
  
  # creating subsets

    df.2 <- df.1
    
    # Initializing the factors
    alpha = max(df.2$ENROLLMENT)
    # beta = mean(df.2$ENROLLMENT)
    # k = 0.001
    # alpha = 10
    # beta = 10
    k = 1
    # m = 1
    
    tryCatch({
      nls.negexp <- minpack.lm::nlsLM(df.2$ENROLLMENT ~ alpha*(1-exp(-k*(df.2$TIME))), 
                                      data = df.2, 
                                      start = list(alpha = alpha, k = k), 
                                      control = list(maxiter = 1000))
      coef(nls.negexp) 
      
      predict.negexp <-growthmodels::negativeExponential(c(1:n_period), 
                                                         alpha = coef(nls.negexp)[["alpha"]], 
                                                         # beta = coef(nls.loglogit)[["beta"]], 
                                                         k = coef(nls.negexp)[["k"]])
      # m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP = group,
                                                             TIME = c(1:n_period),
                                                             PREDICTION = predict.negexp,
                                                             TYPE = "NEGEXP"))
    }, error = function(e){print(paste0("MODEL FAILED ")) }
    ) 
    
    print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
    return(df.predictions)
}

# Linear
fit.linear <- function(group, df){
  # browser()
  # df = df.raw
  # group = unique(df.raw$GROUP)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(GROUP = group,
                               TIME = 0,
                               PREDICTION = 0,
                               TYPE = "LINEAR")
  
  # creating subsets
  
    df.2 <- df.1
    
    tryCatch({
      nls.linear <- lm(ENROLLMENT ~ 1 + TIME, 
                       data = df.2)
      coef(nls.linear) 
      
      predict.linear <-predict(nls.linear, newdata = data.frame(TIME = c(1:n_period)))
      # m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP = group,
                                                             TIME = c(1:n_period),
                                                             PREDICTION = predict.linear,
                                                             TYPE = "LINEAR"))
    }, error = function(e){print(paste0("MODEL FAILED ")) }
    ) 
    
    print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
    return(df.predictions)
}

# Logrithmic
fit.log <- function(group, df){
  # browser()
  # df = df.raw
  # group = unique(df.raw$GROUP)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(GROUP = group,
                               TIME = 0,
                               PREDICTION = 0,
                               TYPE = "LOGRITHMIC")
  
  # creating subsets
  
  df.2 <- df.1
  
  tryCatch({
    nls.linear <- lm(ENROLLMENT ~ 1 + log(TIME), 
                     data = df.2)
    coef(nls.linear) 
    
    predict.linear <-predict(nls.linear, newdata = data.frame(TIME = c(1:n_period)))
    # m = coef(nls.weibull)[["m"]])
    df.predictions <- bind_rows(df.predictions, data.frame(GROUP = group,
                                                           TIME = c(1:n_period),
                                                           PREDICTION = predict.linear,
                                                           TYPE = "LOGRITHMIC"))
  }, error = function(e){print(paste0("MODEL FAILED ")) }
  ) 
  
  print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
  return(df.predictions)
  }
