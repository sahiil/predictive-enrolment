# Repository of all the functions

# Gompertz
fit.gompertz <- function(group, df){
  # df = df.raw
  # group = unique(df.raw$GROUP1)[1]
  # group='Brookhaven College_ACCT-2301_DL_FALL 1ST 8 WEEK'
  # df.raw$GROUP1
  # Filtering the data for the group
  df.1 <- df[df$GROUP1 == group,]
  
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(NULL)
  
  # creating subsets
  iter <- seq(1, nrow(df.1), nrow(df.1)/10)
  j = 0
  # i=45.1
  for(i in iter){
    # i = iter[2]
    j = j + 1
    df.2 <- df.1[1:i,]
    
    # Initializing the factors
    alpha = mean(df$ENROLLMENT)
    beta = 9.1618 
    k = 0.0028
    
    # beta = 10
    # k = 5
    
    tryCatch({
      nls.gompertz <- minpack.lm::nlsLM(df.2$ENROLLMENT ~ alpha*exp(-beta*exp(-k*df.2$TIME)), 
                                        data = df.2, 
                                        start = list(alpha = alpha, beta = beta, k = k), 
                                        control = list(maxiter = 500))
      coef(nls.gompertz) 
      
      predict.gompertz <-growthmodels::gompertz(df.1$TIME, 
                                                alpha = coef(nls.gompertz)[["alpha"]], 
                                                beta = coef(nls.gompertz)[["beta"]], 
                                                k = coef(nls.gompertz)[["k"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP1 = group,
                                                             MODEL = paste0("POST ", i, " DAYS"),
                                                             DAYS_PASSED = round(i),
                                                             MODEL_ID = paste0("MODEL ",j),
                                                             TIME = df.1$TIME, 
                                                             ENROLLMENT = df.1$ENROLLMENT,
                                                             PREDICTION = predict.gompertz,
                                                             TYPE = "GOMPERTZ"))
      print(paste0("MODEL COMPLETED FOR -- ", i, " DAYS"))
    }, error = function(e){print(paste0("MODEL FAILED FOR -- ", i, " DAYS")) }
    ) 
  }
  print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
  return(df.predictions)
}

# Weibull
fit.weibull <- function(group, df){
  # df = df.raw
  # group = unique(df.raw$GROUP1)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP1 == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(NULL)
  
  # creating subsets
  iter <- seq(1, nrow(df.1), nrow(df.1)/10)
  j = 0
  for(i in iter){
    # i = iter[2]
    j = j + 1
    df.2 <- df.1[1:i,]
    
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
      
      predict.weibull <-growthmodels::weibull(df.1$TIME, 
                                              alpha = coef(nls.weibull)[["alpha"]], 
                                              beta = coef(nls.weibull)[["beta"]], 
                                              k = coef(nls.weibull)[["k"]],
                                              m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP1 = group,
                                                             MODEL = paste0("POST ", i, " DAYS"),
                                                             MODEL_ID = paste0("MODEL ",j),
                                                             DAYS_PASSED = round(i),
                                                             TIME = df.1$TIME, 
                                                             ENROLLMENT = df.1$ENROLLMENT,
                                                             PREDICTION = predict.weibull,
                                                             TYPE = "WEIBULL"))
      print(paste0("MODEL COMPLETED FOR -- ", i, " DAYS"))
      
    }, error = function(e){print(paste0("MODEL FAILED FOR -- ", i, " DAYS"))}
    
    )   }
  print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
  return(df.predictions)
}

# LogLogistic
fit.loglogit <- function(group, df){
  # df = df.raw
  # group = unique(df.raw$GROUP1)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP1 == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(NULL)
  
  # creating subsets
  iter <- seq(1, nrow(df.1), nrow(df.1)/10)
  j = 0
  for(i in iter){
    # i = iter[2]
    j = j + 1
    df.2 <- df.1[1:i,]
    
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
      
      predict.loglogit <-growthmodels::logistic(df.1$TIME, 
                                                alpha = coef(nls.loglogit)[["alpha"]], 
                                                beta = coef(nls.loglogit)[["beta"]], 
                                                k = coef(nls.loglogit)[["k"]])
      # m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP1 = group,
                                                             MODEL = paste0("POST ", i, " DAYS"),
                                                             MODEL_ID = paste0("MODEL ",j),
                                                             DAYS_PASSED = round(i),
                                                             TIME = df.1$TIME, 
                                                             ENROLLMENT = df.1$ENROLLMENT,
                                                             PREDICTION = predict.loglogit,
                                                             TYPE = "LOGLOGIT"))
      print(paste0("MODEL COMPLETED FOR -- ", i, " DAYS"))
      
    }, error = function(e){print(paste0("MODEL FAILED FOR -- ", i, " DAYS"))}
    
    )   }
  print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
  return(df.predictions)
}

# Negative Exponential
fit.negexp <- function(group, df){
  # df = df.raw
  # group = unique(df.raw$GROUP1)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP1 == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(NULL)
  
  # creating subsets
  iter <- seq(1, nrow(df.1), nrow(df.1)/10)
  j = 0
  for(i in iter){
    # i = iter[2]
    j = j + 1
    df.2 <- df.1[1:i,]
    
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
      
      predict.negexp <-growthmodels::negativeExponential(df.1$TIME, 
                                                         alpha = coef(nls.negexp)[["alpha"]], 
                                                         # beta = coef(nls.loglogit)[["beta"]], 
                                                         k = coef(nls.negexp)[["k"]])
      # m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP1 = group,
                                                             MODEL = paste0("POST ", i, " DAYS"),
                                                             MODEL_ID = paste0("MODEL ",j),
                                                             DAYS_PASSED = round(i),
                                                             TIME = df.1$TIME, 
                                                             ENROLLMENT = df.1$ENROLLMENT,
                                                             PREDICTION = predict.negexp,
                                                             TYPE = "NEGATIVE EXP"))
      print(paste0("MODEL COMPLETED FOR -- ", i, " DAYS"))
      
    }, error = function(e){print(paste0("MODEL FAILED FOR -- ", i, " DAYS"))}
    
    )   }
  print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
  return(df.predictions)
}

# Collate Results
collate.results <- function(df){
  df.chck <- df%>%
    group_by(GROUP1) %>%
    mutate(MAX_WK = max(TIME))
  
  df.chck <- df.chck[df.chck$TIME == df.chck$MAX_WK,]
  df.chck$PREDICTION <- round(df.chck$PREDICTION,1)
  return(df.chck)
}

# Linear
fit.linear <- function(group, df){
  # browser()
  # df = df.raw
  # group = unique(df.raw$GROUP1)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP1 == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(NULL)
  
  # creating subsets
  iter <- seq(1, nrow(df.1), nrow(df.1)/10)
  j = 0
  for(i in iter){
    # i = iter[2]
    j = j + 1
    df.2 <- df.1[1:i,]
    
    tryCatch({
      nls.linear <- lm(ENROLLMENT ~ 1 + TIME, 
                       data = df.2)
      coef(nls.linear) 
      
      predict.linear <-predict(nls.linear, newdata = df.1)
      # m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP1 = group,
                                                             MODEL = paste0("POST ", i, " DAYS"),
                                                             MODEL_ID = paste0("MODEL ",j),
                                                             DAYS_PASSED = round(i),
                                                             TIME = df.1$TIME, 
                                                             ENROLLMENT = df.1$ENROLLMENT,
                                                             PREDICTION = predict.linear,
                                                             TYPE = "LINEAR"))
      # print(paste0("MODEL COMPLETED FOR -- ", i, " DAYS"))
      
    }, error = function(e){print(paste0("MODEL FAILED FOR -- ", i, " DAYS"))}
    
    )   }
  # print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
  return(df.predictions)
}

# Logrithmic
fit.log <- function(group, df){
  # browser()
  # df = df.raw
  # group = unique(df.raw$GROUP1)[1]
  # Filtering the data for the group
  df.1 <- df[df$GROUP1 == group,]
  
  
  # creating a null data frame for predictions
  df.predictions <- data.frame(NULL)
  
  # creating subsets
  iter <- seq(1, nrow(df.1), nrow(df.1)/10)
  j = 0
  for(i in iter){
    # i = iter[2]
    j = j + 1
    df.2 <- df.1[1:i,]
    
    tryCatch({
      nls.log <- lm(ENROLLMENT ~ 1 + log(TIME), 
                    data = df.2)
      coef(nls.log) 
      
      predict.log <-predict(nls.log, newdata = df.1)
      # m = coef(nls.weibull)[["m"]])
      df.predictions <- bind_rows(df.predictions, data.frame(GROUP1 = group,
                                                             MODEL = paste0("POST ", i, " DAYS"),
                                                             MODEL_ID = paste0("MODEL ",j),
                                                             DAYS_PASSED = round(i),
                                                             TIME = df.1$TIME, 
                                                             ENROLLMENT = df.1$ENROLLMENT,
                                                             PREDICTION = predict.log,
                                                             TYPE = "LOGRITHMIC"))
      # print(paste0("MODEL COMPLETED FOR -- ", i, " DAYS"))
      
    }, error = function(e){print(paste0("MODEL FAILED FOR -- ", i, " DAYS"))}
    
    )   }
  # print(paste0("=================MODEL COMPLETED FOR -- ", group, "====================="))
  return(df.predictions)
}

