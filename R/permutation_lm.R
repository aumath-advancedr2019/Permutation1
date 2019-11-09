# Permutation lm function

permutation_lm <- function(dataset, predictor, response, model1, model2, no_perm=10000, nice_plot=F, progress=TRUE){
  # Verify if the columns exists and if the groups exists
  accepted_models <- c("linear","quadratic","cubic")
  
  if (any(names(dataset) == predictor) == FALSE | any(names(dataset) == response) == FALSE){
    stop("One of the columns does not exists. Please, check the spelling or the dataset you are giving") 
  } else if (any(accepted_models == model1) == FALSE | any(accepted_models == model2) == FALSE) {
    stop("Error: Models must be a combination of linear, quadractic or cubic.")
  }
  
  # Select columns in the dataset and remane those
  dataset <- dataset[c(predictor,response)]      # Select
  names(dataset) <- c("predictor","response")    # Rename
  
  #Check if the dataset is normally distributed, and give a warning, if it is:
  if (shapiro.test(dataset$response)$p.value>0.05){
    warning("Data is normally distributed, it might be better to use a parametric test, (e.g. anova)")
  }
  
  r <- rep(NA,no_perm)
  
  if (progress == TRUE) {
    # SELECT MODEL
    if ((model1=="linear" & model2=="quadratic")|(model2=="linear" & model1=="quadratic")){    # For linear and quadratic model:
      # Observed models:
      mod1 <- lm(response ~ predictor, data=dataset)
      mod2 <- lm(response ~ poly(predictor, 2), data=dataset)
      # Progress bar
      pb <- txtProgressBar(min = 1, max = no_perm, style = 3)
      for (i in 1:length(r)) {
        setTxtProgressBar(pb, i)
        # Shuffle dataset
        p <- dataset
        p$response <- sample(p$response)
        
        # Make models
        m1 <- lm(response ~ predictor, data=p)
        m2 <- lm(response ~ poly(predictor, 2), data=p)
        
        # Make F-distribution
        f_dist <- anova(m1, m2)
        
        # Add F-values to r-vector:
        r[i] <- f_dist$`F`[2]
        
      }
      close(pb)
    } else if ((model1=="linear" & model2=="cubic")|(model2=="linear" & model1=="cubic")){    # For linear and cubic model:
      # Observed models:
      mod1 <- lm(response ~ predictor, data=dataset)
      mod2 <- lm(response ~ poly(predictor, 3), data=dataset)
      # Progress bar
      pb <- txtProgressBar(min = 1, max = no_perm, style = 3)
      for (i in 1:length(r)) {
        setTxtProgressBar(pb, i)
        # Shuffle dataset
        p <- dataset
        p$response <- sample(p$response)
        
        # Make models
        m1 <- lm(resp ~ predictor, data=p)
        m2 <- lm(resp~ poly(predictor, 3), data=p)
        
        # Make F-distribution
        f_dist <- anova(m1, m2)
        
        # Add F-values to r-vector:
        r[i] <- f_dist$`F`[2]
        
      }
      close(pb)
    } else if ((model1=="quadratic" & model2=="cubic")|(model2=="quadratic" & model1=="cubic")){    # For quadratic and cubic model:
      # Observed models:
      mod1 <- lm(response ~ poly(predictor,2), data=dataset)
      mod2 <- lm(response ~ poly(predictor, 3), data=dataset)
      # Progress bar
      pb <- txtProgressBar(min = 1, max = no_perm, style = 3)
      for (i in 1:length(r)) {
        setTxtProgressBar(pb, i)
        # Shuffle dataset
        p <- dataset
        p$response <- sample(p$response)
        
        # Make models
        m1 <- lm(response ~ poly(predictor, 2), data=p)
        m2 <- lm(response ~ poly(predictor, 3), data=p)
        
        # Make F-distribution
        f_dist <- anova(m1, m2)
        
        # Add F-values to r-vector:
        r[i] <- f_dist$`F`[2]
        
      }
      close(pb)
    }
  } else {
    # SELECT MODEL
    if ((model1=="linear" & model2=="quadratic")|(model2=="linear" & model1=="quadratic")){    # For linear and quadratic model:
      # Observed models:
      mod1 <- lm(response ~ predictor, data=dataset)
      mod2 <- lm(response ~ poly(predictor, 2), data=dataset)
      # Progress bar
      for (i in 1:length(r)) {
        # Shuffle dataset
        p <- dataset
        p$response <- sample(p$response)
        
        # Make models
        m1 <- lm(response ~ predictor, data=p)
        m2 <- lm(response ~ poly(predictor, 2), data=p)
        
        # Make F-distribution
        f_dist <- anova(m1, m2)
        
        # Add F-values to r-vector:
        r[i] <- f_dist$`F`[2]
      }
    } else if ((model1=="linear" & model2=="cubic")|(model2=="linear" & model1=="cubic")){    # For linear and cubic model:
      # Observed models:
      mod1 <- lm(response ~ predictor, data=dataset)
      mod2 <- lm(response ~ poly(predictor, 3), data=dataset)
      # Progress bar
      for (i in 1:length(r)) {
        # Shuffle dataset
        p <- dataset
        p$response <- sample(p$response)
        
        # Make models
        m1 <- lm(resp ~ predictor, data=p)
        m2 <- lm(resp~ poly(predictor, 3), data=p)
        
        # Make F-distribution
        f_dist <- anova(m1, m2)
        
        # Add F-values to r-vector:
        r[i] <- f_dist$`F`[2]
      }
    } else if ((model1=="quadratic" & model2=="cubic")|(model2=="quadratic" & model1=="cubic")){    # For quadratic and cubic model:
      # Observed models:
      mod1 <- lm(response ~ poly(predictor,2), data=dataset)
      mod2 <- lm(response ~ poly(predictor, 3), data=dataset)
      # Progress bar
       for (i in 1:length(r)) {
        # Shuffle dataset
        p <- dataset
        p$response <- sample(p$response)
        
        # Make models
        m1 <- lm(response ~ poly(predictor, 2), data=p)
        m2 <- lm(response ~ poly(predictor, 3), data=p)
        
        # Make F-distribution
        f_dist <- anova(m1, m2)
        
        # Add F-values to r-vector:
        r[i] <- f_dist$`F`[2]
      }
    }
  }
  
  
  # The observed value and format
  observed <- anova(mod1, mod2)$`F`[2]
  estimate <- as.numeric(observed)
  names(estimate) <- c("Observed F value between models")
  
  # P-value and format
  p_value <- (sum((r) >= (observed))+1)/(length(r)+1)
  names(p_value) <- "p-value"
  
  # Result format
  result = list(method = sprintf("Permutation Test (ANOVA) for %s and %s models", model1, model2), p.value = p_value,
                estimate = estimate,
                data.name = sprintf("Permutation for predictor %s and response %s", predictor, response))
  class(result) <- "htest"
  
  # If statement to plot or not
  if (nice_plot == TRUE) {
    # Plot + result
    
    plot <- plot_permutation(r,observed, "Permutation test (Model comparison, F-test)")
    
    print(plot)
    return(result)
  } else if (nice_plot == FALSE) {
    # Result
    hist(r, col="darkblue", breaks = 50, main = "F permuted distribution", ylab = "Count"); abline(v = observed, col = "darkorange")
    return(result)
  }
}