# Permutation mean function

permutation_mean <- function(dataset, name, count, group1, group2, no_perm=10000, nice_plot=FALSE){
  # Verify if the columns exists and if the groups exists
  if (any(names(dataset) == name) == FALSE | any(names(dataset) == count) == FALSE){
    stop("One of the columns does not exists. Please, check the spelling or the dataset you are giving") 
  } else if (any(unique(get(name,dataset)) == group1) == FALSE | any(unique(get(name,dataset)) == group2) == FALSE) {
    stop("One of the groups does not exists. Please, check the spelling or the dataset you are giving")
  }
  
  # Select columns in the dataset and remane those
  dataset <- dataset[c(count,name)]      # Select
  names(dataset) <- c("count","name")    # Rename
  
  #Check if the dataset is normally distributed, and give a warning, if it is:
  if (shapiro.test(dataset$count)$p.value>0.05){
    warning("Data is normally distributed, it might be better to use a parametric test (e.g. t-test)")
  }
  
  r <- rep(NA,no_perm)
  
  # Progress bar
  pb <- txtProgressBar(min = 1, max = no_perm, style = 3)
  
  for (i in 1:length(r)) {
    setTxtProgressBar(pb, i)
    #shuffle dataset
    p <- dataset
    p$count <- sample(p$count)
    
    #calculate mean for group 1 and group 2
    g1 <- mean(p[which(p$name == group1),]$count)
    g2 <- mean(p[which(p$name == group2),]$count)
    
    #add difference in mean to vector
    r[i] <- as.numeric(g1) - as.numeric(g2)
  }
  close(pb)
  
  # The observed difference
  m1 <- mean(dataset[which(dataset$name == group1),]$count)
  m2 <- mean(dataset[which(dataset$name == group2),]$count)
  observed <- m1-m2       # Observed mean
  
  estimate <- c(as.numeric(m1),as.numeric(m2),as.numeric(observed))
  names(estimate) <- c("Mean first group","Mean second group","Observed mean difference")
  
  # P-value
  p_value <- ((sum(abs(r) >= abs(observed))+1)/(length(r)+1))
  names(p_value) <- "p-value"
  
  result = list(method = "Permutation Test (Mean)", p.value = p_value,
                estimate = estimate,
                data.name = sprintf("Permutation for group: %s and %s", group1, group2))
  class(result) <- "htest"
  
  # If statement to plot or not
  if (nice_plot == TRUE) {
    # Plot + result
    
    plot <- plot_permutation(r, observed, "Permutation test (Mean)")
    
    print(plot)
    return(result)
  } else if (nice_plot == FALSE) {
    # Result
    hist(r, col="darkblue", breaks = 50, main = "Mean permuted distribution", ylab = "Count"); abline(v = observed, col = "darkorange")
    return(result)
  }
}
