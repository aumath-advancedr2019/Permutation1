# Nice plot permutation function

plot_permutation <- function(data, observed_value, title){
  #making dataset to put in to permutation plot
  pd <- data.frame(i=1:length(data), value=data)
  #plotting the null distribution and observed difference with the observed as a line
  plot <- ggplot(data = pd, mapping = aes(x=value)) + 
    geom_histogram(fill="darkblue", bins=50) +
    geom_vline(color="darkorange",xintercept = as.numeric(observed_value)) + 
    xlab("")+
    ylab("Count")+
    ggtitle(title, "The orange line is the observed value")+
    theme_minimal()
  return(plot)
}