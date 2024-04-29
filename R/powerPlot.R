require(ggplot2)

powerPlot <- function(powerSims, power_threshold = 80) {
  powerplot <- ggplot(powerSims, aes(x = EffectSize, y = Power)) +
    geom_point(color = "blue") + geom_line(color="blue") +
    theme_minimal() +
    labs(x = "Effect Size (standardized regression coefficients)", y = "Power", title = "Power Analysis Plot") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = power_threshold, color = "red", linetype = "dashed") + scale_x_continuous(breaks = seq(.01, .1, by = .01)) + annotate("text", x = .01, y = power_threshold, label = paste(power_threshold,  "% power"), hjust = 0, vjust = -1, color = "red")
    return(powerplot)

}



