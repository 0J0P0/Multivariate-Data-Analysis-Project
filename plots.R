YlGnBu = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")


histogram_plots <- function(df, palette="YlGnBu") {
  hist_plots <- lapply(seq_along(names(df)), function(i) {
    p <- plot_ly(type = "histogram", name = names(df)[i])
    p <- add_trace(p, data = df, x = ~get(names(df)[i]),
                   marker = list(color=YlGnBu[i]))
    p
  })
  
  subplot <- subplot(hist_plots, nrows = 3, margin = 0.05) %>%
    layout(title = 'Feature Histograms')
  
  subplot
}


box_plots <- function(df, palette="YlGnBu") {
  boxplots <- lapply(seq_along(names(df)), function(i) {
    p <- plot_ly(type = "box", name = names(df)[i])
    p <- add_trace(p, data = df, y=~get(names(df)[i]),
                   color = I(YlGnBu[i]))
    p
  })
  
  subplot <- subplot(plotlist = boxplots, nrows = 3, margin = 0.05) %>%
    layout(title = 'Feature Boxplots')
  
  subplot
}


qq_plots <- function(df) {
  lapply(seq_along(names(df)), function(i) {
    p <- ggplot(data=df, aes(sample=get(names(df)[i]))) +
      stat_qq_band(color=YlGnBu[3], fill=YlGnBu[3], alpha=0.3) +
      stat_qq_line(color=YlGnBu[5]) +
      stat_qq_point(color=YlGnBu[7], fill=YlGnBu[6], shape=21) +
      theme_bw() +
      xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
      ggtitle(paste("Normal Q-Q Plot for", names(df)[i])) +
      theme(plot.title = element_text(hjust = 0.5))
    p
  })
}
