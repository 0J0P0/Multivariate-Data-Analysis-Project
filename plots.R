
histogram_plots <- function(df, num_col, palette="YlGnBu") {
  if (palette=="YlGnBu")
    YlGnBu = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")

  hist_plots <- lapply(seq_along(names(df)[-num_col]), function(i) {
    p <- plot_ly(type = "histogram", name = names(df)[-num_col][i])
    p <- add_trace(p, data = df, x = ~get(names(df)[-num_col][i]),
                   marker = list(color=YlGnBu[i]))
    p
  })
  
  subplot <- subplot(hist_plots, nrows = 3, margin = 0.05) %>%
    layout(title = 'Feature Histograms')
  
  subplot
}


box_plots <- function(df, num_col, palette="YlGnBu") {
  if (palette=="YlGnBu")
    YlGnBu = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")
  
  boxplots <- lapply(seq_along(names(df)[-num_col]), function(i) {
    p <- plot_ly(type = "box", name = names(df)[-10][i])
    p <- add_trace(p, data = df, y=~get(names(df)[-10][i]),
                   color = I(YlGnBu[i]))
    p
  })
  
  subplot <- subplot(plotlist = boxplots, nrows = 3, margin = 0.05) %>%
    layout(title = 'Feature Boxplots')
  
  subplot
}