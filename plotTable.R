plotTable <- function(data) {
  years <- as.numeric(names(data)[-1])
  dataShape <- reshape2::melt(data, id.vars = 1)
  dataPercent <- split(dataShape$value, dataShape$group)
  
  # Ensure the range for ylim is finite by removing NA and Inf values
  valid_data <- unlist(data[-1])
  valid_data <- valid_data[is.finite(valid_data) & !is.na(valid_data)]
  
  # Initialize a new plot
  plot.new()
  plot.window(xlim = range(years), ylim = range(valid_data))
  
  # Add axes to the plot
  axis(1)
  axis(2)
  box()
  clours <- 1:length(dataPercent)
  mapply(function(x, i) {
    lines(years, x, col = i)
  }, dataPercent, clours)
  legend("topright", legend = names(dataPercent), lty = 1, col = clours,
         bg = "transparent")
}
