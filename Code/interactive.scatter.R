interactive.scatter <- function(x, y, col, pt.names){

  library(plotly)
  library(dplyr)

  x[is.na(x)] = -Inf
  y[is.na(y)] = -Inf

df = data.frame(pt.names, x, y)

fig <- plot_ly(df,
               x = ~x,
               y = ~y,
               color = ~col,
               hovertext = paste("<br> Gene :", df$pt.names),
               type = "scatter",
               mode = "markers"
               )

#x.label = paste("Max. LOD (", paste(et.choice, collapse = ", "), ")", sep = "")
#y.label = paste("Max. FS (", paste(tissue.choice, collapse = ", "), ")", sep = "")

x.label = list(title = "Max. LOD")
y.label = list(title = "Max. Functional Score")
fig <- fig %>% layout(xaxis = x.label, yaxis = y.label)
fig

out = NULL
out$df = df
out$fig = fig
return(out)

}