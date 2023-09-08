interactive.2d.cdf.scatter <- function(x, y, pt.names){

  library(plotly)
  library(dplyr)

  x[is.na(x)] = -Inf
  y[is.na(y)] = -Inf
    
cdf = matrix(0, nrow = length(x), ncol = 1)
for(i in 1:length(cdf)){
  cdf[i] = length(which(x < x[i] & y < y[i]))
}
cdf = cdf / length(cdf)

df = data.frame(pt.names, x, y, cdf)
df = df[df[ , "cdf"] > 0, ]

fig <- plot_ly(df,
               x = ~x,
               y = ~y,
               color = ~cdf,
               hovertext = paste("<br> Gene :", df$pt.names,
                                 "<br> Combined score :", df$cdf),
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
out$cdf = cdf
out$fig = fig
return(out)

}