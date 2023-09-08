#sets up a genome plot with chromomes for adding points to
#requires a map from qtl2


setup_genome_plot <- function(map, ylim = c(0,8), chr = c(1:19, "X")){
  
  ymax <- max(ylim)
  ymin <- min(ylim)
  plot.height <- ymax - ymin

  u_chr <- names(map)
  chr.locale <- sapply(u_chr, function(x) which(names(map) == x))
  names(chr.locale) <- u_chr
  chr.min <- sapply(chr.locale, function(x) min(map[[x]]))
  chr.max <- sapply(chr.locale, function(x) max(map[[x]]))
  chr.len <- chr.max - chr.min

  par(xpd = TRUE)
  plot.new()
  plot.window(xlim = c(0, length(u_chr)+1), ylim = ylim)
  label.shift <- ymin - (plot.height*0.02)

  for(i in 1:length(u_chr)){
    xmin <- (chr.min[i]/chr.max[i] + (i-1))
    xmax <- ((chr.min[i]/chr.max[i])+i)
    if(i%%2 == 0){
      draw.rectangle(xmin, xmax, ymin, ymax, fill = rgb(0.8, 0.8, 0.8), border.col = NA)
      }
  text(x = mean(c(xmin, xmax)), y = label.shift, labels = u_chr[i], adj = 0.5)
  }
  par(xpd = FALSE)
}

