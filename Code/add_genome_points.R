#adds points to a plot set up with setup_genome_plot
#This function requires a table of information for the points
#that include the following columns, chromosome_name,
#start_position, end_position, and y value (LOD score, or some other
#score that is to be plotted.)
#the rownames of the info.table need to be included in the map
#id.col gives the column of info table holding the same kind of 
#IDs as in map

add_genome_points <- function(info.table, map, col = "black", ycol = 1, id.col = 1,
type = "p", ylab = "", verbose = TRUE){
  
  if(length(col == 1)){col <- rep(col, nrow(info.table))}

  mapV <- unlist(map)
  split.map <- strsplit(names(mapV), "\\.")
  map.chr <- sapply(split.map, function(x) x[1])
  marker.label <- sapply(split.map, function(x) x[2])

  u_chr <- unique(map.chr)
  chr.len <- sapply(map, function(x) max(x) - min(x))

  if(verbose){cat("Locating markers...\n")}
  marker.chr <- rep(NA, nrow(info.table))
  marker.pos <- rep(NA, nrow(info.table))
  for(i in 1:nrow(info.table)){
    if(verbose){report.progress(i, nrow(info.table))}
    marker.locale <- which(marker.label == info.table[i,id.col])
    if(length(marker.locale) > 0){
      marker.chr[i] <- map.chr[marker.locale]
      chr.locale <- which(names(map) == marker.chr[i])
      marker.chrom <- map[[chr.locale]]
      marker.idx <- which(names(marker.chrom) == info.table[i,id.col])
      if(length(marker.idx) > 0){
        marker.pos[i] <- marker.chrom[marker.idx]
      }
    }
  }
  
    
  if(verbose){cat("Adding points to plot...\n")}
  for(i in 1:length(u_chr)){
    gene.chr.locale <- which(marker.chr == u_chr[i])
    gene.pos <- marker.pos[gene.chr.locale]
    marker.idx <- match(marker.label[gene.chr.locale], rownames(info.table))
    marker.yval <- as.numeric(info.table[marker.idx,ycol])
    xvals <- gene.pos/chr.len[i] + i - 1
    points(x = xvals, y = marker.yval, type = type, pch = 16)
  }    

}