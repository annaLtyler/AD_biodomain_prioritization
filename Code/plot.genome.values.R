#This function plots values at genomic positions. 
#y.vals  specifies the y values to be plotted.
#is can be either a numeric vector, or a list of numeric
#vectors all the same length. If a list, the different
#vectors of the list will be overplotted.
#chromosome.labels should be a numeric vector the same length
#as y.vals indicating which chromosome each y value is on.
#chromosome.labels must be a vector regardless of the structure
#of y.vals
#pos is a vector the same length as y.vals indicating the
#genomic position of each point. 
#pos must be a vector regardless of the structure of y.vals.
#col is an optional color or vector of colors indicating the
#color of all points or each point respectively. If y.vals is
#a list, col can also be a list to indicate independent colors
#for each set of y values.
#pt.labels is a vector of length y.vals indicating text labels
#for individual points. Points that are not labeled should have
#a value of "". If y.vals is a list, pt.labels can also be a list
#to indicate independent labels for each set of y values.
#To suppress pt.labels, it should be valued NA
#ylim is an optional vector of length 2 giving the y limits of the
#plot. If NULL, ylim is set to the min and max of y.vals.
#pch and type can be a single value, or a vector of the same 
#length as the list y.vals indicating the pch/type for each set 
#of plots.


plot.genome.values <- function(y.vals, chromosome.labels, pos, col = "black", 
pt.labels = NA, ylim = NULL, type = "p", pch = 16){

    u_chr <- sort(as.numeric(unique(chromosome.labels)))
    n.panels <- length(u_chr) + 1
    if(is.null(ylim)){
        ylim <- c(min(y.vals, na.rm = TRUE), max(y.vals, na.rm = TRUE))
    }
      
    layout.mat <- matrix(1:n.panels, nrow = 1)
    #quartz(width = 11, height = 5)
    layout(layout.mat)
    #layout.show(21)

    #convert a vector to a list of length len
    #to match structure across all variables 
    #that can be either lists or vectors.
    v2list <- function(V, len){
        v.list <- vector(mode = "list", length = len)
        for(i in 1:len){
            v.list[[i]] <- V
        }
        return(v.list)
    }

    #convert all listable variables to lists.
    if(class(y.vals) != "list"){ 
        list.len = 1
        y.vals <- v2list(y.vals, list.len)
        col <- v2list(col, list.len)
        pt.labels <- v2list(pt.labels, list.len)
    }else{
        list.len <- length(y.vals)
        if(class(col) != "list"){
            col <- v2list(col, list.len)
        }
        if(class(pt.labels) != "list"){
            pt.labels <- v2list(pt.labels, list.len)
        }
    }
    
    #expand the color vector to match y.vals if we need to.
    for(i in 1:length(y.vals)){
        if(length(col[[i]]) == 1){
            col[[i]] <- rep(col[[i]], length(y.vals[[i]]))
        }
    }

    #make sure other vectors are the proper length for 
    #lists of y.vals
    if(length(type) < list.len){
        type <- rep(type, list.len)
    }
    if(length(pch) < list.len){
        pch <- rep(pch, list.len)
    }
    
    par(mar = c(4,0.2,4,0.2))    
    plot.new()
    plot.window(xlim = c(0, 1), ylim = ylim)
    axis(2, line = -2.5)
    
    par(xpd = NA)
    for(i in 1:length(u_chr)){
        par(mar = c(4,0.2,4,0.2))
        if(i == length(u_chr)){par(mar = c(4,0.2,4,1))}
        chr.locale <- which(chromosome.labels == u_chr[i]) 
        min.pos <- min(pos[chr.locale])
        max.pos <- max(pos[chr.locale])

        plot.new()
        plot.window(xlim = c(min.pos, max.pos), ylim = ylim)
        
        #plot points for every element of y.vals
        for(j in 1:length(y.vals)){
            points(pos[chr.locale], y.vals[[j]][chr.locale], type = type[j], 
            col = col[[j]][chr.locale], pch = pch[j])
        
            if(length(pt.labels[[j]]) > 0){
                to.label <- which(pt.labels[[j]][chr.locale] != "")
                if(length(to.label) > 0){
                    label.genes <- pt.labels[[j]][chr.locale[to.label]]
                    text(x = pos[chr.locale[to.label]], 
                    y = y.vals[[j]][chr.locale[to.label]], 
                    labels = pt.labels[[j]][chr.locale[to.label]], pos = 3)
                }
            }
        } #end looping through multiple elements of y.vals

        mtext(side = 1, text = u_chr[i]) #add chromosome label

    }
    par(xpd = FALSE)
        
}