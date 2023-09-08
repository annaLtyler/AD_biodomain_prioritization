#enrich.mat is what is returned from plot.enrichment.group
#This function creates a bipartite graph from the enrichment
#matrix and returns the bipartite projection onto the terms
#and the traits.

enrich_bipartite_proj <- function(enrich.mat, vertex.col = "gray", 
label.vertex = NULL, label.col = "lightblue", 
search.name = c("full", "partial")){

    #find columns with no enrichment
    no.enrich <- which(colSums(enrich.mat) == 0)
    if(length(no.enrich) > 0){
        cat("Removing columns with no enrichment:", "\n")
        cat(colnames(enrich.mat)[no.enrich], sep = "\n")
        has.enrich <- which(colSums(enrich.mat) > 0)
        enrich.mat <- enrich.mat[,has.enrich]
        if(!is.null(label.vertex)){
            keep.label <- which(label.vertex %in% colnames(enrich.mat))
            label.vertex <- label.vertex[keep.label]
        }
    }

    if(is.null(enrich.mat)){
        return(NULL)
    }

    search.name = search.name[1]

    terms <- rownames(enrich.mat)
    l.traits <- colnames(enrich.mat)

    pair.list <- lapply(1:ncol(enrich.mat), function(x) enrich.mat[which(enrich.mat[,x] != 0),x,drop=FALSE])
    pair.table <- lapply(1:length(pair.list), function(x) cbind(rep(colnames(enrich.mat)[x], length(pair.list[[x]])), names(pair.list[[x]]), pair.list[[x]]))
    
    all.pairs <- Reduce("rbind", pair.table)
    if(ncol(all.pairs) == 2){
        pair.mat <- cbind(rownames(all.pairs), all.pairs[,1])
        all.pairs <- pair.mat
    }
    inc.mat <- incidence.matrix(all.pairs)
    inc.net <- graph_from_incidence_matrix(inc.mat)
    
    v.col <- rep(vertex.col, vcount(inc.net))
    if(!is.null(label.vertex)){
        if(search.name == "full"){
            trait.idx <- sapply(label.vertex, function(x) which(V(inc.net)$name == x))
        }else{
            trait.idx <- sapply(label.vertex, function(x) grep(x, V(inc.net)$name))
        }
        v.col[trait.idx] <- label.col
        V(inc.net)$color <- v.col
    }

    proj <- bipartite_projection(inc.net)
    result <- list("Network" = inc.net, "Projections" = proj)
    return(result)
}
