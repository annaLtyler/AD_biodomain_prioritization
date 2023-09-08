#This function takes a list the same way plotVenn takes
#a list. It calcuates jaccard indices between all pairs
#of elements and returns a jaccard matrix.
#if return.intersections is TRUE, just the number of 
#common elements will be returned.

jaccard.matrix <- function(listX, return.intersections = FALSE, verbose = FALSE){
    group.pairs <- pair.matrix(1:length(listX), FALSE, TRUE)
    j.mat <- matrix(NA, length(listX), length(listX))
    colnames(j.mat) <- rownames(j.mat)  <- names(listX)
    for(i in 1:nrow(group.pairs)){
        if(verbose){report.progress(i, nrow(group.pairs))}
        ind1 <- group.pairs[i,1]
        ind2 <- group.pairs[i,2]
        if(return.intersections){
            pair.j  <- length(intersect(listX[[ind1]], listX[[ind2]]))
        }else{
            pair.j  <- jaccard.ind(listX[[ind1]], listX[[ind2]])        
        }
        j.mat[ind1, ind2] <- pair.j
        j.mat[ind2, ind1] <- pair.j
    }
    return(j.mat)
}