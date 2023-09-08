#This function ranks elements of a matrix based on two
#dimensions. The rank of each element is based on how many 
#elements are contained in the square determined by the 
#element's x and y coordinates. If many elements are in this
#square, the element is highly ranked. If only a few elements
#are in this square, the element is ranked low.
#plot(mat[,1], mat[,2], type = "n");text(mat[,1], mat[,2], labels = rownames(mat))

rank.2D <- function(mat, return.ranks = TRUE){

    get.rank <- function(idx){
        if(any(is.na(mat[idx,]))){return(NA)}
        below.x <- which(mat[,1] < mat[idx,1])
        below.y <- which(mat[,2] < mat[idx,2])
        below.both <- length(intersect(below.x, below.y))
        return(below.both)
    }

    rank.mat <- matrix(sapply(1:nrow(mat), get.rank), ncol = 1)
    rownames(rank.mat) <- rownames(mat)
    colnames(rank.mat) <- "rank"
    ordered.rank <- rank.mat[order(rank.mat[,1], decreasing = TRUE),,drop=FALSE]
 
    if(!return.ranks){
        return(rownames(ordered.rank))
    }else{

        u_scores <- sort(unique(rank.mat[,1]), decreasing = TRUE)
        u_ranks <- 1:length(u_scores)
        new.ranks <- matrix(NA, nrow = nrow(rank.mat), ncol = 1)
        for(i in 1:length(u_scores)){
            if(is.na(u_scores[i])){
                new.ranks[i] <- NA
            }else{
                score.locale <- which(rank.mat[,1] == u_scores[i])
                new.ranks[score.locale,1] <- u_ranks[i]
            }
        }
        rownames(new.ranks) <- rownames(rank.mat)
        #sorted.ranks <- new.ranks[order(new.ranks[,1], decreasing = FALSE),]
        #head(sorted.ranks)
        return(new.ranks)
    }
}