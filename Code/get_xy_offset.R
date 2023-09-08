#This function calculates an x an y position near
#a given point so that an arrow can begin or end
#with an offset.
#The function takes in starting and ending x and
#y point. It generates a circle around each x1,y1
#point based on the provided offset. It returns the 
#point at which the line from x0,y0 intersects the 
#circle around x1,y1.
#the size of the offset is a percentage of the total
#size of the plot. density specifies the density of the
#circle to use for calculating the intersecting point.
#x0 = 1:10; y0 = 1:10; x1 = 2:11; y1 = 2:11; offset = 10

arrow_offset <- function(x0, x1, y0, y1, offset = 1, density = 0.01, plot.results = FALSE){
	
	euclidean <- function(a, b) sqrt(sum((a - b)^2))

	total.x <- max(c(x0,x1), na.rm = TRUE) - min(c(x0,x1), na.rm = TRUE)
	total.y <- max(c(y0,y1), na.rm = TRUE) - min(c(y0,y1), na.rm = TRUE)

	x.offset <- total.x*(offset/100)
	y.offset <- total.y*(offset/100)


	get_new_pt <- function(idx){
		intersecting_circle <- get_circle(radius = x.offset, center_x = x1[idx], 
			center_y = y1[idx], dens = density)

		euclidean(a = c(x0[idx],y0[idx]), b = c(intersecting_circle$x[1], intersecting_circle$y[1]))	

		dist_to_orig <- sapply(1:length(intersecting_circle$x), 
			function(i) euclidean(a = c(x0[idx],y0[idx]), b = c(intersecting_circle$x[i], 
			intersecting_circle$y[i])))
		new.pt.idx <- which.min(dist_to_orig)
		new.pt <- c("x" = intersecting_circle$x[new.pt.idx], "y" = intersecting_circle$y[new.pt.idx])
		return(new.pt)
	}

	#get new ending positions for every entry in x0,y0 and x0,y1
	new.pts <- t(sapply(1:length(x0), function(x) get_new_pt(x)))
	colnames(new.pts) <- c("x1", "y1")

	if(plot.results){
		par(mfrow = c(1,2))
		plot(c(x0,x1), c(y0, y1), main = "original arrow")
		arrows(x0 = x0, x1 = x1, y0 = y0, y1 = y1)
				
		plot(c(x0,x1), c(y0, y1), main = "new arrow")
		arrows(x0 = x0, x1 = new.pts[,1], y0 = y0, y1 = new.pts[,2])
	}

	return(new.pts)
	
}