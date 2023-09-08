#This function takes an x and y and plots them with a fitted linear model
#Set confidence to NA to suppress the plotting of the confidence interval.
#xlim = NULL; ylim = NULL; col = "black"; pch = 16; main = ""; xlab = ""; ylab = ""; report = c("lm", "cor.test"); cex = 1; plot.results = TRUE; add = FALSE; plot.type = "p"; confidence = 0.95; confidence.border.color = "blue"; confidence.poly.color = "gray80"

plot.with.model <- function(x, y, xlim = NULL, ylim = NULL, col = "black", 
pch = 16, main, xlab, ylab, report = c("lm", "cor.test"), cex = 1,
plot.results = TRUE, add = FALSE, plot.type = "p", confidence = NA, 
confidence.border.color = "blue", confidence.poly.color = "gray80"){
	
	if(missing(main)){main = ""}
	if(missing(xlab)){xlab = deparse(substitute(x))}
	if(missing(ylab)){ylab = deparse(substitute(y))}

	report <- report[1]

	no.na <- intersect(which(is.finite(y)), which(is.finite(x)))	
	#if(length(no.na) < length(y)){
	#	message("Removing NA's and infinite values.\n")
	#	}

	if(length(no.na) <= 3){
		if(is.na(confidence)){
			return(c("r" = NA, "p" = NA))
		}else{
			return(c("r" = NA, "p" = NA, "slope.fit" = NA, "slope." = NA, "slope" = NA))
		}
	}

	if(is.null(xlim)){xlim <- c(min(x[no.na]), max(x[no.na]))}
	if(is.null(ylim)){ylim <- c(min(y[no.na]), max(y[no.na]))}

	x.df <- data.frame(x[no.na])
	model <- lm(y[no.na]~x.no.na., data = x.df)
	
	if(!is.na(confidence)){
		newx <- data.frame(segment_region(min(x, na.rm = TRUE), max(x, na.rm = TRUE), 100))
		colnames(newx) <- "x.no.na."
		#quartz();plot(newx)
		conf_interval <- predict(model, newdata = newx, 
			interval = "confidence", level = confidence)
		#head(conf_interval)
		conf_min <- conf_interval[1,]
		max.locale <- max(which(!is.na(conf_interval[,1])))
		conf_max <- conf_interval[max.locale,]
		slope.est <- slope(newx[1,1], newx[max.locale,1], conf_min["fit"], conf_max["fit"])
		slope.min <- slope(newx[1,1], newx[max.locale,1], conf_min["upr"], conf_max["lwr"])
		slope.max <- slope(newx[1,1], newx[max.locale,1], conf_min["lwr"], conf_max["upr"])
		

		if(slope.est == 0 || !is.finite(slope.est)){
			if(is.na(confidence)){
				return(c("r" = NA, "p" = NA))
			}else{
				return(c("r" = NA, "p" = NA, "slope" = slope.est, 
					"slope" = slope.min, "slope" = slope.max))
			}
		}
	}

	if(report == "lm"){
		f <- summary(model)$fstatistic
	    p <- pf(f[1],f[2],f[3],lower.tail=F)
		r2 <- signif(summary(model)$r.squared, 2)
		rlab <- "R2"
		}else{
		test <- cor.test(x,y)	
		p <- signif(test$p.value, 2)
		r2 <- signif(test$estimate, 2)
		rlab <- "r"
		}

	if(plot.results){
	new.title <- paste0(main, "\n", rlab, " = ", r2, ", p = ", signif(p, 2))
	if(!add){
		plot.new()
		plot.window(xlim = xlim, ylim = ylim)
	}
	
	#add the points
	points(x,y, col = col, pch = pch, cex = cex, type = plot.type)

	#add comfidence interval first so it doesn't cover up the points
	if(!is.na(confidence)){
		plot.poly.xy(newx[,1], conf_interval[,2], newx[,1], conf_interval[,3], 
		col = confidence.poly.color, border = NA)
		lines(newx[,1], conf_interval[,2], col = confidence.border.color, lty = 2)
		lines(newx[,1], conf_interval[,3], col = confidence.border.color, lty = 2)
	}
	
	#add line of best fit
	abline(model, col = "#a6bddb", lwd = 3)

	#add axes and labels
	axis(1)
	axis(2)
	mtext(side = 1, xlab, line = 2.5)
	mtext(side = 2, ylab, line = 2.5)
	mtext(side = 3, new.title, line = 1.5)

	}


	if(is.na(confidence)){
		invisible(c("r" = r2, "p" = p))
	}else{
		invisible(c("r" = r2, "p" = p, "slope" = slope.est, 
			"slope" = slope.min, "slope" = slope.max))
	}
	
}