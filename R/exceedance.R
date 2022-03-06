noise <- runif(1E6,min=-1,max=1)
signal <- sin(seq(from=0,to=10,length.out=1E6))*seq(from=5,to=10,length.out=1E6)

x <- signal + noise

peaks   <- rep(NA,length(x))
valleys <- rep(NA,length(x))

for (ii in 1:length(x)) {

    if (ii == 1) {
        
        if (x[ii+1] > x[ii]) {
            
            valleys[ii] <- ii
            
        } else {
            
            peaks[ii] <- ii
            
        }
        
    } else if (ii == length(x)) {
        
        if (x[ii] > x[ii-1]) {
            
            peaks[ii] <- ii
            
        } else {
            
            valleys[ii] <- ii
            
        }
        
    } else {
        
       if (x[ii] > x[ii-1] && x[ii] > x[ii+1]) {
           
           peaks[ii] <- ii
           
        }
        else if  (x[ii] < x[ii-1] && x[ii] < x[ii+1]) {
           
           valleys[ii] <- ii
           
        }
        
    }

}

peaks <- x[peaks[!is.na(peaks)]]
valleys <- x[valleys[!is.na(valleys)]]

peak_test <- function(limit, values) {
    
    sum(values > limit)
    
}

valley_test <- function(limit, values) {
    
    sum(values < limit)
    
}

limits <- seq(-10,10,length.out = 25)

exceedanceValleys <- data.frame(limits, rep("valleys",length(limits)), sapply(limits, FUN = valley_test, valleys))
colnames(exceedanceValleys) <- c("limits","type","count")
exceedancePeaks <- data.frame(limits, rep("peaks",length(limits)), sapply(limits, FUN = peak_test, peaks))
colnames(exceedancePeaks) <- c("limits","type","count")
exceedance<-rbind(exceedanceValleys,exceedancePeaks)

g <- ggplot(exceedance, aes(x=limits, y = count, color = type)) + geom_line() + geom_point()

plot(g)
