moveList = read.csv("l_synth.csv")


which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
      if (decreasing){
          if (partial){
              which(diff(c(FALSE,diff(x)>0,TRUE))>0)
          }else {
              which(diff(diff(x)>0)>0)+1
          }
      }else {
          if (partial){
              which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
          }else {
              which(diff(diff(x)>=0)<0)+1
          }
      }
  }

coherence <- 1

coherenceScoreList <- c()

for (move in moveList[,]) {
	if (move == "c") {
		coherence <-  coherence + ( (1 - coherence) / 2 )
	}
	if (move == "d" || move == "i") {
		coherence <-  coherence - ( coherence / 2 )
	}
	if (move == "j") {
		coherence <- 0
	}
	coherenceScoreList <- c(coherenceScoreList, coherence)
}

smoothedCoherenceScoreList <- smooth.spline(1:length(coherenceScoreList),coherenceScoreList,spar=0.35)

write.csv(smoothedCoherenceScoreList$y)

pdf(width=20, height=5)
plot(smoothedCoherenceScoreList, type="l", col="blue")
minima <- which.peaks(smoothedCoherenceScoreList$y, decreasing=TRUE, partial=FALSE)
abline(v=minima)

result <- rle(diff(smoothedCoherenceScoreList$y) > 0)

startIndex <- c()
b <- 1 
for (i in result$lengths) {
	startIndex <- c(startIndex,b)
	b <- b + i
}

write.csv(cbind(startIndex, c(result$lengths),c(result$values)), file = "segments.csv")
