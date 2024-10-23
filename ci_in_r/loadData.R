income <- read.csv(file = "https://juandmontoro.github.io/stat2/ci_in_r/data.csv")
income <- income$x

ci <- function(sampleData,confidenceLevel=0.95,populationSD){
  smean=mean(sampleData)
  size=length(sampleData)
  reliability = qnorm((1-confidenceLevel)/2, lower.tail = F)
  lcl=round(smean-reliability*populationSD/sqrt(size),1)
  ucl=round(smean +reliability*populationSD/sqrt(size),1)
  return(c(lcl,ucl))
}

plotCI <- function(mydf){ 
with(mydf, {
  plot(1:nrow(mydf), LCL, ylim=range(c(LCL, UCL)), type="n", xlab="Sample #", ylab="Confidence interval")
  segments(1:nrow(mydf), LCL, 1:nrow(intervals), UCL, col=ifelse(contains == "YES", "green", "red"), lwd=2)
  points(1:nrow(mydf), LCL, pch=16, col=ifelse(contains == "YES", "green", "red"))
  points(1:nrow(mydf), UCL, pch=16, col=ifelse(contains == "YES", "green", "red"))
  abline(h=1500, col="blue", lty=2)  
})
legend("bottomleft", legend="Population mean", col="blue", lty=2, bty="n")
}