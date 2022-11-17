





























#include <Rcpp.h>
using namespace Rcpp;
Then, right before the function starts, you must write the following comment line to let Rcpp know that this
is the main function:
  // [[Rcpp::export]]
NumericVector dummy(NumericVector x){
  return x;
}


library(Rcpp)
sourceCpp("*.cpp") # replace * with name of file












#crop

library(imager)

compressed.img <- function(x){
  dog <- load.image("dog.jpeg")
  col.mat <- as.array(dog[, , 1,])
  cropped.dog <- col.mat[1:600, 1:600, ]
  
  mat <- array(0, dim = c(x, x, 3))
  
  for(i in seq(1,600,600/x)){
    for (j in seq(1,600,600/x)) {
      for(k in 1:3){
        mat[(i+1)/(600/x),(j+1)/(600/x),k] <- mean(cropped.dog[i:i+(600/x - 1), j:j+(600/x - 1), k])
      }
    }
  }
  return(plot(as.cimg(mat)))
} 
compressed.img(120)






#snowy region

img1 <- load.image("land1.jpeg"); img2 <- load.image("land2.jpeg")

snow_reco <- function(x){
  r <- as.vector(x[,,1,1]); g <- as.vector(x[,,1,2]); b <- as.vector(x[,,1,3])
  mean.x <- cbind(mean(r), mean(g), mean(b))
  colnames(mean.x)  <- c("r", "g", "b")
  if(mean(mean.x) > 0.5)
    print("Snowy region")
  else
    print("Not a snowy region")
}
snow_reco(img1)
snow_reco(img2)



img.90.clockwise <- array(0, dim = c(dim.dog[2], dim.dog[1], 1, 3))
dim.dog <- dim(dog[,,1,])
for(i in 1:dim.dog[2]){
  for(j in 1:dim.dog[1]){
    img.90.clockwise[i, j, 1, ] <- dog[j, dim.dog[2] - i + 1, 1, ]
  }
}



cppFunction('double EucC(NumericVector x, NumericVector y) {
  double track = 0;
  int n = x.size();
  
  for(int i = 0; i < n; i++){
    track = track + pow( (x[i] - y[i]), 2);
  }
  track = sqrt(track);
  return track;
}
')


cppFunction('NumericVector funcC(NumericVector vec){
double track = 0;
int n = vec.size();
NumericVector ans(n);
for(int i = 0; i< n; i++){
track = track + log(vec[i]);
}

for(int i = 0; i < n; i++){
  ans[i] = log(vec[i])/track;
}
return ans;
}
')


# Simpson's paradox here
plot(iris$Sepal.Length, iris$Sepal.Width, 
     xlab = "Sepal Length", ylab = "Sepal Width",
     pch = 16, col = iris$Species)

for(i in 1:length(levels(iris$Species)))
{
  level <- levels(iris$Species)[i]
  foo <- subset(iris, iris$Species == level)
  abline(lm(foo$Sepal.Width ~ foo$Sepal.Length), col = i)
}



hist(anorexia$Prewt, add = TRUE,
     col = adjustcolor("blue", alpha.f = .3))