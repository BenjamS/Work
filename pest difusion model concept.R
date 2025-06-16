library(tidyverse)
library(Matrix)
x<-Matrix(rnorm(9),3)
X<-forceSymmetric(x)

distMax <- 350; distMin <- 3;
nYrs <- 20
nMos <- 10
n <- 500
#M <- matrix(runif(n * n), n, n)
M <- matrix(rlnorm(n * n, 0.1, 0.5), n, n)
matDist <- tcrossprod(M, M)  #M %*% t(M)
hist(log(matDist))
min(matDist)
diag(matDist) <- 0
matProbInfecTransp <- exp(-0.0001 * matDist) #1 / matDist^0.4
hist(matProbInfecTransp)
diag(matProbInfecTransp) <- 1
#colnames(matProbInfecTransp) <- paste0("pix", c(1:n))
#gathercols <- colnames(matProbInfecTransp)
#dfAdjacency <- matProbInfecTransp %>% as.data.frame() %>% gather_("pix_i", "prob", gathercols)
#dfAdjacency$pix_j <- paste0("pix", rep(1:n, n)); dfAdjacency <- dfAdjacency %>% relocate(pix_j, .after = pix_i)
#max(dfAdjacency$prob)
indPatient0 <- sample(1:n, 1, replace = FALSE, prob = NULL)
indInfctd <- indPatient0
pixInfctdVec <- rep(0, n); pixInfctdVec[indInfctd] <- 1
nPixInfctd <- c()
for(y in 1:nYrs){
  for(m in 1:nMos){
  }
  D <- diag(pixInfctdVec); Dc <- diag(1 - pixInfctdVec)
  matProbSuscept <- D %*% matProbInfecTransp %*% Dc
  matRandSuscept <- matProbSuscept > 0
  nRand <- sum(matRandSuscept)
  matRandSuscept[matRandSuscept == 1] <- runif(nRand) #exp(rnorm(nRand))
  matNewInfctd <- matRandSuscept < matProbSuscept
  indNewInfctd <- which(colSums(matNewInfctd) > 0)
  if(length(indNewInfctd)){pixInfctdVec[indNewInfctd] <- 1}
  nPixInfctd[y] <- sum(pixInfctdVec)
}

ggplot(data.frame(y = 1:nYrs, nPixInfctd), aes(y, nPixInfctd)) + geom_line()
