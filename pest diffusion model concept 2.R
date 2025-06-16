library(Matrix)
library(ggplot2)

# PARAMETERS
n <- 10000
nYrs <- 20
nMos <- 10

# Create symmetric sparse distance matrix
set.seed(42)
matDistVals <- 10^3 * rlnorm(n * n, -2, 0.5)
matDist <- Matrix(matrix(matDistVals, n, n), sparse = TRUE)
matDist <- forceSymmetric(matDist)
diag(matDist) <- 0  # no self-distances
hist(as.matrix(matDist), 35)

# Convert distances to infection probabilities
matProbInfecTransp <- 1 / matDist^2
diag(matProbInfecTransp) <- 0  # no self-infection
matProbInfecTransp <- drop0(matProbInfecTransp)
hist(as.matrix(matProbInfecTransp), 35)
# Pre-convert to triplet format for indexing
tripletProb <- as(matProbInfecTransp, "dgTMatrix")
edge_i <- tripletProb@i + 1  # rows (infected)
edge_j <- tripletProb@j + 1  # cols (susceptible)
edge_prob <- tripletProb@x   # transmission probability

# Initialize infection state
indPatient0 <- sample(1:n, 1)
pixInfctdVec <- rep(0L, n)
pixInfctdVec[indPatient0] <- 1L
nPixInfctd <- integer(nYrs)

# SIMULATION LOOP
for (y in 1:nYrs) {
  cat("Year", y, "\n")
  for (m in 1:nMos) {
  }
  infected <- pixInfctdVec[edge_i] == 1L
  susceptible <- pixInfctdVec[edge_j] == 0L
  activeEdges <- infected & susceptible
  nPixInfctd[y] <- sum(pixInfctdVec)
  
  if (!any(activeEdges)) next  # skip if no valid edges
  
  randVals <- runif(sum(activeEdges))
  newInf <- randVals < edge_prob[activeEdges]
  newNodes <- edge_j[activeEdges][newInf]
  pixInfctdVec[unique(newNodes)] <- 1L
  
}

# PLOT RESULTS
df <- data.frame(Year = 1:nYrs, nPixInfctd = nPixInfctd)
ggplot(df, aes(Year, nPixInfctd)) + geom_line() +
  labs(title = "Infection Spread Over Time", y = "# Infected Nodes")
