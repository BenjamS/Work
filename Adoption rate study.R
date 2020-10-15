library(ggplot2)
library(rootSolve)
#---------------------------------------------------------------------------
# Define functions
# Function to get lambda at which farmer indifferent between new tech and status quo
get_lamda_break <- function(environ){
  P0 <- environ[["P0"]]
  P1 <- environ[["P1"]]
  b0 <- environ[["b0"]]
  b1 <- environ[["b1"]]
  h0 <- environ[["h0"]]
  h1 <- environ[["h1"]]
  
  term1 <- ((1 - h0) / (1 - h1))^((1 - h0) * (1 - h1))
  term2 <- exp(b0 * (1 - h1) - b1 * (1 - h0))
  term3 <- P0^(1 - h1) * P1^(1 - h0)
  lambda_break <- as.numeric((term1 * term2 * term3)^(1 / (h0 - h1)))
  return(lambda_break)
}

#----------------------------------------------------------------------------
# Supply curve

QS_at_P <- function(P, environ){
  b <- environ[["b"]]
  h <- environ[["h"]]
  bounds <- environ[["bounds"]]
  mu <- environ[["mu"]]
  sig <- environ[["sig"]]
  N <- environ[["N"]]

  cv <- sig / mu
  s <- sqrt(log(cv^2 + 1))
  m <- log(mu) - 1 / 2 * s^2
  
  if(is.null(bounds)){bounds <- c(0, h^(-h / (1 - h)))}
  
  Phi_upper <- plnorm(bounds[2], m, s)
  Phi_lower <- plnorm(bounds[1], m, s)
  mkt_particip <- N * (Phi_upper - Phi_lower)
  Supply <- exp(b / (1 - h)) * P^(h / (1 - h)) * mkt_particip
  df_out <- data.frame(P, Supply, mkt_particip)
  return(df_out)
}
#=========================================================================
# Initiate parameters
yint0 <- -3.3
yint1 <- yint0
a0 <- c(.15, .2, .3)
a1 <- c(.15, .25, .3)
b0 <- as.numeric(t(a0) %*% log(a0) + yint0)
b1 <- as.numeric(t(a1) %*% log(a1) + yint1)
h0 <- as.numeric(t(a0) %*% rep(1, length(a0)))
h1 <- as.numeric(t(a1) %*% rep(1, length(a1)))
P0 <- 2500
P1 <- P0
trans_cost <- 50000
#--------------------------------------------------------------------------
# generate random yield data
mu_y <- 22
cv_y <- 0.2
sig_y <- mu_y * cv_y
s_y <- sqrt(log(cv_y^2 + 1))
m_y <- log(mu_y) - 1 / 2 * s_y^2
y <- rlnorm(100, m_y, s_y)
hist(y)
#---
lambda <- (exp(b0 / (1 - h0)) * P0^(h0 / (1 - h0)) / y)^((1 - h0) / h0)
RCratio <- lambda / h0
hist(RCratio)
hist(lambda)
#y0 <- exp(b0 / (1 - h0)) * (P0 / lambda)^(h0 / (1 - h0))
k <- exp(b0 / (1 - h0)) * P0^(h0 / (1 - h0))
mu_u <- mu_y / k # u = (1/lambda)^(h/(1-h))
sig_u <- sig_y / k
cv_u <- sig_u / mu_u
s_u <- sqrt(log(cv_y^2 + 1))
m_u <- log(mu_u) - 1 / 2 * s_u^2

u <- lambda^(-h0 / (1 - h0))
Density <- dlnorm(u, m_u, s_u)
ulim <- h0^(-h0 / (1 - h0))
df_plot <- data.frame(u, Density)
gg <- ggplot(df_plot, aes(u, Density))
gg <- gg + geom_line(lwd = 1.3)
gg <- gg + geom_vline(xintercept = ulim, color = "red")
gg


Density <- dlnorm(y, m_y, s_y)
ylim <- k * ulim
df_plot <- data.frame(y, Density)
gg <- ggplot(df_plot, aes(y, Density))
gg <- gg + geom_line(lwd = 1.3)
gg <- gg + geom_vline(xintercept = ylim, color = "red")
gg

#plnorm(ylim, m_y, s_y)

N <- 90
environ <- list()
environ[["b"]] <- b0
environ[["h"]] <- h0
environ[["bounds"]] <- NULL
environ[["mu"]] <- mu_u
environ[["sig"]] <- sig_u
environ[["N"]] <- N
Pvec <- seq(500, 4000, length.out = 40)

df_plot <- QS_at_P(Pvec, environ)
#df_plot <- as.data.frame(do.call(rbind, purrr::map(Pvec, QS_at_P, environ)))

gg <- ggplot(df_plot, aes(P, Supply))
gg <- gg + geom_line(lwd = 1.3)
gg














this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/Tegemeo Data/OSU Work 230713/"
this_file <- "Kenya 2007 Clean 03-08-13.csv"
this_filepath <- paste0(this_folder, this_file)
df <- read.csv(this_filepath, stringsAsFactors = F)
#colnames(df)
# df$educ <- exp(df$leduc)
# df$drainage <- exp(df$ldrainage)
keep_these <- c("yieldmaiz", "fertactot_F",
                "ageavg",
                "labfammale_qtyM_F",
                #"labfamfem_qtyM_F",
                #"hybmz_F",
                "pkgftot_vil",
                "qwetxt",
                "qwetpre")
df_mod <- df[, keep_these]
log_these <- setdiff(keep_these, c("hybmz_F", "gend"))
df_mod[, log_these] <- as.data.frame(apply(df_mod[, log_these], 2, log))
# df_mod[, -1] <- as.data.frame(apply(df_mod[, -1], 2, function(x) 1 / x))
# df_mod$yieldmaiz <- log(df_mod$yieldmaiz)
for(i in 1:ncol(df_mod)){
  df_mod[which(is.infinite(df_mod[, i])), i] <- NA
}
mod <- lm(yieldmaiz~., df_mod, na.action = na.omit)
summary(mod)
#plot(mod$fitted.values, mod$residuals)

a <- coefficients(mod)
sum(a[c("fertactot_F", "labfammale_qtyM_F")])
sum(a[-1])
