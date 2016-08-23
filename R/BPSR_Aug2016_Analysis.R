# Data analysis for BPSR research note on the comparability of the Senate and
# Chamber of Deputies in Brazil.
rm(list=ls())
library(MCMCpack)
library(dplyr)
library(BEST)

load("./data/votes.Rda")

# split by legislature:

votes48 <- votes %>% 
  filter(legislature == 48) %>% 
  mutate(constraint1 = unique(.$ID[grep("JAMIL HADDAD", .$name)])) %>% 
  mutate(constraint2 = unique(.$ID[grep("MARCO MACIEL", .$name)]))
votes49 <- votes %>% 
  filter(legislature == 49) %>% 
  mutate(constraint1 = unique(.$ID[grep("EDUARDO SUPLICY", .$name)])) %>% 
  mutate(constraint2 = unique(.$ID[grep("MARCO MACIEL", .$name)]))
votes50 <- votes %>% 
  filter(legislature == 50) %>% 
  mutate(constraint1 = unique(.$ID[grep("EDUARDO SUPLICY", .$name)])) %>% 
  mutate(constraint2 = unique(.$ID[grep("JOSE AGRIPINO", .$name)]))
votes51 <- votes %>% 
  filter(legislature == 51) %>% 
  mutate(constraint1 = unique(.$ID[grep("EDUARDO SUPLICY", .$name)])) %>% 
  mutate(constraint2 = unique(.$ID[grep("JOSE AGRIPINO", .$name)]))
votes52 <- votes %>% 
  filter(legislature == 52) %>% 
  mutate(constraint1 = unique(.$ID[grep("EDUARDO SUPLICY", .$name)])) %>% 
  mutate(constraint2 = unique(.$ID[grep("JOSE AGRIPINO", .$name)]))
votes53 <- votes %>% 
  filter(legislature == 53) %>% 
  mutate(constraint1 = unique(.$ID[grep("EDUARDO SUPLICY", .$name)])) %>% 
  mutate(constraint2 = unique(.$ID[grep("JOSE AGRIPINO", .$name)]))

# the following function takes a subset of the votes (by legislature) as input,
# creates a matrix of rollcall votes, runs this through MCMCpack's 
# MCMCirt1d function, summarises the mcmc object, extracts the ideal points
# and joins this to the data on the legislators, and then returns this object.

matrix_votes <- function(votes){
  legisId <- votes$ID
  voteId <- votes$PROJ.ANO
  vote <- votes$Voto
  nameID <- unique(legisId)
  n <- length(unique(nameID))
  m <- length(unique(voteId))
  constraint1 <- unique(votes$constraint1)
  constraint2 <- unique(votes$constraint2)

  rollCallMatrix <- matrix(NA, n, m)

  name_row <- match(legisId, nameID)
  name_col <- unique(voteId)
  name_col <- match(voteId, name_col)

  for(k in 1:length(legisId)){
    rollCallMatrix[name_row[k], name_col[k]] <- vote[k]
  }

  rollCallMatrix <- matrix(as.numeric(unlist(rollCallMatrix)), nrow=nrow(rollCallMatrix))
  dimnames(rollCallMatrix) <- list(unique(nameID), unique(voteId))
  
  mcmc <- MCMCirt1d(rollCallMatrix, theta.constraints = list(constraint1 = "+",
                                                             constraint2 = "-"),
                    burnin = 100, mcmc = 200, thin = 10, verbose = 10)
  smc <- summary(mcmc)
  theta <- smc$statistics[grep("theta", row.names(smc$statistics)),1]
  thetaQ <- smc$quantiles[grep("theta", row.names(smc$statistics)),c(1,5)]
  theta <- as.data.frame(cbind(theta, thetaQ))
  colnames(theta)[1:3] <- c("Mean", "Lower", "Upper")
  row.names(theta) <- NULL
  theta <- theta %>% 
    mutate(ID = as.numeric(row.names(rollCallMatrix))) %>% 
    arrange(Mean)
  
  legis <- votes %>% 
    dplyr::select(ID, Sigla.Partido, name, house) %>% 
    distinct()
  
  ideals <- full_join(legis, theta)
  
  return(ideals)
}

# run mcmc and get ideal points:
m48 <- matrix_votes(votes48)
m49 <- matrix_votes(votes49)
m50 <- matrix_votes(votes50)
m51 <- matrix_votes(votes51)
m52 <- matrix_votes(votes52)
m53 <- matrix_votes(votes53)

# the following code compares the ideal points of the Chamber and the Senate, 
# and plots the result and prints summaries, for each legislature.

c48 <- filter(m48, house == "Chamber")
s48 <- filter(m48, house =="Senate")
best48 <- BESTmcmc(y1=c48$Mean, y2 =s48$Mean, verbose=TRUE, rnd.seed=123, parallel=TRUE)
summary(best48)
plot(best48, which = "mean")
plotAll(best48)


c49 <- filter(m49, house == "Chamber")
s49 <- filter(m49, house =="Senate")
best49 <- BESTmcmc(y1=c49$Mean, y2 =s49$Mean, verbose=TRUE, rnd.seed=123, parallel=TRUE)
plot(best49, which = "mean")


c50 <- filter(m50, house == "Chamber")
s50 <- filter(m50, house =="Senate")
best50 <- BESTmcmc(y1=c50$Mean, y2 =s50$Mean, verbose=TRUE, rnd.seed=123, parallel=TRUE)
plot(best50, which = "mean")


c51 <- filter(m51, house == "Chamber")
s51 <- filter(m51, house =="Senate")
best51 <- BESTmcmc(y1=c51$Mean, y2 =s51$Mean, verbose=TRUE, rnd.seed=123, parallel=TRUE)
plot(best51, which = "mean")


c52 <- filter(m52, house == "Chamber")
s52 <- filter(m52, house =="Senate")
best52 <- BESTmcmc(y1=c52$Mean, y2 =s52$Mean, verbose=TRUE, rnd.seed=123, parallel=TRUE)
plot(best52, which = "mean")


c53 <- filter(m53, house == "Chamber")
s53 <- filter(m53, house =="Senate")
best53 <- BESTmcmc(y1=c53$Mean, y2 =s53$Mean, verbose=TRUE, rnd.seed=123, parallel=TRUE)
plot(best53, which = "mean")

