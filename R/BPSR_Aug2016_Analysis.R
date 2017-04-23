# Data analysis for BPSR research note on the comparability of the Senate and
# Chamber of Deputies in Brazil.
rm(list=ls())
library(MCMCpack)
library(BEST)
library(tidyverse)
library(devtools)
library(showtext)

# add Cambria font to figures:
font.paths()
font.add(family = "Cambria", regular = "Library/Fonts/Microsoft/Cambria.ttf")
# these scripts modify the BEST package functions in order to use Cambria:
source_gist("5288f15c217c402057ec4e6bd74ed9b1", 
            filename = "plotPost_2.R", 
            sha1 = "62bcb0b2fa612c213140a02f07510c7b3f6514e0")
source_gist("9187862808d6ad1cbd243bf282196eb2", 
            filename = "plot.BEST_2.R", 
            sha1 = "4be58e22004b3ede5072bfeedf8b7d1b3fb75de7")



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
# This may be skipped by loading the data below.

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

  rollCallMatrix <- matrix(as.numeric(unlist(rollCallMatrix)), 
                           nrow=nrow(rollCallMatrix))
  dimnames(rollCallMatrix) <- list(unique(nameID), unique(voteId))
  
  mcmc <- MCMCirt1d(rollCallMatrix, theta.constraints = list(constraint1 = "+",
                                                             constraint2 = "-"),
                    burnin = 10000, mcmc = 100000, thin = 10, verbose = 10000)
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

save(m48, file="data/estimates_48.Rda")
save(m49, file="data/estimates_49.Rda")
save(m50, file="data/estimates_50.Rda")
save(m51, file="data/estimates_51.Rda")
save(m52, file="data/estimates_52.Rda")
save(m53, file="data/estimates_53.Rda")

load("data/estimates_48.Rda")
load("data/estimates_49.Rda")
load("data/estimates_50.Rda")
load("data/estimates_51.Rda")
load("data/estimates_52.Rda")
load("data/estimates_53.Rda")


# the following code compares the ideal points of the Chamber and the Senate, 
# and plots the result and prints summaries, for each legislature (Bayesian t-test).

best <- function(legis, period){
  c <- filter(legis, house == "Chamber")
  s <- filter(legis, house =="Senate")
  b <- BESTmcmc(y1 = c$Mean, y2 = s$Mean, verbose=TRUE, 
                rnd.seed=123, parallel=TRUE)
  Save <- paste0("data/best", deparse(substitute(legis)), ".Rda")
  save(b, file=Save)
  Legis <- gsub("m", "", deparse(substitute(legis)))
  Legis2 <- gsub("m", "\n", deparse(substitute(legis)))
  png(filename = paste0("images/best", Legis, ".png"), width = 400,
  height = 400, pointsize=15)
  plot.BEST_2(b, which = "mean", main=paste0("Difference of Means ", period))
  dev.off()
  png(filename = paste0("images/best", Legis, "_effect.png"), width = 400,
      height = 400, pointsize=15)
  plot.BEST_2(b, which = "effect", main=paste0("Effect Size ", period),
       family = "Cambria")
  dev.off()
  png(filename = paste0("images/best", Legis, "_sd.png"), width = 400,
      height = 400, pointsize=15, type = "quartz", family = "Cambria")
  plot.BEST_2(b, which = "sd", main=paste0("Difference in Std. Dev. ", period),
       family = "Cambria")
  dev.off()
}

best(m48, "48th")
best(m49, "49th")
best(m50, "50th") 
best(m51, "51st") 
best(m52, "52nd") 
best(m53, "53rd") 

setwd("images/")
# stitch plots together and make black & white:
# figure 1: 
system2("convert",  args = c("best48.png", "best49.png", 
                             "best50.png", "+append", "means_1.png"))
system2("convert",  args = c("best51.png", "best52.png", 
                             "best53.png", "+append", "means_2.png"))
system2("convert",  args = c("means_1.png", "means_2.png", 
                             "-append", "figure_1.png"))
system2("convert", args = c("-type", "Grayscale", "figure_1.png", "figure_1g.png"))

# figure 2: 
system2("convert",  args = c("best48_effect.png", "best49_effect.png", 
                             "best50_effect.png", "+append", "effects_1.png"))
system2("convert",  args = c("best51_effect.png", "best52_effect.png", 
                             "best53_effect.png", "+append", "effects_2.png"))
system2("convert",  args = c("effects_1.png", "effects_2.png", 
                             "-append", "figure_2.png"))
system2("convert", args = c("-type", "Grayscale", "figure_2.png", "figure_2g.png"))


system2("rm", args = c("effects_1.png", "effects_2.png", "means_1.png", "means_2.png"))
setwd("../")

# Fully detailed plots:
# all_plot <- function(){
#   legis <- c("m48", "m49", "m50", "m51", "m52", "m53")
#   for(l in legis){
#     load(paste0("data/best", l, ".Rda"))
#     png(filename = paste0("images/plot_all_", l, ".png"),
#         type = "quartz", family = "Cambria")
#     plotAll(b)
#     dev.off()
#   }
# }
# all_plot()

# Bayesian t-tests for parties:
party <- function(legis){
  siglas <- c("PMDB", "PFL", "PSDB", "PT")
  for(s in seq_along(siglas)){
    partido_c <- filter(legis, Sigla.Partido==siglas[s], house=="Chamber")
    partido_s <- filter(legis, Sigla.Partido==siglas[s], house=="Senate")
    p_best <- BESTmcmc(y1 = partido_c$Mean, y2 = partido_s$Mean, 
                       verbose=TRUE, rnd.seed=123, parallel=TRUE)
    Save <- paste0("data/partybest", siglas[s], 
                   deparse(substitute(legis)), ".Rda")
    save(p_best, file=Save)
  }
}

party(m48)
party(m49)
party(m50) 
party(m51) 
party(m52) 
party(m53) 


# plot effect sizes over time

effects <- function(){
  Y <- data_frame(mean=NA, HDIlo=NA, HDIup=NA, item=NA, party=NA, legis=NA)
  legis <- c("m48", "m49", "m50", "m51", "m52", "m53")
  parties <- c("PMDB", "PFL", "PSDB", "PT")
  for(l in legis){
    for(p in parties){
      load(paste0("data/partybest", p, l, ".Rda"))
      X <- as_data_frame(summary(p_best))[c(1,2,9), c(1, 5:6)] %>% 
        mutate(item = c("Chamber", "Senate", "Effect"),
               party = p, legis = l, 
               legis = gsub("m", "", legis))
      Y <- rbind(Y, X) %>% 
        filter(!is.na(mean))
    }
  }
  return(Y)
}


Effects <- effects() %>% 
  filter(item=="Effect")
Effects[Effects$party=="PT" & Effects$legis %in% c("48", "49", "50", "51", "52"),] <- 0
Effects <- Effects %>% 
  filter(item != 0) %>% 
  arrange(legis, party, item)

png(file="images/Figure_3.png", width=650, height=400)  
ggplot(Effects, aes(y = mean, x = legis)) +
  geom_hline(yintercept = 0, linetype=8) +
  geom_point(size = 1.5) +
  geom_line(aes(group = party, linetype = party)) +
  theme_bw() +
  theme(legend.position="none", 
        axis.text = element_text(family = "Cambria"),
        axis.title = element_text(family = "Cambria")) +
  ylab("Effect Size") +
  xlab("Legislature") +
  annotate("label", x=1.3, y=2, label="PFL", size = 5, family = "Cambria") + 
  annotate("label", x=1.7, y=-1.65, label="PSDB", size = 5, family = "Cambria") + 
  annotate("label", x=0.75, y=1.15, label="PMDB", size = 5, family = "Cambria") + 
  annotate("label", x=5.9, y=-1.65, label="PT", size = 5, family = "Cambria") 
dev.off()



# Cohesion test with SDs:
SDs <- function(){
  Y <- data_frame(mode=NA, HDIlo=NA, HDIup=NA, 
                  item=c("Chamber", "Senate"), party=NA, legis=NA)
  legis <- c("m48", "m49", "m50", "m51", "m52", "m53")
  parties <- c("PMDB", "PFL", "PSDB", "PT")
  for(l in legis){
    for(p in parties){
      load(paste0("data/partybest", p, l, ".Rda"))
      X <- as_data_frame(summary(p_best))[c(4:5), c(3, 5:6)] %>% 
        mutate(item=c("Chamber", "Senate"),
          party=p, legis=l, 
               legis = gsub("m", "", legis))
      Y <- rbind(Y, X) %>% 
        filter(!is.na(mode))
    }
  }
  return(Y)
}
sd <- SDs()

sd[sd$party=="PT" & sd$legis %in% c("48", "49", "50", "51", "52"),] <- 0

sd <- sd %>% 
  filter(party != 0) %>% 
  arrange(legis, party) %>% 
  mutate(diff = HDIup - HDIlo) %>%
  dplyr::select(-c(mode, HDIlo, HDIup)) %>% 
  spread(item, diff) %>% 
  mutate(test = if_else(Chamber > Senate, 1, 0))

write.csv(sd, "data/Table_1.csv")
