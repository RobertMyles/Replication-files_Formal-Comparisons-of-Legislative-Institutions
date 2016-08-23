# Data analysis for BPSR research note on the comparability of the Senate and Chamber of Deputies in Brazil.
rm(list=ls())
library(dplyr)
library(readr)
library(bRasilLegis)
library(devtools)
library(stringr)
library(stringi)
library(Hmisc)
library(lubridate)

# Import CEBRAP data:


source_gist(id = "https://gist.github.com/RobertMyles/c1e1f254f687a15419530845e6449601", filename = "voteMatrix.R", sha1 = "ad823f1578eb2bab6c844ce07454fc03d4261364")  
source_gist(id = "https://gist.github.com/RobertMyles/7348c6bdfb47b26a7e6a49f73e5d4c86",  filename = "voteToRollcall.R", sha1 = "1d5a70917d23afca9ccc259d49a0ced3ebb33576")

camara <- voteToRollcall(1995:2010, legis.data = T)
cam.data <- camara$legis.data
camara <- camara$votes
listarProposicoesVotadasEmPlenario("PL", "1995")
cam.mpv <- camara[, grep("MPV", colnames(camara))]

