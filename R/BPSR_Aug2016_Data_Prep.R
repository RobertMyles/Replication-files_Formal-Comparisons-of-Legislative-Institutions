# Data prep for BPSR research note on the comparability of the Senate and Chamber of Deputies in Brazil.
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

m1 <- mdb.get("BancoPublico.mdb")

cam <- m1$tbl_DepF_X_Votacao %>% 
  dplyr::select(Id.Votacao, Id.Dep, Voto, Sigla.Partido, Coalizao.GOV) %>% 
  mutate(Voto = as.character(Voto)) %>% 
  mutate(Voto = ifelse(Voto == "S", 1, ifelse(Voto == "N", 0, NA))) %>% 
  mutate(Voto = as.numeric(Voto)) %>% 
  mutate(Id.Dep = as.numeric(Id.Dep)) %>% 
  dplyr::rename(ID = Id.Dep)

cam.votes <- m1$tbl_DepF_Votacao %>% 
  dplyr::select(Id.Votacao, Data, Casa.C, Projeto, PROJ.ANO) %>% 
  mutate(Data = unlist(str_extract_all(Data, "[0-9/]{8}"))) %>% 
  mutate(Data = mdy(Data)) %>% 
  dplyr::rename(date = Data)

deps <- m1$tbl_DepF %>% 
  dplyr::select(Id.Dep, Nome.Dep) %>% 
  mutate(Nome.Dep = as.character(Nome.Dep)) %>% 
  mutate(Id.Dep = as.numeric(Id.Dep)) %>% 
  dplyr::rename(ID = Id.Dep, name = Nome.Dep) %>% 
  distinct(ID, .keep_all=T)

camara <- full_join(cam, cam.votes)
camara <- left_join(camara, deps) %>% 
  dplyr::select(-Id.Votacao) %>% 
  mutate(house = "Chamber")


sen <- m1$tbl_Sen_X_Votacao %>% 
  dplyr::select(Id.Votacao, Id.Sen, Voto, Sigla.Partido, Coalizao.GOV) %>% 
  mutate(Voto = as.character(Voto)) %>% 
  mutate(Voto = ifelse(Voto == "S", 1, ifelse(Voto == "N", 0, NA))) %>% 
  mutate(Voto = as.numeric(Voto)) %>% 
  mutate(Id.Sen = as.numeric(Id.Sen)) %>% 
  dplyr::rename(ID = Id.Sen)

sens <- m1$tbl_Sen %>% 
  dplyr::select(Id.Sen, Nome.Sen) %>% 
  mutate(Nome.Sen = as.character(Nome.Sen)) %>% 
  mutate(Id.Sen = as.numeric(Id.Sen)) %>% 
  dplyr::rename(ID = Id.Sen, name = Nome.Sen) %>% 
  distinct(ID, .keep_all=T)
  
sen.votes <- m1$tbl_Sen_Votacao %>% 
  dplyr::select(Id.Votacao, Data, Projeto, NumSenado) %>% 
  mutate(Data = unlist(str_extract_all(Data, "[0-9/]{8}"))) %>% 
  mutate(Data = mdy(Data)) %>% 
  dplyr::rename(date = Data, PROJ.ANO = NumSenado)

senate <- full_join(sen, sen.votes)
senate <- left_join(senate, sens) %>% 
  dplyr::select(-Id.Votacao) %>% 
  mutate(house = "Senate")

votes <- full_join(camara, senate)

save(votes, file="votes.Rda")
