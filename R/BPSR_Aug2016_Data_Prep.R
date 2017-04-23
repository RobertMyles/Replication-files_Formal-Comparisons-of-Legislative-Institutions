# Data prep for BPSR research note on the comparability of the Senate and Chamber of Deputies in Brazil.
rm(list=ls())
library(tidyverse)
library(bRasilLegis)
library(stringr)
library(stringi)
library(Hmisc)
library(lubridate)

# You will need access to the CEBRAP database to replicate the script below,
# it is too large for me to host on Github (it is available from the Brazilian
# Political Science Review data replication repository for this paper). The script 
# below shows how the
# data were prepared. For a fully replicable script, see
# 'BPSR_Aug2016_Analysis.R'.




#####   UPDATE: 

# these data are no longer available from the Harvard Dataverse. For an 
# alternative, see the "congressbr" package for R, which has all this data 
# (more, actually) in the "nominal-votes" dataset. 
# As a result, this script is NO LONGER REPLICABLE.
# See https://github.com/RobertMyles/congressbr for more details on congressbr.




#############################################################

# Import CEBRAP data:
m1 <- mdb.get("data/BancoPublico.mdb")

# the data are prepared so that the variable PROJ.ANO can be used to merge
# the Senate and Chamber data.


# prepare Camara data:
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


# prepare Senado data:
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


legislature <- m1$tbl_Legislatura %>%
  dplyr::select(-c(5:6)) %>%
  mutate(DataInicio = unlist(str_extract_all(DataInicio, "[0-9/]{8}"))) %>%
  mutate(DataTermino = unlist(str_extract_all(DataTermino, "[0-9/]{8}"))) %>%
  mutate(DataInicio = mdy(DataInicio), DataTermino = mdy(DataTermino))

votes <- full_join(camara, senate)

head(legislature)
votes <- votes %>%
  mutate(legislature = ifelse(date > "1987-02-01"  &
                                date < "1991-01-31", 48,
                              ifelse(date > "1991-02-01" &
                                       date < "1995-01-31", 49,
                                     ifelse(date > "1995-02-01" &
                                              date < "1999-01-31", 50,
                                            ifelse(date > "1999-02-01" &
                                                     date < "2003-01-31", 51,
                                                   ifelse(date > "2003-02-01" &
                                                            date < "2007-01-31",
                                                          52, ifelse(
                                                            date > "2007-02-01"
                                                            & date < "2011-01-31",
                                                            53, 0))))))) %>%
  dplyr::filter(legislature != 0)


save(votes, file="data/votes.Rda")
rm(m1, sen, sens, sen.votes, legislature, deps, camara, cam.votes, cam, senate)

## Descriptive Statistics
load("data/votes.Rda")

desc <- votes %>%
  select(PROJ.ANO, house, date) %>%
  distinct(house, PROJ.ANO, .keep_all=T) %>%
  filter(nchar(PROJ.ANO, allowNA=T) > 4) %>%
  group_by(PROJ.ANO) %>%
  mutate(freq=n()) %>%
  filter(freq==2) %>%
  select(-freq) %>%
  spread(house, date) %>%
  arrange(Chamber) %>%
  mutate(project = gsub(PROJ.ANO, pattern="[0-9/]",
                        replacement="", perl=T, PROJ.ANO)) %>%
  filter(project == "MPV") %>%
  select(-project) %>%
  rename(`Chamber Date` = Chamber, `Senate Date` = Senate,
         `Project Year` = PROJ.ANO)

write_csv(desc, path="data/desc.csv")
