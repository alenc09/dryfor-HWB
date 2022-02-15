library(readxl)
library(tidyverse)


#####
#Básico_geral
Basico_PE <- read_excel("Dados_censo_ibge_2010/data_exc/PE_20171016/PE/Base informaçoes setores2010 universo PE/EXCEL/Basico_PE.xls", 
                         col_types = c("text", "text", "text", "text", "text", "text",
                                            "text", "text", "text", "text", "text", "text", 
                                            "text","text", "text", "text", "text", "text",
                                            "text", "text", "text", "numeric", "numeric", 
                                            "numeric", "numeric","numeric", "numeric", 
                                            "numeric", "numeric", "numeric", "numeric",
                                            "numeric", "numeric"))
Basico_PE %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>% 
  rename( "V1"= V001,"V2"= V002, "V3"= V009)->Basico_Geral_dummy

Basico_AL<- read_excel("Dados_censo_ibge_2010/data_exc/AL/Base informaçoes setores2010 universo AL/EXCEL/Basico_AL.xls",
                       col_types = c("text", "text", "text", "text", "text", "text",
                                     "text", "text", "text", "text", "text", "text", 
                                     "text","text", "text", "text", "text", "text",
                                     "text", "text", "text", "numeric", "numeric", 
                                     "numeric", "numeric","numeric", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric"))
Basico_AL %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral_dummy)->Basico_Geral


Basico_BA <- read_excel("Dados_censo_ibge_2010/data_exc/BA/Base informaçoes setores2010 universo BA/EXCEL/Basico_BA.xls", 
                        col_types = c("text", "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text", "text", 
                                      "text","text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
Basico_BA %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral)->Basico_Geral

Basico_CE <- read_excel("Dados_censo_ibge_2010/data_exc/CE/Base informaçoes setores2010 universo CE/EXCEL/Basico_CE.xls", 
                        col_types = c("text", "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text", "text", 
                                      "text","text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))

Basico_CE %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral)->Basico_Geral

Basico_MA <- read_excel("Dados_censo_ibge_2010/data_exc/MA/Base informaçoes setores2010 universo MA/EXCEL/Basico_MA.xls", 
                        col_types = c("text", "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text", "text", 
                                      "text","text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))

Basico_MA %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral)->Basico_Geral


Basico_MG <- read_excel("Dados_censo_ibge_2010/data_exc/MG/Base informaçoes setores2010 universo MG/EXCEL/Basico-MG.xls", 
                        col_types = c("text", "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text", "text", 
                                      "text","text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
Basico_MG %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral)->Basico_Geral

Basico_PB <- read_excel("Dados_censo_ibge_2010/data_exc/PB/Base informaçoes setores2010 universo PB/EXCEL/Basico_PB.xls", 
                        col_types = c("text", "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text", "text", 
                                      "text","text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
Basico_PB %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral)->Basico_Geral

Basico_PI <- read_excel("Dados_censo_ibge_2010/data_exc/PI/Base informaçoes setores2010 universo PI/EXCEL/Basico_PI.xls", 
                        col_types = c("text", "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text", "text", 
                                      "text","text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
Basico_PI %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral)->Basico_Geral


Basico_RN <- read_excel("Dados_censo_ibge_2010/data_exc/RN/Base informaçoes setores2010 universo RN/EXCEL/Basico_RN.xls", 
                        col_types = c("text", "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text", "text", 
                                      "text","text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
Basico_RN %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral)->Basico_Geral

Basico_SE <- read_excel("Dados_censo_ibge_2010/data_exc/SE/Base informaçoes setores2010 universo SE/EXCEL/Basico_SE.xls", 
                        col_types = c("text", "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text", "text", 
                                      "text","text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
Basico_SE %>% 
  select(Cod_setor, Cod_UF, Situacao_setor, V001, V002, V009) %>%
  rename( "V1"= V001,"V2"= V002, "V3"= V009) %>% 
  full_join(Basico_Geral)->Basico_Geral
str(Basico_Geral)

remove(Basico_AL, Basico_BA, Basico_CE, Basico_MG, Basico_MA,
       Basico_PB, Basico_PE, Basico_PI, Basico_RN, Basico_SE, Basico_Geral_dummy)

###### 
# Domicilios Geral

Domicilio01_AL <- read_excel("Dados_censo_ibge_2010/data_exc/AL/Base informaçoes setores2010 universo AL/EXCEL/Domicilio01_AL.XLS")
str(Domicilio01_AL) #OMG 
Domicilio01_AL %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>% 
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("27")) -> Domicilios01_Geral


Domicilio01_BA <- read_excel("Dados_censo_ibge_2010/data_exc/BA/Base informaçoes setores2010 universo BA/EXCEL/Domicilio01_BA.xls")
str(Domicilio01_BA)# verificandoo
Domicilio01_BA %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>% 
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("BA")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral

Domicilio01_CE <- read_excel("Dados_censo_ibge_2010/data_exc/CE/Base informaçoes setores2010 universo CE/EXCEL/Domicilio01_CE.xls")
str(Domicilio01_CE)# verificandoo
Domicilio01_CE %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("CE")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral

Domicilio01_MA <- read_excel("Dados_censo_ibge_2010/data_exc/MA/Base informaçoes setores2010 universo MA/EXCEL/Domicilio01_MA.XLS")
str(Domicilio01_MA)# verificandoo
Domicilio01_MA %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MA")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral


Domicilio01_MG <- read_excel("Dados_censo_ibge_2010/data_exc/MG/Base informaçoes setores2010 universo MG/EXCEL/Domicilio01_MG.xls")
str(Domicilio01_MG)# verificandoo
Domicilio01_MG %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MG")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral


Domicilio01_PB <- read_excel("Dados_censo_ibge_2010/data_exc/PB/Base informaçoes setores2010 universo PB/EXCEL/Domicilio01_PB.xls")
str(Domicilio01_PB)# verificandoo
Domicilio01_PB %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PB")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral


Domicilio01_PE <- read_excel("Dados_censo_ibge_2010/data_exc/PE_20171016/PE/Base informaçoes setores2010 universo PE/EXCEL/Domicilio01_PE.xls")
str(Domicilio01_PE)# verificandoo
Domicilio01_PE %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PE")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral

Domicilio01_PI <- read_excel("Dados_censo_ibge_2010/data_exc/PI/Base informaçoes setores2010 universo PI/EXCEL/Domicilio01_PI.XLS")
str(Domicilio01_PI)# verificandoo
Domicilio01_PI %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PI")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral

Domicilio01_RN <- read_excel("Dados_censo_ibge_2010/data_exc/RN/Base informaçoes setores2010 universo RN/EXCEL/Domicilio01_RN.xls")
str(Domicilio01_RN)# verificandoo
Domicilio01_RN %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("RN")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral

Domicilio01_SE <- read_excel("Dados_censo_ibge_2010/data_exc/SE/Base informaçoes setores2010 universo SE/EXCEL/Domicilio01_SE.xls")
str(Domicilio01_SE)# verificandoo
Domicilio01_SE %>%
  select(Cod_setor, Situacao_setor, V001, V013, V014, V016, V019, V020, V038, V046) %>%
  rename( "V4"= V001, "V5"=V013, "V6"=V014, "V7"=V016, "V8"=V019, "V9"=V020, "V10"=V038, "V11"=V046) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("SE")) %>% 
  full_join(Domicilios01_Geral)-> Domicilios01_Geral

remove(Domicilio01_AL, Domicilio01_BA, Domicilio01_CE, Domicilio01_MG, Domicilio01_MA,
       Domicilio01_PB, Domicilio01_PE, Domicilio01_PI, Domicilio01_RN, Domicilio01_SE)

######
# pessoa01_geral

Pessoa01_AL <- read_excel("Dados_censo_ibge_2010/data_exc/AL/Base informaçoes setores2010 universo AL/EXCEL/Pessoa01_AL.xls")
str(Pessoa01_AL)
Pessoa01_AL %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("27"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) -> Pessoa01_Geral



Pessoa01_BA <- read_excel("Dados_censo_ibge_2010/data_exc/BA/Base informaçoes setores2010 universo BA/EXCEL/Pessoa01_BA.xls")
str(Pessoa01_BA)
Pessoa01_BA %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("BA"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral
  


Pessoa01_CE <- read_excel("Dados_censo_ibge_2010/data_exc/CE/Base informaçoes setores2010 universo CE/EXCEL/Pessoa01_CE.xls")
str(Pessoa01_CE)
Pessoa01_CE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("CE"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral

Pessoa01_MA <- read_excel("Dados_censo_ibge_2010/data_exc/MA/Base informaçoes setores2010 universo MA/EXCEL/Pessoa01_MA.xls")
str(Pessoa01_MA)
Pessoa01_MA %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MA"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral

Pessoa01_MG <- read_excel("Dados_censo_ibge_2010/data_exc/MG/Base informaçoes setores2010 universo MG/EXCEL/Pessoa01_MG.xls")
str(Pessoa01_MG)
Pessoa01_MG %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MG"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral


Pessoa01_PB <- read_excel("Dados_censo_ibge_2010/data_exc/PB/Base informaçoes setores2010 universo PB/EXCEL/Pessoa01_PB.xls")
str(Pessoa01_PB)
Pessoa01_PB %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PB"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral# Na na Sit_setor

Pessoa01_PE <- read_excel("Dados_censo_ibge_2010/data_exc/PE_20171016/PE/Base informaçoes setores2010 universo PE/EXCEL/Pessoa01_PE.xls")
str(Pessoa01_PE)
Pessoa01_PE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PE"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral

Pessoa01_PI <- read_excel("Dados_censo_ibge_2010/data_exc/PI/Base informaçoes setores2010 universo PI/EXCEL/Pessoa01_PI.xls")
str(Pessoa01_PI)
Pessoa01_PI %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PI"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral

Pessoa01_RN <- read_excel("Dados_censo_ibge_2010/data_exc/RN/Base informaçoes setores2010 universo RN/EXCEL/Pessoa01_RN.xls")
str(Pessoa01_RN)
Pessoa01_RN %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("RN"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral

Pessoa01_SE <- read_excel("Dados_censo_ibge_2010/data_exc/SE/Base informaçoes setores2010 universo SE/EXCEL/Pessoa01_SE.xls")
str(Pessoa01_SE)
Pessoa01_SE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("SE"))%>%
  rowwise() %>%
  mutate(V12 = sum(c_across(c(15:79)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V12) %>%
  full_join(Pessoa01_Geral)->Pessoa01_Geral

remove(Pessoa01_AL, Pessoa01_BA, Pessoa01_CE, Pessoa01_MG, Pessoa01_MA, 
       Pessoa01_PB, Pessoa01_PE, Pessoa01_PI, Pessoa01_RN, Pessoa01_SE)

######
#Pessoa03_Geral.csv
Pessoa03_AL <- read_excel("Dados_censo_ibge_2010/data_exc/AL/Base informaçoes setores2010 universo AL/EXCEL/Pessoa03_AL.xls")
str(Pessoa03_AL)
Pessoa03_AL %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("27"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006)->Pessoa03_Geral

Pessoa03_BA <- read_excel("Dados_censo_ibge_2010/data_exc/BA/Base informaçoes setores2010 universo BA/EXCEL/Pessoa03_BA.xls")
str(Pessoa03_BA)
Pessoa03_BA %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("BA"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

Pessoa03_CE <- read_excel("Dados_censo_ibge_2010/data_exc/CE/Base informaçoes setores2010 universo CE/EXCEL/Pessoa03_CE.xls")
str(Pessoa03_CE)
Pessoa03_CE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("CE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

Pessoa03_MA <- read_excel("Dados_censo_ibge_2010/data_exc/MA/Base informaçoes setores2010 universo MA/EXCEL/Pessoa03_MA.xls")
str(Pessoa03_MA)
Pessoa03_MA %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MA"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

Pessoa03_MG <- read_excel("Dados_censo_ibge_2010/data_exc/MG/Base informaçoes setores2010 universo MG/EXCEL/Pessoa03_MG.xls")
str(Pessoa03_MG)
Pessoa03_MG %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MG"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

Pessoa03_PB <- read_excel("Dados_censo_ibge_2010/data_exc/PB/Base informaçoes setores2010 universo PB/EXCEL/Pessoa03_PB.xls")
str(Pessoa03_PB)
Pessoa03_PB %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PB"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

Pessoa03_PE <- read_excel("Dados_censo_ibge_2010/data_exc/PE_20171016/PE/Base informaçoes setores2010 universo PE/EXCEL/Pessoa03_PE.xls")
str(Pessoa03_PE)
Pessoa03_PE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

Pessoa03_PI <- read_excel("Dados_censo_ibge_2010/data_exc/PI/Base informaçoes setores2010 universo PI/EXCEL/Pessoa03_PI.xls")
str(Pessoa03_PI)
Pessoa03_PI %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PI"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

Pessoa03_RN <- read_excel("Dados_censo_ibge_2010/data_exc/RN/Base informaçoes setores2010 universo RN/EXCEL/Pessoa03_RN.xls")
str(Pessoa03_RN)
Pessoa03_RN %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("RN"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

Pessoa03_SE <- read_excel("Dados_censo_ibge_2010/data_exc/SE/Base informaçoes setores2010 universo SE/EXCEL/Pessoa03_SE.xls")
str(Pessoa03_SE)
Pessoa03_SE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("SE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V001, V002, V003, V004, V005, V006) %>% 
  rename("V13"=V001, "V14"=V002, "V15"=V003, "V16"=V004, "V17"=V005, "V18"=V006) %>% 
  full_join(Pessoa03_Geral)->Pessoa03_Geral

remove(Pessoa03_AL, Pessoa03_BA, Pessoa03_CE, Pessoa03_MG, Pessoa03_MA,
       Pessoa03_PB, Pessoa03_PE, Pessoa03_PI, Pessoa03_RN, Pessoa03_SE)

#####
# Domicilios_renda_geral

DomicilioRenda_AL <- read_excel("Dados_censo_ibge_2010/data_exc/AL/Base informaçoes setores2010 universo AL/EXCEL/DomicilioRenda_AL.XLS")
str(DomicilioRenda_AL)
DomicilioRenda_AL %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("27"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014)->DomicilioRenda_Geral

DomicilioRenda_BA <- read_excel("Dados_censo_ibge_2010/data_exc/BA/Base informaçoes setores2010 universo BA/EXCEL/DomicilioRenda_BA.XLS")
str(DomicilioRenda_BA)
DomicilioRenda_BA %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("BA"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral

DomicilioRenda_CE <- read_excel("Dados_censo_ibge_2010/data_exc/CE/Base informaçoes setores2010 universo CE/EXCEL/DomicilioRenda_CE.XLS")
str(DomicilioRenda_CE)
DomicilioRenda_CE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("CE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral

DomicilioRenda_MA <- read_excel("Dados_censo_ibge_2010/data_exc/MA/Base informaçoes setores2010 universo MA/EXCEL/DomicilioRenda_MA.XLS")
str(DomicilioRenda_MA)
DomicilioRenda_MA %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MA"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral

DomicilioRenda_MG <- read_excel("Dados_censo_ibge_2010/data_exc/MG/Base informaçoes setores2010 universo MG/EXCEL/DomicilioRenda_MG.XLS")
str(DomicilioRenda_MG)
DomicilioRenda_MG %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MG"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral

DomicilioRenda_PB <- read_excel("Dados_censo_ibge_2010/data_exc/PB/Base informaçoes setores2010 universo PB/EXCEL/DomicilioRenda_PB.XLS")
str(DomicilioRenda_PB)
DomicilioRenda_PB %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PB"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral

DomicilioRenda_PE <- read_excel("Dados_censo_ibge_2010/data_exc/PE_20171016/PE/Base informaçoes setores2010 universo PE/EXCEL/DomicilioRenda_PE.XLS")
str(DomicilioRenda_PE)
DomicilioRenda_PE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral

DomicilioRenda_PI <- read_excel("Dados_censo_ibge_2010/data_exc/PI/Base informaçoes setores2010 universo PI/EXCEL/DomicilioRenda_PI.XLS")
str(DomicilioRenda_PI)
DomicilioRenda_PI %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PI"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral

DomicilioRenda_RN <- read_excel("Dados_censo_ibge_2010/data_exc/RN/Base informaçoes setores2010 universo RN/EXCEL/DomicilioRenda_RN.XLS")
str(DomicilioRenda_RN)
DomicilioRenda_RN %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("RN"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral

DomicilioRenda_SE <- read_excel("Dados_censo_ibge_2010/data_exc/SE/Base informaçoes setores2010 universo SE/EXCEL/DomicilioRenda_SE.XLS")
str(DomicilioRenda_SE)
DomicilioRenda_SE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("SE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V005, V007, V014) %>% 
  rename("V19"=V005, "V20"=V007, "V21"=V014) %>% 
  full_join(DomicilioRenda_Geral)->DomicilioRenda_Geral


remove(DomicilioRenda_AL, DomicilioRenda_BA, DomicilioRenda_CE, DomicilioRenda_MG, DomicilioRenda_MA,
       DomicilioRenda_PB, DomicilioRenda_PE, DomicilioRenda_PI, DomicilioRenda_RN, DomicilioRenda_SE)

#####
#Entorno04_Geraç

Entorno04_AL <- read_excel("Dados_censo_ibge_2010/data_exc/AL/Base informaçoes setores2010 universo AL/EXCEL/Entorno04_AL.XLS")
str(Entorno04_AL)
Entorno04_AL %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("27"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23)->Entorno04_Geral

Entorno04_BA <- read_excel("Dados_censo_ibge_2010/data_exc/BA/Base informaçoes setores2010 universo BA/EXCEL/Entorno04_BA.XLS")
str(Entorno04_BA)
Entorno04_BA %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("BA"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral

Entorno04_CE <- read_excel("Dados_censo_ibge_2010/data_exc/CE/Base informaçoes setores2010 universo CE/EXCEL/Entorno04_CE.XLS")
str(Entorno04_CE)
Entorno04_CE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("CE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral

Entorno04_MA <- read_excel("Dados_censo_ibge_2010/data_exc/MA/Base informaçoes setores2010 universo MA/EXCEL/Entorno04_MA.XLS")
str(Entorno04_MA)
Entorno04_MA %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MA"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral

Entorno04_MG <- read_excel("Dados_censo_ibge_2010/data_exc/MG/Base informaçoes setores2010 universo MG/EXCEL/Entorno04_MG.XLS")
str(Entorno04_MG)
Entorno04_MG %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("MG"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral

Entorno04_PB <- read_excel("Dados_censo_ibge_2010/data_exc/PB/Base informaçoes setores2010 universo PB/EXCEL/Entorno04_PB.XLS")
str(Entorno04_PB)
Entorno04_PB %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PB"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral

Entorno04_PE <- read_excel("Dados_censo_ibge_2010/data_exc/PE_20171016/PE/Base informaçoes setores2010 universo PE/EXCEL/Entorno04_PE.XLS")
str(Entorno04_PE)
Entorno04_PE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral


Entorno04_PI <- read_excel("Dados_censo_ibge_2010/data_exc/PI/Base informaçoes setores2010 universo PI/EXCEL/Entorno04_PI.XLS")
str(Entorno04_PI)
Entorno04_PI %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("PI"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral

Entorno04_RN <- read_excel("Dados_censo_ibge_2010/data_exc/RN/Base informaçoes setores2010 universo RN/EXCEL/Entorno04_RN.XLS")
str(Entorno04_RN)
Entorno04_RN %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("RN"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral

Entorno04_SE <- read_excel("Dados_censo_ibge_2010/data_exc/SE/Base informaçoes setores2010 universo SE/EXCEL/Entorno04_SE.XLS")
str(Entorno04_SE)
Entorno04_SE %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  mutate_at(vars(Cod_setor, Situacao_setor), ~as.character(as.numeric(.))) %>% 
  mutate(Cod_UF=rep("SE"))%>%
  select(Cod_setor, Situacao_setor, Cod_UF, V625, V626, V627, V628) %>% 
  rowwise() %>%
  mutate(V22 = sum(c_across(c(4:5)), na.rm = F)) %>%
  mutate(V23 = sum(c_across(c(6:7)), na.rm = F)) %>% 
  ungroup() %>% 
  select(Cod_setor, Situacao_setor, Cod_UF, V22, V23) %>% 
  full_join(Entorno04_Geral)->Entorno04_Geral

remove(Entorno04_AL, Entorno04_BA, Entorno04_CE, Entorno04_MG, Entorno04_MA, 
       Entorno04_PB, Entorno04_PE, Entorno04_PI, Entorno04_RN, Entorno04_SE)


######
# juntando as variaveis

full_join(Basico_Geral, Domicilios01_Geral) -> IBGE_filter_data

# Rodar apenas se quiser ver o erro que o full_join da devido a NA da tabela Pessoa01_PB que esta na Pessoa01_geral
#full_join(IBGE_filter_data,Pessoa01_Geral) -> x_row_a_mais

IBGE_filter_data %>% 
  full_join(Pessoa03_Geral) %>% 
  full_join(DomicilioRenda_Geral) %>% 
  full_join(Entorno04_Geral) %>% 
  right_join(Pessoa01_Geral) %>% # Tem que ser right_join pq a linha com NA tá dentro da Pessoa01_geral 
  select(Cod_setor, Situacao_setor, Cod_UF, V1, V2, V3, V4,
         V6, V7, V8, V9, V10, V11, V12, V13, everything())->IBGE_filter_data #Ajeitanto pra variáveis ficarem em ordem nas colunas

str(IBGE_filter_data)

