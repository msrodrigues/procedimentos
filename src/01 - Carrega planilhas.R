library(RColorBrewer)
library(extrafont)
library(scales)
library(chron)
library(treemap)
library(ggthemes)
library(ggmap)
library(knitr)
library(readr)
library(readxl)
library(xtable)
library(kableExtra)
library(rebus)
library(tibble)
library(cronR)
library(lubridate)
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(usethis)

setwd("~/Dropbox/Coding/R/misc/procedimentos/")
source("~/Dropbox/Coding/R/funs/msrfun.R")

Sys.setenv(TZ="Brazil/East")
options(tz="Brazil/East")
Sys.getenv("TZ")
Sys.setlocale("LC_TIME", "pt_BR")


carrega_planilha_google <- function(arquivo , planilha = "Form Responses 1") {
  # Carrega a SpreadSheet(ss) Procedimentos (Response)
  lista_planilhas <- drive_find(type = "spreadsheet") 
  id_arquivo <- lista_planilhas[lista_planilhas$name == arquivo, 2]$id
  ss <- drive_get(id = id_arquivo)
  sheets_read(ss, sheet = planilha)
}

# Cria DF das respostas
respostas <- carrega_planilha_google(arquivo = "Procedimentos (Responses)", planilha = "Form Responses 1")
antigas <- carrega_planilha_google(arquivo = "Registro de Procedimentos (Responses)")


# Conversão de tipos
respostas$Prontuário <- as.numeric(as.character(respostas$Prontuário))
respostas <- troca_tipo(respostas,c("Residente", "Procedimento", "Estágio", "Preceptor"), fun = as.factor)

names(respostas) <- c("timestamp", "residente", "prontuario", "data", "estagio", "procedimento", 
                      "sucesso", "complicacoes", "preceptor", "observacoes", "eco")

levels(factor(antigas$`Quem realizou o procedimento?`))
# Carrega as respostas antigas --------------------------------------------
antigas$`Qual procedimento?` <- if_else((antigas$`Qual o procedimento realizado?`) == "Outro", antigas$`Qual procedimento?`, antigas$`Qual o procedimento realizado?`)

# Cria o data frame RECICLADO para usar as respotas antigas de acordo com o novo padrão
reciclado <- antigas[, c("Carimbo de data/hora", "Quem realizou o procedimento?", "Número do prontuário", 
                         "Data do procedimento", "Onde foi realizado?", "Qual procedimento?", "Complicações imediatas", "Complicações", 
                         "Preceptor responsável", "Nome do supervisor")]


# Cria a variável booleana preceptor, para gravar a informação da presença ou não de preceptor
# usando as duas variáveis do banco de dados antigo ( "Preceptor responsável" e "Nome do supervisor")

reciclado$Preceptor <- if_else(is.na(reciclado$`Preceptor responsável`) & is.na(reciclado$`Nome do supervisor`),"Não","Sim")
reciclado$`Nome do supervisor` <- NA
reciclado$`Complicações imediatas` <- NA
reciclado$`Preceptor responsável` <- NULL
names(reciclado)
names(respostas)




names(reciclado) <- c("timestamp", "residente", "prontuario", "data", "estagio", "procedimento", 
                      "sucesso", "complicacoes", "observacoes",  "preceptor")

reciclado <- reciclado[, c("timestamp", "residente", "prontuario", "data", "estagio", "procedimento", 
                           "sucesso", "complicacoes", "preceptor", "observacoes") ]

reciclado$antigo <- TRUE
respostas$antigo <- FALSE

# Correção dos Nones dos Residentes
reciclado$residente <- if_else(reciclado$residente == "Eliana Schneider", "Eliana Iora Schneider", reciclado$residente)
reciclado$residente <- if_else(reciclado$residente == "Gabriel Miranda", "Gabriel Curubeto Lona de Miranda", reciclado$residente)

reciclado$residente <- if_else(reciclado$residente == "Laura Zanrosso", "Laura Zaparoli Zanrosso", reciclado$residente)
reciclado$residente <- if_else(reciclado$residente == "Maria Luisa Adib", "Maria Luisa Barrufi Adib", reciclado$residente)
reciclado$residente <- if_else(reciclado$residente == "Ana Beatriz Aguiar", "Ana Beatriz Machado de Aguiar", reciclado$residente)
reciclado$residente <- if_else(reciclado$residente == "Camila Osorio" , "Camila Osório Silveira", reciclado$residente)
reciclado$residente <- if_else(reciclado$residente == "Felicia", "Felícia Mariana Machado Lima", reciclado$residente)
reciclado$residente <- if_else(reciclado$residente == "Laisa Matana", "Laisa Manuela Mattana", reciclado$residente)
reciclado$residente <- if_else(reciclado$residente == "Marcella Barbosa", "Marcella de Lima Barbosa", reciclado$residente)
reciclado$residente <- if_else(reciclado$residente == "Mauricio Lazzarin", "Mauricio da Costa Lazzarin", reciclado$residente)

reciclado$residente <- as.factor(reciclado$residente)





reciclado$hospital <- reciclado$estagio
reciclado$estagio <- as.factor(NA)
respostas$hospital <- NA
reciclado$eco <- NA

resposta <- respostas

respostas <- rbind(reciclado, respostas)

respostas$dia_semana <- factor(weekdays(respostas$data), ordered = TRUE,
                               levels = c("Segunda Feira", "Terça Feira", "Quarta Feira", "Quinta Feira", "Sexta Feira", "Sábado", "Domingo"))



# Carrega a lista de residentes 
residentes <- carrega_planilha_google(arquivo = "residentes", planilha = "Sheet1")

# Conversão de tipos da lista dos residentes
residentes <- troca_tipo(residentes, c("residente", "residencia", "nick"), as.factor)

# Merge na lista de residentes e as repostas
respostas <- left_join(respostas, residentes, by = c("residente"))
respostas <- troca_tipo(respostas, c("residente"), as.factor)
saveRDS(respostas, "~/Dropbox/Coding/R/misc/procedimentos/data/respostas.rds")
saveRDS(residentes, "~/Dropbox/Coding/R/misc/procedimentos/data/residentes.rds")











