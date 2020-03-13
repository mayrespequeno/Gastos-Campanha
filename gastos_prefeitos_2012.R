#------------------Dados relativos a candidatos-------------------------------------------

cand <- candidate_local(2012, uf = "all", ascii = FALSE, encoding = "latin1",
                        export = TRUE)

View(cand)

colnames(cand)

cand1 <- subset(cand, select = c("SIGLA_UF","ANO_ELEICAO","NUMERO_CANDIDATO","NUM_TITULO_ELEITORAL_CANDIDATO",
                                 "SEQUENCIAL_CANDIDATO", "CPF_CANDIDATO", "NOME_CANDIDATO","DATA_NASCIMENTO",
                                 "CODIGO_SEXO","DESCRICAO_SEXO","NUMERO_PARTIDO","SIGLA_PARTIDO","NUM_TURNO",
                                 "CODIGO_LEGENDA","COMPOSICAO_LEGENDA","NOME_MUNICIPIO_NASCIMENTO","CODIGO_CARGO", 
                                 "DESCRICAO_CARGO","COD_SIT_TOT_TURNO","DESC_SIT_TOT_TURNO","DESPESA_MAX_CAMPANHA"))


"Nesse subset está ausente o municipio de candidatura, apenas está o municipio de nascimento"

#Removendo casos de vice prefeito
cand2 <- with(cand1, which(CODIGO_CARGO == 12, arr.ind = TRUE))
cand3 <- cand1[-cand2, ]
#Removendo casos de vereador
cand4 <- with(cand3, which(CODIGO_CARGO == 13, arr.ind = TRUE))
cand5 <- cand3[-cand4, ]


#------------------Dados relativos a votos-------------------------------------------------
vot <- vote_mun_zone_local(2012)

vot1 <- subset(vot, select = c("SIGLA_UF","ANO_ELEICAO","NUMERO_CAND","SQ_CANDIDATO","NOME_CANDIDATO",
                               "NUMERO_PARTIDO","SIGLA_PARTIDO","NUM_TURNO",
                               "SEQUENCIAL_LEGENDA","COMPOSICAO_LEGENDA","CODIGO_CARGO", 
                               "DESCRICAO_CARGO","CODIGO_MUNICIPIO",
                               "NOME_MUNICIPIO", "TOTAL_VOTOS"))

#Removendo casos de vereador
vot2 <- with(vot1, which(CODIGO_CARGO == 13, arr.ind = TRUE))
vot3 <- vot1[-vot2, ]

names(vot3)[1:15] <- c("SIGLA_UF","ANO_ELEICAO","NUMERO_CANDIDATO","SEQUENCIAL_CANDIDATO","NOME_CANDIDATO",
                       "NUMERO_PARTIDO","SIGLA_PARTIDO","NUM_TURNO","CODIGO_LEGENDA","COMPOSICAO_LEGENDA",
                       "CODIGO_CARGO","DESCRICAO_CARGO","CODIGO_MUNICIPIO","NOME_MUNICIPIO","TOTAL_VOTOS")

ncol(vot3)
#------------------Dados relativos as despesas------------------------------------------
desp <- personal_finances_local(2012)
#ERROR

#Recorremos então a base do TSE

desp <- read.csv("C:/Users/mayre/Documents/Rscript/Gastos 2012/despesas_candidatos_2012_brasil.txt",
                 sep=";", dec = ',', quote="\"")


colnames(despesas_cand)

desp1 <- subset(desp, select = c("UF","Desc..Eleição","Número.candidato", "Sequencial.Candidato","CPF.do.candidato","Nome.candidato",
                                 "Sigla..Partido","Município","Cargo","Valor.despesa","Tipo.despesa","Descriçao.da.despesa" ))

#Criando variável numérica para excluir casos que não serão utilizados
desp1$CODIGO_CARGO <- as.numeric(desp1$Cargo)

#Recodoficamdo para criar variável Ano de Eleição
desp1$ANO_ELEICAO <- ifelse(desp1$Desc..Eleição == "Eleição Municipal 2012", "2012", NA)

#Removendo os casos de vereador
desp2 <- with(desp1, which(CODIGO_CARGO == 2, arr.ind = TRUE))
desp3 <- desp1[-desp2, ]

desp3$CODIGO_CARGO <-NULL
desp3$Desc..Eleição <-NULL

colnames(desp3)
ncol(desp3)

names(desp3)[1:12] <- c("UF","NUMERO_CANDIDATO","SEQUENCIAL_CANDIDATO","CPF_CANDIDATO","NOME_CANDIDATO","SIGLA_PARTIDO",
                        "NOME_MUNICIPIO","DESCRICAO_CARGO","VALOR_DESPESA", "TIPO_DESPESA", "DESCRICAO_DESPESA", "ANO_ELEICAO")

#O dataframe ainda possui candidatos repetidos, mesmo com o tipos de gastos iguais, sendo assim, vamos agregÃ¡-los.


desp4 <- desp3%>% 
  group_by(UF,NUMERO_CANDIDATO,SEQUENCIAL_CANDIDATO,CPF_CANDIDATO,NOME_CANDIDATO,SIGLA_PARTIDO,
           NOME_MUNICIPIO,DESCRICAO_CARGO,TIPO_DESPESA) %>% 
  summarise(SUM_TIPO_DESPESA = sum(VALOR_DESPESA))


str(desp4$TIPO_DESPESA)

desp4$TIPO_DESPESA

desp4 %>% 
  mutate(TIPO_DESPESA=as.character(TIPO_DESPESA))

#------------------despesas_transporte-----------------------------------

desp4$despesast1 <- 
  ifelse((desp4$TIPO_DESPESA == "Combustíveis e lubrificantes" & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA,NA)

desp4$despesast2 <- ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Combustíveis e lubrificantes"
                            & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA, desp4$despesast1)

desp4$despesast3 <- ifelse((desp4$TIPO_DESPESA == "Cessão ou locação de veículos"
                            & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA, desp4$despesast2)

desp4$despesast4 <- ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Cessão ou locação de veículos"
                            & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA, desp4$despesast3)

desp4$despesast5 <- ifelse((desp4$TIPO_DESPESA == "Despesas com transporte ou deslocamento"
                            & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA, desp4$despesast4)

desp4$despesast6 <- ifelse((desp4$TIPO_DESPESA == "Baixa de Estimaveis - Despesas com transporte ou deslocamento"
                            & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA, desp4$despesast5)

names(desp4)[16] <- "DESPESAS_TRANSPORTE"

desp4[11:15] <- NULL

#------------------despesa_publicidade-----------------------------------------------------

desp4$publi1 <- 
  ifelse((desp4$TIPO_DESPESA == "Publicidade por materiais impressos" & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA,NA)

desp4$publi2 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Publicidade por materiais impressos" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi1)

desp4$publi3 <- 
  ifelse((desp4$TIPO_DESPESA == "Produção de jingles, vinhetas e slogans" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi2)

desp4$publi4 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Produção de jingles, vinhetas e slogans" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi3)

desp4$publi5 <- 
  ifelse((desp4$TIPO_DESPESA == "Atividades de militância e mobilização de rua" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi4)

desp4$publi6 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Atividades de militância e mobilização de rua" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi5)

desp4$publi7 <- 
  ifelse((desp4$TIPO_DESPESA == "Publicidade por carros de som" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi6)

desp4$publi8 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Publicidade por carros de som" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi7)

desp4$publi9 <- 
  ifelse((desp4$TIPO_DESPESA == "Publicidade por adesivos" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi8)

desp4$publi10 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Publicidade por adesivos" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi9)

desp4$publi11 <- 
  ifelse((desp4$TIPO_DESPESA == "Comícios" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi10)

desp4$publi12 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Comícios" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi11)

desp4$publi13 <- 
  ifelse((desp4$TIPO_DESPESA == "Produção de programas de rádio, televisão ou vídeo" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi12)

desp4$publi14 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Produção de programas de rádio, televisão ou vídeo" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi13)

desp4$publi15 <- 
  ifelse((desp4$TIPO_DESPESA == "Eventos de promoção da candidatura" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi14)

desp4$publi16 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Eventos de promoção da candidatura" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi15)

desp4$publi17 <- 
  ifelse((desp4$TIPO_DESPESA == "Criação e inclusão de páginas na internet" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi16)

desp4$publi18 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Criação e inclusão de páginas na internet" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi17)

desp4$publi19 <- 
  ifelse((desp4$TIPO_DESPESA == "Publicidade por jornais e revistas" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi18)

desp4$publi19 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Publicidade por jornais e revistas" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi18)

names(desp4)[13] <- "DESPESAS_PUBLICIDADE"

desp4[24] <- NULL

desp4$publi20 <- 
  ifelse((desp4$TIPO_DESPESA == "Publicidade por placas, estandartes e faixas" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$DESPESAS_PUBLICIDADE)

desp4$publi20 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Publicidade por placas, estandartes e faixas" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi20)

desp4$publi21 <- 
  ifelse((desp4$TIPO_DESPESA == "Publicidade por Telemarketing" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$DESPESAS_PUBLICIDADE)

desp4$publi22 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Publicidade por Telemarketing" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$publi21)

desp4$DESPESAS_PUBLICIDADE <- NULL

names(desp4)[25] <- "DESPESAS_PUBLICIDADE"

#----------------- despesas_doacoe_outros_cand_comi_part----------------------------------------

desp4$DESPESAS_DOACOES_OUTROS_CAND_COMI_PART <- 
  ifelse((desp4$TIPO_DESPESA == "Doações financeiras a outros candidatos/comitês financeiros/partido" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,NA)

desp4$DESPESAS_DOACOES_OUTROS_CAND_COMI_PART1<- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Doações financeiras a outros candidatos/comitês financeiros/partido" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$DESPESAS_DOACOES_OUTROS_CAND_COMI_PART)

desp4[12] <- NULL

names(desp4)[13] <- "DESPESAS_DOACOES_OUTROS_CAND_COMI_PART"

#------------------despesas_pesquisas_eleitorais---------------

desp4$pesqui1<- 
  ifelse((desp4$TIPO_DESPESA == "Pesquisas ou testes eleitorais" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,NA)

desp4$pesqui2<- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Pesquisas ou testes eleitorais" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$pesqui1)

names(desp4)[15] <- "DESPESAS_PESQUISAS_ELEITORAIS"

desp4[14] <- NULL


#------------------despesas_com_pessoal---------------------------

desp4$pessoal1<- 
  ifelse((desp4$TIPO_DESPESA == "Despesas com pessoal" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,NA)

desp4$pessoal2<- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Despesas com pessoal" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$pessoal1)

names(desp4)[16] <- "DESPESAS_COM_PESSOAL"

desp4[15] <- NULL

#------------------despesas_multas_eleitorais---------------------

desp4$multas1<- 
  ifelse((desp4$TIPO_DESPESA == "Multas eleitorais" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,NA)

desp4$multas2<- 
  ifelse((desp4$TIPO_DESPESA == "Multas eleitorais" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA, desp4$multas1)

names(desp4)[17] <- "DESPESAS_MULTAS_ELEITORAIS"

desp4[16] <- NULL

#------------------despesas_comite-------------------------------

desp4$com1<- 
  ifelse((desp4$TIPO_DESPESA == "Alimentação" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,NA)

desp4$com2<- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Alimentação" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA, desp4$com1)

desp4$com3<- 
  ifelse((desp4$TIPO_DESPESA == "Materiais de expediente" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$com2)

desp4$com4<- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Materiais de expediente" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$com3)

desp4$com5<- 
  ifelse((desp4$TIPO_DESPESA == "Água" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$com4)

desp4$com6<- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Água" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$com5)

desp4$com7<- 
  ifelse((desp4$TIPO_DESPESA == "Telefone" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$com6)

desp4$com7<- 
  ifelse((desp4$TIPO_DESPESA == "Baixas Estimáveis - Telefone" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$com6)

desp4$com8<- 
  ifelse((desp4$TIPO_DESPESA == "Energia Elétrica" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$com7)

desp4$com9<- 
  ifelse((desp4$TIPO_DESPESA == "Baixas Estimáveis - Energia Elétrica" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$com8)

names(desp4)[25] <- "DESPESAS_COMITE"

desp4[16:23] <- NULL



tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "LocaÃ§Ã£o/cessÃ£o de bens imÃ³veis"] <- "despesas_comite"
tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "Baixa de Estimaveis - LocaÃ§Ã£o/cessÃ£o de bens imÃ³veis"] <- "despesas_comite"


tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "LocaÃ§Ã£o/cessÃ£o de bens mÃ³veis (exceto veÃ???culos)"] <- "despesas_comite"
tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "Baixa de Estimaveis - LocaÃ§Ã£o/cessÃ£o de bens mÃ³veis (exceto veÃ???culos)"] <- "despesas_comite"


tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "PrÃ©-instalaÃ§Ã£o fÃ???sica de comitÃª de campanha"] <- "despesas_comite"
tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "Baixa de Estimaveis - PrÃ©-instalaÃ§Ã£o fÃ???sica de comitÃª de campanha"] <- "despesas_comite"

tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "CorrespondÃªncias e despesas postais"] <- "despesas_comite"
tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "Baixa de Estimaveis - CorrespondÃªncias e despesas postais"] <- "despesas_comite"


tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "AquisiÃ§Ã£o/DoaÃ§Ã£o de bens mÃ³veis ou imÃ³veis"] <- "despesas_comite"
tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "Baixa de Estimaveis - AquisiÃ§Ã£o/DoaÃ§Ã£o de bens mÃ³veis ou imÃ³veis"] <- "despesas_comite"


tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "Despesas com Hospedagem"] <- "despesas_comite"
tab11$TIPO_DESPESA[tab11$TIPO_DESPESA == "Baixa de Estimaveis - Despesas com Hospedagem"] <- "despesas_comite"
#------------------despesas_serv_terceiros--------------------------------

desp4$serv1 <- 
  ifelse((desp4$TIPO_DESPESA == "Serviços prestados por terceiros" & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA,NA)

desp4$serv2 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixas de Estimáveis - Serviços prestados por terceiros" & desp4$SUM_TIPO_DESPESA >= 0)
         ,desp4$SUM_TIPO_DESPESA,desp4$serv1)

desp4$serv3 <- 
  ifelse((desp4$TIPO_DESPESA == "Serviços próprios prestados por terceiros" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA, desp4$serv2)

desp4$serv4 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixas de Estimáveis - Serviços próprios prestados por terceiros" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA, desp4$serv3)

names(desp4)[21] <- "SERV_TERCEIROS"

desp4[18:20] <- NULL

#------------------despesas_no_especificado---------------------------

desp4$noesp <- 
  ifelse((desp4$TIPO_DESPESA == "Diversas a especificar" & desp4$SUM_TIPO_DESPESA >= 0),desp4$SUM_TIPO_DESPESA,NA)

desp4$noesp1 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Diversas a especificar" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$noesp)

names(desp4)[19] <- "DESPESAS_NO_ESPECIFICADO"

desp4[19] <- NULL

#------------------despesas_impos_encarg------------------------------
desp4$impo <- 
  ifelse((desp4$TIPO_DESPESA == "Encargos financeiros, taxas bancárias e/ou op. cartão de crédito" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,NA)

desp4$impo1 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Encargos financeiros, taxas bancárias e/ou op. cartão de crédito" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$impo)


names(desp4)[21] <- "DESPESAS_IMPOS_ENCARG"

desp4[20] <- NULL

#------------------despesas_reembolso_eleitores---------------------

desp4$re <- 
  ifelse((desp4$TIPO_DESPESA == "Reembolsos de gastos realizados por eleitores" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,NA)

desp4$re1 <- 
  ifelse((desp4$TIPO_DESPESA == "Baixa de Estimáveis - Reembolsos de gastos realizados por eleitores" & desp4$SUM_TIPO_DESPESA >= 0),
         desp4$SUM_TIPO_DESPESA,desp4$re)

names(desp4)[22] <- "DESPESAS_REEMBOLSO_ELEITORES"

desp4[21] <- NULL





#--------------------merge para minha base final----------------------

names(desp4)[1] <- "SIGLA_UF"

colnames(desp4)

gastos1 <- merge(vot3, desp4, by = c("NUMERO_CANDIDATO","SEQUENCIAL_CANDIDATO"))

gastos2 <- merge(gastos1, cand5, by = c("NUMERO_CANDIDATO","SEQUENCIAL_CANDIDATO"))

colnames(gastos2)

excluir <- c("ANO_ELEICAO.y","SIGLA_UF.y","NOME_CANDIDATO.y","SIGLA_PARTIDO.x",
             "NUM_TURNO.y","NOME_MUNICIPIO.y","DESCRICAO_CARGO.y","NOME_CANDIDATO.y")

gastos2 <- gastos2[,!(names(gastos2)%in% excluir)]

head(gastos2)


excluir1 <- c("SIGLA_UF.x","DESPESA_MAX_CAMPANHA","COD_SIT_TOT_TURNO","COMPOSICAO_LEGENDA.y",
              "CPF_CANDIDATO.x","SIGLA_PARTIDO.y","CODIGO_MUNICIPIO","CODIGO_LEGENDA.x",
              "COMPOSICAO_LEGENDA.x","CODIGO_CARGO.x","CODIGO_CARGO.y","DESCRICAO_CARGO", 
              "NUMERO_PARTIDO.y","NUMERO_PARTIDO.x","NUM_TITULO_ELEITORAL_CANDIDATO",
              "DATA_NASCIMENTO","NOME_CANDIDATO.x")

gastos2 <- gastos2[,!(names(gastos2)%in% excluir1)]

head(gastos2)

#Removendo variáveis que não vou utilizar

gastos2$CODIGO_LEGENDA.y <- NULL

names(gastos2)[3] <- "ANO_ELEICAO"
names(gastos2)[4] <- "NUM_TURNO"
names(gastos2)[5] <- "DESCRICAO_CARGO"
names(gastos2)[6] <- "NOME_MUNICIPIO"
names(gastos2)[22] <- "CPF_CANDIDATO"                                   
View(gastos2)

#Filtrando casos de mulheres

gastos3 <- with(gastos2, which(CODIGO_SEXO == 2, arr.ind = TRUE))

gastos4 <- gastos2[-gastos3, ]
