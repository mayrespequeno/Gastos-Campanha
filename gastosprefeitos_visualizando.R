#---------------------Pacotes---------------
install.packages("dplyr")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("Rmisc")
install.packages("Psysc")


install.packages("readxl")
install.packages("stargazer")
install.packages("car")
install.packages("vcd")
install.packages("plyr")
install.packages("mousetrap")
install.packages("Rmisc")
install.packages("ggpubr")

#Lendo os pacotes

library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(Hmisc)
library(ggpubr)

library(readxl)
library(stargazer)

library(car)
library(RColorBrewer)
library(vcd)
library(plyr)
library(mousetrap)
library(Rmisc)

#---------------------Diretório e Visualização da Base----------------
load("C:/Users/mayre/Documents/Rscript/Meu dinheiro minhas regras/pref.RData")
View(pref)
getwd()
dir()


write.csv(pref, "gastosprefeitos.csv", row.names = FALSE)

#outra base


receitas2012 <- import("receitas_candidatos_2012_brasil.txt")


#---------------------Descritivos #5-------------------------------

describe(pref$eleicao)



#Tabela 1 - Descritivos dos gastos de campanha por eleição

tab1 <- pref %>% group_by(eleicao)%>%
  summarise(maximo = max(total_gastos_candidato),
             minimo = min(total_gastos_candidato),
             soma = sum(total_gastos_candidato),
             media = mean(total_gastos_candidato),
             dp = sd(total_gastos_candidato),
             cv = (dp/media)*100)
write.csv2(tab1,"Descritivos dos gastos de campanha por eleicao.csv", row.names = FALSE)

str(pref$eleicao)

# Médias para o data frame e output dos gráficos
tab2 <- pref%>% 
  na.omit()%>%
  group_by(eleicao)%>%
  summarise(publicidade = mean(gastos_publicidade),
            comite = mean(gastos_comite),
            transportes = mean(gastos_transporte),
            encargos = mean(gastos_impos_encarg),
            nao_especificado = mean(gastos_no_especificado),
            doaçoes = mean(gastos_doacoe_outros_cand_comi_part),
            com_pessoal = mean(gastos_com_pessoal),
            reembolso = mean(gastos_reembolso_eleitores),
            multas = mean(gastos_multas_eleitorais),
            pesquisas_eleitorais = mean(gastos_pesquisas_eleitorais),
            serviços_terceiros = mean(gastos_serv_terceiros))
write.csv2(tab1,".csv",row.names = FALSE)

#Modificando o data frame tab2

#Transformando linhas em novas variáveis para execução próximos gráficos
tab3 <- as.data.frame(t(tab2))
row.names(tab3)

#Criando uma nova coluna com categorias
tab3$categorias <- row.names(tab3)

#Names para as colunas criadas
names(tab3)[1:3] <- c("gastos_2008", "gastos_2012", "gastos_2016")

#Excluindo a primeira linha que não será utilizada
tab3 <- tab3[-c(1),]

#Renomeando categorias para plotar 
tab3$categoriasnew<- c("Publicidade", "Cômite", "Transportes",
                          "Encargos","Nao Especificado","Doaçoes",
                          "Com Pessoal","Reembolsos à Eleitores",
                          "Multas Eleitorais","Pesquisas Eleitorais",
                          "Serviços de Terceiros")
  

#---------------------Plotando Gráficos #5----------------------------
#Change axis text style
black.bold.12.text <- element_text(
  face = "bold", color = "black", size = 10)

#Gráfico 1 - Média por tipo de gastos em 2008
tab3 %>%
  ggplot(aes(y= gastos_2008, x= reorder(categoriasnew, gastos_2008)))+
  geom_bar(stat = "identity")+ 
  theme(legend.position = 'none') +
  coord_flip() +
  geom_hline(yintercept = mean(tab3$gastos_2008) , linetype = 'longdash', 
             color = 'black', size = 1)+
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000), limits = c(0,25000), expand = c(0,0))+
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y =black.bold.12.text)
 
#Gráfico 2 - Média por tipo de gastos em 2012 
tab3 %>%
  ggplot(aes(y= gastos_2012, x= reorder(categoriasnew, gastos_2012)))+
  geom_bar(stat = "identity")+ 
  theme(legend.position = 'none') +
  coord_flip() +
  geom_hline(yintercept = mean(tab3$gastos_2012) , linetype = 'longdash', 
             color = 'black', size = 1)+
  scale_y_continuous(breaks = c(0, 20000, 40000, 60000), limits = c(0,60000), expand = c(0,0))+
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y =black.bold.12.text)

#Gráfico 3 - Média por tipo de gastos em 2016
tab3 %>%
  ggplot(aes(y= gastos_2016, x= reorder(categoriasnew, gastos_2016, face = "bold")))+
  geom_bar(stat = "identity")+ 
  theme(legend.position = 'none') +
  coord_flip() +
  geom_hline(yintercept = mean(tab3$gastos_2016) , linetype = 'longdash', 
             color = 'black', size = 1)+
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000),limits = c(0,40000), expand = c(0,0))+
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y =black.bold.12.text)
 

#---------------------Descritivos #6-----------------------

#Transformando a variável Votos tamanho. Do tipo Labelled double (formato SPSS) em para character, depois factor.

colnames(pref)
pref$cod_votos_tamanho <- as.character(pref$votos_tamanho)
str(pref$cod_votos_tamanho)

pref$desc_votos_tamanho <- factor(pref$cod_votos_tamanho, levels = c("1", "2" , "3", "4", "5"),
              labels = c("Até 5 mil", "5.001 até 10 mil", "10.001 até 50 mil", "50.001 até 200 mil",
                         "Acima de 200 mil"))

str(pref$desc_votos_tamanho)

#Data frame com total de gastos por ano, de acordo com tamanho do município.

tab4 <- pref %>%
  group_by(eleicao,(desc_votos_tamanho)) %>% 
  summarise(mean(total_gastos_candidato))
  names(tab4)[2] <- c("desc_votos_tamanho")
  names(tab4)[3] <- c("total_gastos_candidato")
  
#Média dos gastos com estratégia e estrutura por ano de Eleição
tab5 <- pref %>%
  na.omit() %>%
  group_by(eleicao) %>% 
  summarise(mean(estratégia), mean(estrutura))
  names(tab5)[2:3] <- c("estrategia", "estrutura")
  write.csv2(tab4,".csv",row.names = FALSE)
 
#Criando um novo data frame com as respectivas médias anteriores, uma a cada linha 
tab6 <- data.frame(eleicao = c(2008, 2008, 2012, 2012, 2016, 2016),
                     tipo = c("estrutura", "estrategia","estrutura", "estrategia", "estrutura", "estrategia"),
                     gastos = c(39346.92, 11649.34, 67053.84, 51083.23, 36445.14, 31363.03))

tab6$eleicao <- as.character(tab6$eleicao)

#Desnecessáriooooooo
tab7 <- tab6 %>%
 
#Obtendo as respectivas somas de acordo com tipo de gastos
tab8 <- pref %>%
  na.omit() %>%
  group_by(eleicao) %>% 
  summarize(transportes = sum(gastos_transporte),
            publicidade = sum(gastos_publicidade),
            pesquisas = sum(gastos_pesquisas_eleitorais),
            multas = sum(gastos_multas_eleitorais),
            comite = sum(gastos_comite),
            pessoal = sum(gastos_com_pessoal),
            doação = sum(gastos_doacoe_outros_cand_comi_part),
            nao_especificado = sum(gastos_no_especificado),
            impostos = sum(gastos_impos_encarg),
            reembolso = sum(gastos_reembolso_eleitores),
            terceiros = sum(gastos_serv_terceiros))

#Modificando o data frame tab8, com o mesmo método usado no primeiro data frame sumarizado
tab9 <-as.data.frame(t(tab8))
tab9<-tab9[-c(1), ]
row.names(tab9)
tab9$categoriasnew <- c("Transportes", "Publicidade", "Pesquisas Eleitorais",
                        "Multas Eleitorais", "Comitê", "Com Pessoal", "Doações", "Não Especificado",
                        "Impostos", "Reembolso", "Serviços de Terceiros")

#---------------------Plotando Gráficos #6---------------

#Gráfico 4 - Média dos gastos por número de eleitores e eleição
tab4 %>%
  ggplot(aes(y = total_gastos_candidato, x = desc_votos_tamanho)) +
  geom_bar(stat = "identity", color = "black", fill = "gray")+
  scale_alpha_continuous(expand = c(0,0)) +
  labs(y = 'Despesas em R$', x = NULL) +
  facet_wrap(~eleicao, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.x = black.bold.12.text) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","))



#Gráfico 5 - Média dos gastos de Estrutura e Estratégia por tipo de gastos e eleições
tab6 %>%
  ggplot(aes(eleicao,gastos)) +
  geom_bar(aes(fill = tipo ), position = "dodge", stat = "identity", color = "black") +
  scale_y_continuous(breaks = c(20000, 40000, 60000, 80000,0)) +
  labs(y = "Gastos em R$", x = NULL) +
  scale_fill_brewer(palette = "Greys") +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","))

#Gráfico 10 - Soma da receita por tipo de gastos em 2008

tab9 %>%
  ggplot(aes(y = c(V1), x =reorder(categoriasnew, V1)))+
  geom_bar(stat = "identity")+
  theme(legend.position = 'none') +
  coord_flip()+
  scale_y_continuous(expand = c(0,0), labels = scales::number_format(big.mark = ".", decimal.mark = ","))+
  labs(y = NULL, x = NULL)+
  geom_hline(yintercept = mean(tab9$V1) , linetype = 'longdash', 
             color = 'black', size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = black.bold.12.text)

# Gráfico 11 - Soma da receita por tipo de gastos em 2012

tab9 %>%
  ggplot(aes(y= c(V2), x =reorder(categoriasnew, V2)))+
  geom_bar(stat = "identity")+
  theme(legend.position = 'none') +
  coord_flip()+
  theme_bw (12)+
  scale_y_continuous(expand = c(0,0),labels = scales::number_format(big.mark = ".", decimal.mark = ","))+
  labs(y = NULL, x = NULL)+
  geom_hline(yintercept = mean(tab9$V2)  , linetype = 'longdash', 
             color = 'black', size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = black.bold.12.text)


#Gráfico 12 - Soma da receita por tipo de gastos em 2016

tab9 %>%
  ggplot(aes(y= c(V3), x =reorder(categoriasnew, V3)))+
  geom_bar(stat = "identity")+
  theme(legend.position = 'none') +
  coord_flip()+
  theme_bw (12)+
  scale_y_continuous(expand = c(0,0), labels = scales::number_format(big.mark = ".", decimal.mark = ","))+
  labs(y = NULL, x = NULL)+
  geom_hline(yintercept = mean(tab9$V3)  , linetype = 'longdash', 
             color = 'black', size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = black.bold.12.text)
  

#---------------------Descritivos #7------------

#Tabela 3 - Descritivos dos gastos de campanha com Dimensão Estratégia
tab10 <-pref%>%
  group_by(eleicao)%>%
    summarise(maximo = max(estratégia),
              minimo = min(estratégia),
              soma = sum(estratégia),
              media = mean(estratégia),
              dp = sd(estratégia),
              cv = ((dp/media)*100))
write.csv2(tab10,"DescritivosEstrategia.csv",row.names = FALSE)

#Tabela 2 - Descritivos dos gastos com a Dimensão Estrutura
tab11 <-pref%>%
  na.omit() %>%
  group_by(eleicao)%>%
  summarise(maximo = max(estrutura),
            minimo = min(estrutura),
            soma = sum(estrutura),
            media = mean(estrutura),
            dp = sd(estrutura),
            cv = ((dp/media)*100))
write.csv2(tab10,"DescritivosEstrutura.csv",row.names = FALSE)


#---------------------Plotando Gráficos #7-----------------
#Gráfico 6 - Gastos com as Dimensões Estratégia e Estrutura por total de votos e eleições

#Lembrar de colocar um filtro para tentar juntar as variáveis. 

pref <- na.omit(pref)
head(pref)
    

pref%>%na.omit%>%
  ggplot(aes(x= log(estratégia), y = log(total_votos_cand)))+
  geom_point()+
  theme(legend.position = 'none') +
  geom_smooth(method="lm", color ="blue")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = 'Log do gasto com Estratégia', y ='Log dos votos')+
  facet_grid(cols = vars(eleicao)) +
  stat_cor(method = "pearson", label.x = 0, label.y = 14)
  
  
annotate("text", x = 5, y = 14, label = paste(cor(pref$estratégia, pref$estrutura)))

pref%>%na.omit%>%
  ggplot(aes(x= log(estrutura), y = log(total_votos_cand)))+
  geom_point()+
  theme(legend.position = 'none') +
  geom_smooth(method="lm", color ="blue")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = 'Log do gasto com Estrutura', y ='Log dos votos')+
  facet_grid(cols = vars(eleicao)) +
  stat_cor(method = "pearson", label.x = 0, label.y = 14)


#Gráfico 7 - Gastos com Estratégia por total de votos, tamanho do município e eleições
pref%>%na.omit%>%
  ggplot(aes(y= log(total_votos_cand), x= log(estrutura)))+
  geom_point()+ 
  theme(legend.position = 'none') +
  scale_alpha_continuous(expand = c(0,0))+
  geom_smooth(method="lm", color ="blue")+
  labs(y = 'Log dos votos', x = 'Log do gasto com Estrutura')+
  facet_grid(rows = vars(eleicao), cols = vars(desc_votos_tamanho))+
  stat_cor(method = "pearson", label.x = 0, label.y = 14)
  
#Gráfico 8 - Gastos com Estratégia por total de votos, tamanho do município e eleições
pref%>%na.omit%>%
  ggplot(aes(y= log(total_votos_cand), x= log(estratégia)))+
  geom_point()+ 
  theme(legend.position = 'none') +
  theme_bw (12)+
  geom_smooth(method="lm", color ="blue")+
  labs(y = 'Log dos votos', x = 'Log do gasto com Estratégia')+
  facet_grid(rows = vars(eleicao), cols = vars(desc_votos_tamanho)) +
  stat_cor(method = "pearson", label.x = 0, label.y = 14)+
  scale_alpha_continuous(expand = c(0,0))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","))

#Gráfico 9 - Média do tipo de gasto por status por eleição (IC 95%)

#Outra variável do sppss
pref$cod_status <- as.character(pref$status)
str(pref$cod_status)

pref$desc_status <- factor(pref$cod_status, levels = c("0", "1"),
                                  labels = c("Challenger", "Incumbent"))


#Primeiro passo para o gráfico "barra de erros"
tab12 <- pref %>%
  group_by(eleicao)%>%
  select(estratégia, estrutura, eleicao, desc_status)

#empilhando estratégia e estrutura
tab13<- tab12 %>%
  gather(estrategia, estrutura, - eleicao, - desc_status)

tab13%>%
  ggplot(aes(y = estrutura, x = desc_status, fill = estrategia, color = estrategia))+
  stat_summary(fun.y=mean, geom = "point", position = position_dodge(1), size=1)+
  stat_summary(fun.data=mean_cl_normal, geom = "errorbar", position = position_dodge(1),size=1)+
  facet_grid(cols = vars(eleicao)) +
  labs(x="Status", y="Media do gastos em R$") +
  scale_colour_manual(values = c("black", "grey"))+
  theme(panel.background = element_rect(colour = "black"))

         