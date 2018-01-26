
dff_entrada <- read.csv("data/sincre_2016_data_entrada_1.csv.txt", sep = ";", encoding = "latin1")

dff_entrada$DATA_ENTRADA <- as.character(dff_entrada$DATA_ENTRADA)
dff_entrada$ANO = substr(dff_entrada$DATA_ENTRADA, 1, 4)
dff_entrada$MES = substr(dff_entrada$DATA_ENTRADA, 5, 6)
dff_entrada$DIA = substr(dff_entrada$DATA_ENTRADA, 7, 8)
dff_entrada = dff_entrada %>% mutate(DATA_ENTRADA_DATE = as.POSIXct(strftime(paste(ANO, MES, DIA, sep = "/"), format = "%Y/%m/%d")))

library(dplyr)

# 1 - Quantos imigrantes são casados?
dff_entrada %>% filter(ESTCIV_DESC == "Casado") %>% summarise(total = n())

# 2 - Qual o percentual de imigrantes casados?
dff_entrada %>% filter(ESTCIV_DESC == "Casado") %>% summarise(total = n() / nrow(dff_entrada) * 100)

# 3 - Quantos são do sexo masculino?
dff_entrada %>% filter(SEXO_DESCRICAO == "Masculino") %>% summarise(total = n())
dff_entrada %>% filter(SEXO_DESCRICAO == "Masculino") %>% summarise(total = n() / nrow(dff_entrada) * 100)

dff_entrada %>% filter(SEXO_DESCRICAO == "Feminino") %>% summarise(total = n())
dff_entrada %>% filter(SEXO_DESCRICAO == "Feminino") %>% summarise(total = n() / nrow(dff_entrada) * 100)

# 4 - Quais são os que moram a cidade de Campina Grande? 
cidade <- "CAMPINA GRANDE"
dff_entrada %>% filter(MUNRES_DESC == cidade) 
dff_entrada %>% filter(MUNRES_DESC == cidade) %>% summarise(total = n())
dff_entrada %>% filter(MUNRES_DESC == cidade) %>% summarise(total = n() / nrow(dff_entrada) * 100)

# 5 - Quais são os que moram no mesmo estado que entraram no país? 
dff_entrada %>% filter(as.character(UF_ENTRADA) == as.character(UFRES)) %>% head()

# 6 - Qual o estado em que isso acontece mais?
dff_entrada %>% filter(as.character(UF_ENTRADA) == as.character(UFRES)) %>% group_by(UF_ENTRADA) %>% summarise(total = n())
dff_entrada %>% filter(as.character(UF_ENTRADA) == as.character(UFRES)) %>% group_by(UF_ENTRADA) %>% summarise(total = n()) %>% arrange(desc(total))

# 7 - Qual o município com o maior índice de temporários? 
dff_entrada %>% filter(CLASSIFICACAO == "TEMPORÁRIO") %>% group_by(MUNRES_DESC) %>% summarise(total = n())
dff_entrada %>% filter(CLASSIFICACAO == "TEMPORÁRIO") %>% group_by(MUNRES_DESC) %>% summarise(total = n()) %>% arrange(desc(total))

# 8 - Qual o número de entradas para cada mês? 
dff_entrada %>% group_by(MES) %>% summarise(total = n())
dff_entrada %>% group_by(MES) %>% summarise(total = n()) %>% filter(total == max(total))

# 9 - Qual o númeto de entradas por estado
dff_entrada %>% group_by(UF_ENTRADA) %>% summarise(total = n()) 

# 10 - Quais os diferentes tipos de entrada (classifição)?
unique(dff_entrada$CLASSIFICACAO)

# 11 - Qual o estado com maior e menor número de entradas? 
dff_entrada %>% group_by(UF_ENTRADA) %>% summarise(total = n()) %>% filter(total == max(total))
dff_entrada %>% group_by(UF_ENTRADA) %>% summarise(total = n()) %>% filter(total == min(total))

# 12 - Qual o país com o maior número de emigrantes?
dff_entrada %>% group_by(PNACI_DESC) %>% summarise(total = n()) %>% arrange(desc(total))
dff_entrada %>% group_by(PNACI_DESC) %>% summarise(total = n() / nrow(dff_entrada) * 100) %>% arrange(desc(total))

# 13 - Qual o ano (dentre dois ou mais) que teve o maior número de imigrações?
dff_entrada_2015 <- read.csv("data/sincre_2015_data_entrada.csv.txt", sep = ";", encoding = "latin1")
dff_entrada_2016 <- read.csv("data/sincre_2016_data_entrada_1.csv.txt", sep = ";", encoding = "latin1")
dff_entrada_2 <- rbind(dff_entrada_2015, dff_entrada_2016)

dff_entrada_2 %>% mutate(ANO = substr(DATA_ENTRADA, 1, 4)) %>% group_by(ANO) %>% summarise(total = n())

# 14 - Qual o total dos imigrantes vindos da américa do sul?
dff_entrada %>% filter(CLASSIFICACAO == "FRONTEIRIÇO") %>% summarise(total = n())

# 15 - Qual o percentual de imigrantes vindos da américa do sul?
dff_entrada %>% filter(CLASSIFICACAO == "FRONTEIRIÇO") %>% summarise(total = n() / nrow(dff_entrada) * 100)

# 16 - Qual o percentual de imigrantes pro profissão?
dff_entrada %>% group_by(PROF_DESC) %>% summarise(total = n() / nrow(dff_entrada) * 100)
dff_entrada %>% group_by(PROF_DESC) %>% summarise(total = n() / nrow(dff_entrada) * 100) %>% arrange(desc(total))

# 17 - Qual o percentual de imigrantes pro profissão?
dff_entrada %>% group_by(CLASSIFICACAO) %>% summarise(total = n() / nrow(dff_entrada) * 100)
dff_entrada %>% group_by(CLASSIFICACAO) %>% summarise(total = n() / nrow(dff_entrada) * 100) %>% arrange(desc(total))

# 19 - Qual o percentual de imigrantes nascidos antes de 1990?
dff_entrada %>% mutate(ANO_NASC = substr(DTNASC, 7, 10)) %>% filter(ANO_NASC < 1990) %>% summarise(total = n())
dff_entrada %>% mutate(ANO_NASC = substr(DTNASC, 7, 10)) %>% filter(ANO_NASC < 1990) %>% summarise(total = n() / nrow(dff_entrada) * 100)

# 20 - Qual o número / percentual de imigrantes residentes na paraíba?
dff_entrada %>% filter(UFRES == "PB") %>% summarise(total = n())
dff_entrada %>% filter(UFRES == "PB") %>% summarise(total = n() / nrow(dff_entrada) * 100)

# 21 - Qual a cidade da paraíba com a maior quantidade de imigrantes residentes?
dff_entrada %>% filter(UFRES == "PB") %>% group_by(MUNRES_DESC) %>% summarise(total = n()) %>% arrange(desc(total))

# 22 - Qual o percentual de imigrantes residentes por cidade da paraíba?
dff_entrada %>% filter(UFRES == "PB") %>% mutate(nimigrante = n()) %>% group_by(MUNRES_DESC) %>% summarise(total = n() / first(nimigrante) * 100) %>% arrange(desc(total))

# 23 - Qual o dia de cada mês com mais entradas?
dff_entrada %>% group_by(MES, DIA) %>% summarise(total = n()) %>% group_by(MES) %>% arrange(MES, desc(total)) %>% summarise(DIA = first(DIA), total = first(total))
# como calcular o percentual por mês?

# 24 - Qual o mês que teve mais / menos entradas no estado da paraíba?
dff_entrada %>% filter(UFRES == "PB") %>% group_by(MES) %>% summarise(total = n()) %>% arrange(desc(total))

# 25 - Qual o mês que teve mais / menos entradas na cidade de campina grande?
dff_entrada %>% filter(MUNRES_DESC == "CAMPINA GRANDE") %>% group_by(MES) %>% summarise(total = n()) %>% arrange(desc(total))

# 26 - Qual a média/mediana de entrada por mês em cada estado?
dff_entrada %>% group_by(UF_ENTRADA) %>% summarise(total = n() / 12)
dff_entrada %>% group_by(UF_ENTRADA) %>% summarise(total = n() / 12) %>% arrange(desc(total))
