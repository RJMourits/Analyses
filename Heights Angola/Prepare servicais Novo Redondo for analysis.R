  library("openxlsx")
  library("dplyr")
  
  #clean environment
  rm(list=ls())
  
  
  ########################
  #### 0. import data ####
  ########################
  
  #read sheet 1
  Servicais   <- read.xlsx("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data Tracy/Serviçais, Novo Redondo (February 10 version).xlsx", sheet=1)
  #read other sheet
  l <- list()
  x <- 1
  repeat{
    l[[paste("Servicais", x, sep="")]] <- read.xlsx("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data Tracy/Serviçais, Novo Redondo (February 10 version).xlsx", sheet=x+1)
    if(x==10){
      break
    }
    x <- x+1
  }
  names(l)
  
  #Ethnicity standardization table
  Ethnicity <- read.xlsx("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data Tracy/Naturalidade - standardization.xlsx")
  Ethnicity$Standard <- ifelse(!is.na(Ethnicity$Jelmer), Ethnicity$Jelmer, Ethnicity$Standard)
  Ethnicity <- Ethnicity[,1:2]
  
  
  ###########################
  #### 1. compare Caixas ####
  ###########################
  
  #
  nrow(Servicais) #3,016
  #
  nrow(l$Servicais1) +
  nrow(l$Servicais2) +
  nrow(l$Servicais3) +
  nrow(l$Servicais4) +
  nrow(l$Servicais5) +
  nrow(l$Servicais6) +
  nrow(l$Servicais7) +
  nrow(l$Servicais8) +
  nrow(l$Servicais9) +
  nrow(l$Servicais10) #3,016
  
  #colnames
  colnames(l$Servicais1)[colnames(l$Servicais1)!=colnames(l$Servicais2)]
  colnames(l$Servicais2)[colnames(l$Servicais2)!=colnames(l$Servicais3)]
  colnames(l$Servicais3)[colnames(l$Servicais3)!=colnames(l$Servicais4)]
  colnames(l$Servicais4)[colnames(l$Servicais4)!=colnames(l$Servicais5)]
  colnames(l$Servicais5)[colnames(l$Servicais5)!=colnames(l$Servicais6)]
  colnames(l$Servicais6)[colnames(l$Servicais6)!=colnames(l$Servicais7)]
  colnames(l$Servicais7)[colnames(l$Servicais7)!=colnames(l$Servicais8)]
  colnames(l$Servicais8)[colnames(l$Servicais8)!=colnames(l$Servicais9)]
  colnames(l$Servicais9)[colnames(l$Servicais9)!=colnames(l$Servicais10)]
  
  #compile & compare
  ServicaisAll <- do.call(rbind, l) %>% arrange(Caixa, Número, Número.de.contracto)
  Servicais <- Servicais %>% arrange(Caixa, Número, Número.de.contracto)
  all.equal(Servicais, ServicaisAll)
  
  
  
  #################################
  #### 2. check for duplicates ####
  #################################
  
  length(which(duplicated(ServicaisAll[, c("Número.de.contracto")]))) #141
  length(which(duplicated(ServicaisAll[, c("Caixa", "Número.de.contracto")]))) #141
  x <- ServicaisAll %>% group_by(Caixa, Número.de.contracto) %>% filter(n()>1) %>% ungroup()
  as.data.frame(table(x$Caixa))
  
  
  
  #############################
  #### 3. repair variables ####
  #############################
  
  l <- list()
  x <- 1
  repeat{
    l[[paste("Servicais", x, sep="")]] <- read.xlsx("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data Tracy/Serviçais, Novo Redondo (February 10 version).xlsx", sheet=x+1, na.strings="---")
    if(x==10){
      break
    }
    x <- x+1
  }
  ServicaisAll <- do.call(rbind, l) %>% arrange(Caixa, Número, Número.de.contracto)
  
  #check whether all numeric variables are numeric
  str(ServicaisAll)
  #Contract number
  ServicaisAll[is.na(as.numeric(ServicaisAll$Número.de.contracto)),]
  ServicaisAll$Número.de.contracto[ServicaisAll$Número.de.contracto=="1336"]  <- 1336.1
  ServicaisAll$Número.de.contracto[ServicaisAll$Número.de.contracto=="1336A"] <- 1336.2
  ServicaisAll$Número.de.contracto <- as.numeric(ServicaisAll$Número.de.contracto)
  #Idade / Age
  ServicaisAll[is.na(as.numeric(ServicaisAll$Idade)),]
  #Altura / Height
  table(ServicaisAll$Altura)
  ServicaisAll$Altura2 <- ifelse(grepl("/", ServicaisAll$Altura)==F, NA, gsub(".* ", "", ServicaisAll$Altura))
  ServicaisAll$Altura <- gsub(" ./.", "", ServicaisAll$Altura)
  ServicaisAll$Altura <- gsub(", ", ",", ServicaisAll$Altura)
  ServicaisAll$Altura <- gsub(",", ".", ServicaisAll$Altura)
  ServicaisAll$Altura <- as.numeric(ServicaisAll$Altura)
  ServicaisAll <- ServicaisAll[,c(1:10,27,11:26)]
  #Anos / Time
  table(ServicaisAll$Anos)
  ServicaisAll$Anos <- gsub(" anos", "", ServicaisAll$Anos)
  ServicaisAll$Anos <- as.numeric(ServicaisAll$Anos)
  #
  str(ServicaisAll)
  
  #check other variables
  colnames(ServicaisAll)
  as.data.frame(table(ServicaisAll$Caixa))
  as.data.frame(table(ServicaisAll$Nomes.dos.serviçais))
  as.data.frame(table(ServicaisAll$Sexo))
  length(which(is.na(ServicaisAll$Sexo)))
  as.data.frame(table(ServicaisAll$Naturalidade))
  colnames(Ethnicity) <- c("Naturalidade", "Naturalidade_standardized")
  ServicaisAll <- merge(ServicaisAll, Ethnicity, by="Naturalidade", all=T)
  as.data.frame(table(ServicaisAll$Nome.do.pai))
  as.data.frame(table(ServicaisAll$Nome.da.mãe))
  as.data.frame(table(ServicaisAll$Estado))
  as.data.frame(table(ServicaisAll$'Baptizado/por.baptizar'))
  as.data.frame(table(ServicaisAll$Serviços))
  as.data.frame(table(ServicaisAll$Com.filhos))
  ServicaisAll$Relação <- ifelse(grepl("crian[cç]a", ServicaisAll$Com.filhos), "criança", NA)
  ServicaisAll$Relação <- ifelse(grepl("filho", ServicaisAll$Com.filhos) & grepl("menor", ServicaisAll$Com.filhos), "filho menor", ServicaisAll$Relação)
  ServicaisAll$Relação <- ifelse(grepl("filho", ServicaisAll$Com.filhos) & grepl("menor", ServicaisAll$Com.filhos)==F, "filho", ServicaisAll$Relação)
  ServicaisAll$Relação <- ifelse(grepl("fil?h?a", ServicaisAll$Com.filhos) & grepl("menor", ServicaisAll$Com.filhos), "filha menor", ServicaisAll$Relação)
  ServicaisAll$Relação <- ifelse(grepl("fil?h?a", ServicaisAll$Com.filhos) & grepl("menor", ServicaisAll$Com.filhos)==F, "filha", ServicaisAll$Relação)
  ServicaisAll$Relação <- ifelse(ServicaisAll$Com.filhos=="Tem idem Bumbe", "filha", ServicaisAll$Relação)
  ServicaisAll$Nom.filhos <- gsub(".*nome ", "", ServicaisAll$Com.filhos)
  ServicaisAll$Nom.filhos <- gsub(".*filh. ", "", ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- gsub(" d?e? ?menor.*", "", ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- gsub(" \\([dD]oente)", "", ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- gsub(" \\([fF]aleceu)", "", ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- gsub(",? por baptizar", "", ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- gsub(" com.*", "", ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- gsub(".*chamada ", "", ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- gsub("puta ", "", ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- ifelse(ServicaisAll$Nom.filhos=="Por ter adoecido não seguir para a ilha de S. Thome foi recolhido no hospital para ser tratada", NA, ServicaisAll$Nom.filhos)
  ServicaisAll$Nom.filhos <- ifelse(ServicaisAll$Nom.filhos=="Tem idem Bumbe", "Bumbe", ServicaisAll$Nom.filhos)
  
  ServicaisAll[!is.na(ServicaisAll$Nom.filhos),]$Nom.filhos
  
  as.data.frame(table(ServicaisAll$Falecimentos))
  ServicaisAll[!is.na(ServicaisAll$Observações),"Observações"]
  as.data.frame(table(ServicaisAll$Resgatados))
  as.data.frame(table(ServicaisAll$Agente))
  ServicaisAll$Agente <- gsub("José António Pereira Guimarães, Augusto Moreira Patrício Álvares, e Boaventura Bento Teixeira", "José António Pereira Guimarães, Augusto Moreira Patrício Álvares, Boaventura Bento Teixeira", ServicaisAll$Agente)
  as.data.frame(table(ServicaisAll$Destino))
  as.data.frame(table(ServicaisAll$Nome.de.fazenda))
  ServicaisAll$Nome.de.fazenda <- gsub("Bem posta", "Bemposta", ServicaisAll$Nome.de.fazenda)
  as.data.frame(table(ServicaisAll$Patrão))
  as.data.frame(table(ServicaisAll$Vapor))
  as.data.frame(table(ServicaisAll$Datas))
  as.data.frame(table(ServicaisAll$Datas2))
  ServicaisAll$Datas <- gsub("de Abril 1878", "de Abril de 1878", ServicaisAll$Datas)
  ServicaisAll$Datas <- gsub("do Maio 1878", "do Maio de 1878", ServicaisAll$Datas)
  ServicaisAll$Datas <- gsub("do Junho 1878", "do Junho de 1878", ServicaisAll$Datas)
  ServicaisAll$Lugar <- "Curadoria Geral em Loanda"
  ServicaisAll$Datas2 <- gsub("Curadoria [gG]era[lm] em L[ou]anda ", "", ServicaisAll$Datas) 
  ServicaisAll$Datas2 <- gsub("Curadoria em L[ou]anda ", "", ServicaisAll$Datas2) 
  ServicaisAll$Dia <- as.numeric(gsub(" .*", "", ServicaisAll$Datas2))
  ServicaisAll$Mês <- gsub(" de 1.*", "", ServicaisAll$Datas2)
  ServicaisAll$Mês <- gsub(".* ", "", ServicaisAll$Mês)
  ServicaisAll$Ano <- as.numeric(gsub(".* ", "", ServicaisAll$Datas2))
  ServicaisAll <- ServicaisAll[,c("Caixa", "Número", "Número.de.contracto", 
                                  "Nomes.dos.serviçais", "Sexo", "Naturalidade", "Naturalidade_standardized", "Nome.do.pai", "Nome.da.mãe", 
                                  "Idade", "Altura", "Altura2", "Estado", "Baptizado/por.baptizar", "Serviços", "Com.filhos", "Relação", "Nom.filhos", "Falecimentos", "Observações", 
                                  "Resgatados", "Agente", "Destino", "Nome.de.fazenda", "Patrão", "Anos", "Vapor", "Lugar", "Dia", "Mês", "Ano", 
                                  "Curador.Geral", "Escrivão")]
  as.data.frame(table(ServicaisAll$Curador.Geral))
  as.data.frame(table(ServicaisAll$Escrivão))
  
  
  write.xlsx(ServicaisAll, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data Tracy/Serviçais, Novo Redondo (August 10 version).xlsx")
  write.xlsx(ServicaisAll[!is.na(ServicaisAll$Com.filhos),c("Com.filhos", "Relação", "Nom.filhos")] %>% arrange(Relação, Nom.filhos), "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data Tracy/Com filhos (August 10 version).xlsx")
  
  
  
