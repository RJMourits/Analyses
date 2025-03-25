  library("data.table")
  library("openxlsx")
  library("dplyr")
  library("ggplot2")
  library("lubridate")
  
  #clean environment
  rm(list=ls())
  
  
  
  ########################
  #### 0. import data ####
  ########################
  
  SlaveVoyages <- fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/AfricanNamesDatabase.csv", encoding="UTF-8")
  #Angola <- SlaveVoyages[SlaveVoyages$majbuypt %in% c("Nova Redonda", "Quicombo"),]
  Angola <- SlaveVoyages[SlaveVoyages$voyageId==3889,]
  
  
  #read sheet 1
  Servicais <- read.xlsx("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data Tracy/Serviçais, Novo Redondo (August 10 version).xlsx")
  #add date-based id
  Servicais$Id <- ifelse(Servicais$Mês=="Janeiro", 1, 
                         ifelse(Servicais$Mês=="Fevereiro", 2, 
                                ifelse(Servicais$Mês=="Marco", 3, 
                                       ifelse(Servicais$Mês=="Abril", 4, 
                                              ifelse(Servicais$Mês=="Maio", 5, 
                                                     ifelse(Servicais$Mês=="Junho", 6, 
                                                            ifelse(Servicais$Mês=="Julho", 7, 
                                                                   ifelse(Servicais$Mês=="Agosto", 8, 
                                                                          ifelse(Servicais$Mês=="Setembro", 9, 
                                                                                 ifelse(Servicais$Mês=="Outubro", 10, 
                                                                                        ifelse(Servicais$Mês=="Novembro", 11, 
                                                                                               ifelse(Servicais$Mês=="Dezembro", 12, NA))))))))))))
  Servicais$Id <- paste(Servicais$Ano, Servicais$Id, Servicais$Dia, sep="-")
  Servicais$Id <- ymd(Servicais$Id)
  Servicais <- Servicais %>% arrange(Id)
  
  
  ############################################
  #### 2. make frequency tables Servicais ####
  ############################################
  
  wb_Servicais <- createWorkbook()
  
  #table date
  addWorksheet(wb_Servicais, "Date")
  writeData(wb_Servicais, sheet="Date", Servicais %>% group_by(Id) %>% summarise(Frequency=n()) %>% ungroup() %>% mutate(Id=as.character(Id)))
  
  #table name
  addWorksheet(wb_Servicais, "Nomes.dos.serviçais")
  writeData(wb_Servicais, sheet="Nomes.dos.serviçais", Servicais %>% group_by(Nomes.dos.serviçais) %>% summarise(Frequency=n()) %>% ungroup() %>% arrange(-Frequency))
  
  #table sex
  addWorksheet(wb_Servicais, "Sexo")
  writeData(wb_Servicais, sheet="Sexo", Servicais %>% group_by(Sexo) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table naturalidade
  addWorksheet(wb_Servicais, "Naturalidade")
  Ethnicity <- Servicais %>% group_by(Naturalidade_standardized) %>% summarise(Frequency=n()) %>% ungroup()
  Ethnicity$Percentage <- round(Ethnicity$Frequency / sum(Ethnicity$Frequency) * 100,1)
  Ethnicity$Percentage2 <- ifelse(Ethnicity$Frequency==1, NA,
                                  round(Ethnicity$Frequency / sum(Ethnicity[Ethnicity$Frequency>1,]$Frequency) * 100,1) )
  Ethnicity <- Ethnicity %>% arrange(-Frequency, Naturalidade_standardized)
  writeData(wb_Servicais, sheet="Naturalidade", Ethnicity)
  rm(Ethnicity)
  
  #table naturalidade x names
  addWorksheet(wb_Servicais, "Naturalidade_nomes")
  Ethnicity <- Servicais %>% group_by(Nomes.dos.serviçais) %>% summarise(Frequency=n()) %>% ungroup()
  Ethnicity <- Ethnicity[Ethnicity$Frequency>30,]
  Ethnicity <- Servicais[Servicais$Nomes.dos.serviçais %in% Ethnicity$Nomes.dos.serviçais,]
  Ethnicity <- Ethnicity %>% group_by(Nomes.dos.serviçais, Naturalidade) %>% summarise(Frequency=n()) %>% ungroup()
  Ethnicity <- Ethnicity %>% arrange(Nomes.dos.serviçais, Naturalidade)
  #write.xlsx(Ethnicity, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Naturalidade 10 most common names.xlsx")
  writeData(wb_Servicais, sheet="Naturalidade_nomes", Ethnicity)
  rm(Ethnicity)
  
  #table name father
  addWorksheet(wb_Servicais, "Nome.do.pai")
  writeData(wb_Servicais, sheet="Nome.do.pai", Servicais %>% group_by(Nome.do.pai) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table name mother
  addWorksheet(wb_Servicais, "Nome.da.mãe")
  writeData(wb_Servicais, sheet="Nome.da.mãe", Servicais %>% group_by(Nome.da.mãe) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table age
  addWorksheet(wb_Servicais, "Idade")
  writeData(wb_Servicais, sheet="Idade", Servicais %>% group_by(Idade) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table height
  addWorksheet(wb_Servicais, "Altura")
  writeData(wb_Servicais, sheet="Altura", Servicais %>% group_by(Altura) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table welock
  addWorksheet(wb_Servicais, "Estado")
  writeData(wb_Servicais, sheet="Estado", Servicais %>% group_by(Estado) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table religiosity
  addWorksheet(wb_Servicais, "Baptizado")
  writeData(wb_Servicais, sheet="Baptizado", Servicais %>% group_by('Baptizado/por.baptizar') %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Sector
  addWorksheet(wb_Servicais, "Serviços")
  writeData(wb_Servicais, sheet="Serviços", Servicais %>% group_by(Serviços) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Offspring
  addWorksheet(wb_Servicais, "Relação")
  writeData(wb_Servicais, sheet="Relação", Servicais %>% group_by(Relação) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table remarks
  addWorksheet(wb_Servicais, "Falecimentos")
  writeData(wb_Servicais, sheet="Falecimentos", Servicais %>% group_by(Falecimentos) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table remarks
  addWorksheet(wb_Servicais, "Observações")
  writeData(wb_Servicais, sheet="Observações", Servicais %>% group_by(Observações) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Location
  addWorksheet(wb_Servicais, "Resgatados")
  writeData(wb_Servicais, sheet="Resgatados", Servicais %>% group_by(Resgatados) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Agente
  addWorksheet(wb_Servicais, "Agente")
  writeData(wb_Servicais, sheet="Agente", Servicais %>% group_by(Agente) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table destination
  addWorksheet(wb_Servicais, "Destino")
  writeData(wb_Servicais, sheet="Destino", Servicais %>% group_by(Destino) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table farm name
  addWorksheet(wb_Servicais, "Nome.de.fazenda")
  writeData(wb_Servicais, sheet="Nome.de.fazenda", Servicais %>% group_by(Nome.de.fazenda) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Patrão
  addWorksheet(wb_Servicais, "Patrão")
  writeData(wb_Servicais, sheet="Patrão", Servicais %>% group_by(Patrão) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Contract Length
  addWorksheet(wb_Servicais, "Anos")
  writeData(wb_Servicais, sheet="Anos", Servicais %>% group_by(Anos) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Ship
  addWorksheet(wb_Servicais, "Vapor")
  writeData(wb_Servicais, sheet="Vapor", Servicais %>% group_by(Vapor) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Location
  addWorksheet(wb_Servicais, "Lugar")
  writeData(wb_Servicais, sheet="Lugar", Servicais %>% group_by(Lugar) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Curador.Geral
  addWorksheet(wb_Servicais, "Curador.Geral")
  writeData(wb_Servicais, sheet="Curador.Geral", Servicais %>% group_by(Curador.Geral) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Scribe
  addWorksheet(wb_Servicais, "Escrivão")
  writeData(wb_Servicais, sheet="Escrivão", Servicais %>% group_by(Escrivão) %>% summarise(Frequency=n()) %>% ungroup())
  
  saveWorkbook(wb_Servicais, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Frequencies Servicais.xlsx", overwrite = TRUE)
  
  
  
  ###############################################
  #### 3. make frequency tables SlaveVoyages ####
  ###############################################
  
  wb_SlaveVoyages <- createWorkbook()
  
  #table names
  addWorksheet(wb_SlaveVoyages, "Name")
  writeData(wb_SlaveVoyages, sheet="Name", Angola %>% group_by(name) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table age
  addWorksheet(wb_SlaveVoyages, "Age")
  writeData(wb_SlaveVoyages, sheet="Age", Angola %>% group_by(age) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table height
  addWorksheet(wb_SlaveVoyages, "Height")
  writeData(wb_SlaveVoyages, sheet="Height", Angola %>% group_by(height) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table sex
  addWorksheet(wb_SlaveVoyages, "Sex")
  writeData(wb_SlaveVoyages, sheet="Sex", Angola %>% group_by(sexage) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Shipname
  addWorksheet(wb_SlaveVoyages, "Shipname")
  writeData(wb_SlaveVoyages, sheet="Shipname", Angola %>% group_by(shipname) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Year
  addWorksheet(wb_SlaveVoyages, "Year")
  writeData(wb_SlaveVoyages, sheet="Year", Angola %>% group_by(datearr) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Buying Port
  addWorksheet(wb_SlaveVoyages, "BuyingPort")
  writeData(wb_SlaveVoyages, sheet="BuyingPort", Angola %>% group_by(majbuypt) %>% summarise(Frequency=n()) %>% ungroup())
  
  #table Selling Port
  addWorksheet(wb_SlaveVoyages, "SellingPort")
  writeData(wb_SlaveVoyages, sheet="SellingPort", Angola %>% group_by(majselpt) %>% summarise(Frequency=n()) %>% ungroup())
  
  saveWorkbook(wb_SlaveVoyages, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Frequencies SlaveVoyages.xlsx", overwrite = TRUE)
  
  
  
  
  
  
  ##############################
  #### 4. plot age * height ####
  ##############################
  
  #make periods & transform Angola$height from inch to meters
  Servicais$Id2 <- ifelse(is.na(Servicais$Id) | Servicais$Id<"1880-01-01", "1876-1878",
                          ifelse(Servicais$Id>"1880-01-01" & Servicais$Id<"1886-01-01", "1880-1885", "1888-1892"))
  Servicais$Id3 <- ifelse(is.na(Servicais$Id) | Servicais$Id<"1880-01-01", "1876-1878", "1880-1892")
  Servicais$Ano2 <- ifelse(is.na(Servicais$Ano), 1878, Servicais$Ano)
  Angola$height2 <- round(Angola$height*2.54*2)/2/100
  
  #combine SlaveVoyages + Servicais into one df
  df1 <- Servicais[,c("Idade", "Altura", "Sexo", "Id3")]
  colnames(df1) <- c("Age", "Height", "Sex", "Period")
  df2 <- Angola[,c("age", "height2", "sexage")]
  df2$sexage <- ifelse(df2$sexage %in% c("Boy", "Man"), "M",
                       ifelse(df2$sexage %in% c("Girl", "Woman"), "F", NA))
  df2$Period <- "1844"
  colnames(df2) <- c("Age", "Height", "Sex", "Period")
  df <- rbind(df1, df2); rm(df1, df2)
  
  Age <- df[!is.na(df$Age) & !is.na(df$Sex),] %>% group_by(Period, Sex, Age) %>% mutate(n_age=n()) %>% ungroup()
  Age <- Age %>% group_by(Period, Sex) %>% mutate(n_age_group=n()) %>% ungroup()
  Age$perc_age <- Age$n_age / Age$n_age_group * 100
  Age <- Age[!duplicated(Age[,c("Period", "Sex", "Age")]),]
  
  Sex <- df
  Sex$Age2 <- ifelse(is.na(Sex$Age), NA, 
                     ifelse(Sex$Age<14, "<14", "14+"))
  Sex <- Sex %>% group_by(Period, Sex, Age2) %>% summarise(Freq=n()) %>% ungroup()
  
  as.data.frame(table(Servicais[Servicais$Id3=="1876-1878",]$Relação))
  as.data.frame(table(Servicais[Servicais$Id3=="1880-1892",]$Relação))
  
  
  Figure1 <- df %>% filter(Period=="1844") %>% group_by(Age, Height) %>% mutate(n=n()) %>% ungroup()
  Figure1_m <- Figure1[Figure1$Sex=="M",]
  Figure1_f <- Figure1[Figure1$Sex=="F",]
  
  Figure2 <- df %>% filter(Period=="1876-1878") %>% group_by(Age, Height) %>% mutate(n=n()) %>% ungroup()
  Figure2_m <- Figure2[Figure2$Sex=="M",]
  Figure2_f <- Figure2[Figure2$Sex=="F",]
  
  Figure3 <- df %>% filter(Period=="1880-1892") %>% group_by(Age, Height) %>% mutate(n=n()) %>% ungroup()
  Figure3_m <- Figure3[Figure3$Sex=="M",]
  Figure3_f <- Figure3[Figure3$Sex=="F",]
  
  
  Height21_30 <- cbind(df %>% filter(Sex=="F",
                                     Age>=21 & Age<=30) %>% group_by(Period) %>% summarise(Height=round(mean(Height, na.rm=T),3), n=n()) %>% ungroup(),
                       df %>% filter(Sex=="M",
                                     Age>=21 & Age<=30) %>% group_by(Period) %>% summarise(Height=round(mean(Height, na.rm=T),3), n=n()) %>% ungroup())
  Height21_30$Cohort <- paste(as.numeric(substr(Height21_30$Period, 1, 4))-30, 
                              ifelse(nchar(Height21_30$Period)==4, as.numeric(Height21_30$Period)-21, as.numeric(substr(Height21_30$Period, 6, 9))-21),
                              sep="-")
  Height21_30 <- Height21_30[,c(7,3,6,2,5)]
  colnames(Height21_30) <- c("Birth year", "N women (21-30)", "N men (21-30)", "Women (21-30)", "Men (21-30)")
  
  
  
  printfigure_age <- function(Figure, kleur){ggplot(data=Figure, aes(x=Age, y=perc_age)) +
      geom_bar(stat="Identity", fill=kleur) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = seq(0,30,5), linetype=3, size=.1) +
      theme(panel.background = element_blank(),
            axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title = element_text(size=14),
            strip.background = element_blank(),
            strip.text.x = element_text(size = 16, face="bold"),
            legend.position="bottom", legend.title = element_blank(),
            legend.key=element_blank()) +
      scale_x_continuous(expand = c(0, 0),
                         breaks=seq(0,50,by=10), 
                         limit=c(0,50)) +
      scale_y_continuous(expand = c(0, 0),
                         breaks=seq(0,30,by=5), 
                         limit=c(0,30.5)) +
      labs(x="Age",
           y="Percentage")
  }
  
  printfigure1 <- function(Figure, kleur){ggplot(data=Figure, aes(x=Age, y=Height)) +
      geom_point(aes(size=n), colour=kleur) +
      geom_smooth(lwd=1, alpha=.3, se=F, method="loess", colour="black") +
      geom_vline(xintercept = 4.5) +
      geom_hline(yintercept = 0.6) +
      geom_hline(yintercept = seq(0.75,2,.25), linetype=3, size=.1) +
      theme(panel.background = element_blank(),
            axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title = element_text(size=14),
            strip.background = element_blank(),
            strip.text.x = element_text(size = 16, face="bold"),
            legend.position="bottom", legend.title = element_blank(),
            legend.key=element_blank()) +
      scale_x_continuous(expand = c(0, 0),
                         breaks=seq(10,30,by=10), 
                         limit=c(4.5,30.5)) +
      scale_y_continuous(expand = c(0, 0),
                         breaks=seq(0.75,2,by=0.25), 
                         limit=c(0.6,2.01)) +
      labs(x="Age",
           y="Height")
  }
  
  
  printfigure_age(Age[Age$Period=="1844" & Age$Sex=="M",], kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 1a - men - age distribution SlaveVoyages 1844.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age[Age$Period=="1876-1878" & Age$Sex=="M",], kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 1b - men - age distribution Serviçais 1876-1878.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age[Age$Period=="1880-1892" & Age$Sex=="M",], kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 1c - men - age distribution Serviçais 1880-1892.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  printfigure_age(Age[Age$Period=="1844" & Age$Sex=="F",], kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 1a - women - age distribution SlaveVoyages 1844.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age[Age$Period=="1876-1878" & Age$Sex=="F",], kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 1b - women - age distribution Serviçais 1876-1878.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age[Age$Period=="1880-1892" & Age$Sex=="F",], kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 1c - women - age distribution Serviçais 1880-1892.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  
  printfigure1(Figure1_m, kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 2a - men - height by age SlaveVoyages 1844.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure2_m, kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 2b - men - height by age Servicais 1876-1878.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure3_m, kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 2c - men - height by age Servicais 1880-1892.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  printfigure1(Figure1_f, kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 2a - women - height by age SlaveVoyages 1844.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure2_f, kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 2b - women - height by age Servicais 1876-1878.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure3_f, kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 2c - women - height by age Servicais 1880-1892.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  
  write.xlsx(Sex, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Sex by age.xlsx")
  write.xlsx(Height21_30, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Height 21-30 by period.xlsx")
  
  #nagedachte
  Figure4_f <- Servicais[Servicais$Idade>=21 & Servicais$Sexo=="F" & Servicais$Id3=="1880-1892",c("Ano", "Idade", "Altura")]
  Figure4_f$Cohort <- Figure4_f$Ano - Figure4_f$Idade
  Figure4_f <- Figure4_f %>% group_by(Cohort) %>% summarise(n=n(), Altura=mean(Altura, na.rm=T)) %>% ungroup()
  Figure4_m <- Servicais[Servicais$Idade>=21 & Servicais$Sexo=="M" & Servicais$Id3=="1880-1892" & Servicais$Altura>1.30,c("Ano", "Idade", "Altura")]
  Figure4_m$Cohort <- Figure4_m$Ano - Figure4_m$Idade
  Figure4_m <- Figure4_m %>% group_by(Cohort) %>% summarise(n=n(), Altura=mean(Altura, na.rm=T)) %>% ungroup()
  
  printfigure3 <- function(Figure, kleur){ggplot(data=Figure, aes(x=Cohort, y=Altura)) +
      geom_point(aes(size=n), colour=kleur) +
      #geom_smooth(lwd=1, alpha=.3, se=F, method="loess", colour="black") +
      geom_vline(xintercept = 1849.5) +
      geom_hline(yintercept = 1.54) +
      geom_hline(yintercept = seq(1.55,1.75,.05), linetype=3, size=.1) +
      theme(panel.background = element_blank(),
            axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title = element_text(size=14),
            strip.background = element_blank(),
            strip.text.x = element_text(size = 16, face="bold"),
            legend.position="bottom", legend.title = element_blank(),
            legend.key=element_blank()) +
      scale_x_continuous(expand = c(0, 0),
                         breaks=seq(1850,1870,by=1), 
                         limit=c(1849.5,1870.5)) +
      scale_y_continuous(expand = c(0, 0),
                         breaks=seq(1.55,1.75,by=0.05), 
                         limit=c(1.54,1.76)) +
      labs(x="Birth year",
           y="Height")
  }
  
  #View(Figure4_m)
  #printfigure3(Figure4_f, kleur="#8624f5")
  printfigure3(Figure4_m, kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/Figure 3 - men - height by age Servicais 1880-1892.png", plot = last_plot(), unit="in", dpi=900, width=8, height=4)
  


  Servicais1878 <- Servicais[Servicais$Id3=="1876-1878",] %>% group_by(Naturalidade) %>% summarise(n=n()) %>% ungroup() %>% mutate(perc=round(n/sum(n)*100, 2)) %>% arrange(-n, Naturalidade)
  Servicais1878$cumperc <- round(cumsum(Servicais1878$perc),1)
  Servicais1880 <- Servicais[Servicais$Id3=="1880-1892",] %>% group_by(Naturalidade) %>% summarise(n=n()) %>% ungroup() %>% mutate(perc=round(n/sum(n)*100, 2)) %>% arrange(-n, Naturalidade)
  Servicais1880$cumperc <- round(cumsum(Servicais1880$perc),1)
  write.xlsx(Servicais1878, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Naturalidade 1876-1878.xlsx")
  write.xlsx(Servicais1880, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Naturalidade 1880-1892.xlsx")
  