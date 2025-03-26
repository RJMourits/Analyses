
  #packages
  library("data.table") 
  library("dplyr")
  library("ggplot2")
  library("lubridate")
  library("tidyr")
  
  #Set the working directory
  rm(list = ls())
  setwd("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen")
  getwd()

  #set province
  provincie <- "Groningen (stad)"

  #Read the data files. Four files; documentation: See folder
  Occupations <- as.data.frame(fread(paste0(provincie, "/Occupations_long.txt"), encoding="UTF-8"))
  Pedigree <- as.data.frame(fread(paste0(provincie, "/Pedigree.txt"), encoding="UTF-8"))
  
  #Zet data goed.
  Pedigree$LastEntryDate <- ymd(Pedigree$LastEntryDate)
  Pedigree$B_date <- ymd(Pedigree$B_date)
  Pedigree$B_max_date <- ymd(Pedigree$B_max_date)
  Pedigree$B_min_date <- ymd(Pedigree$B_min_date)
  Pedigree$D_date <- ymd(Pedigree$D_date)
  Pedigree$M_date_1 <- ymd(Pedigree$M_date_1)
  Pedigree$M_date_2 <- ymd(Pedigree$M_date_2)
  Pedigree$M_date_3 <- ymd(Pedigree$M_date_3)
  Pedigree$M_date_4 <- ymd(Pedigree$M_date_4)
  Pedigree$M_date_5 <- ymd(Pedigree$M_date_5)
  Occupations$Date <- ymd(Occupations$Date)
  
  
  ########################
  # 1. Prepare variables #
  ########################
    
  Sample <- Pedigree[Pedigree$Sex=="f" & !is.na(Pedigree$Id_mother) & !is.na(Pedigree$Id_father),]
  Occ <- Occupations[Occupations$Id_person %in% Sample$Id_person,]
  Occ$year <- year(Occ$Date)
  Occ$Age <- floor(Occ$Age)
  Occ$Cert <- ifelse(Occ$Cert %in% c("Marriage1", "Marriage2", "Marriage3", "Marriage4", "Marriage5"), "Marriage", Occ$Cert)
  #View(as.data.frame(table(Occ[grepl("arbeidster", Occ$Occupation), ]$Occupation)) %>% arrange(-Freq))
  Occ$Occupation[Occ$Occupation %in% c("arbeider", "arbeidster")] <- "arbeider / arbeidster"
  Occ$Occupation[Occ$Occupation %in% c("bakker", "bakster", "broodbakker", "broodbakster")] <- "bakker / bakster / broodbakker / broodbakster"
  Occ$Occupation[Occ$Occupation %in% c("dienstmeid", "dienstmaagd", "dienstbaar")] <- "dienstmeid / dienstmaagd"
  Occ$Occupation[Occ$Occupation %in% c("dagloner", "dagloonster")] <- "dagloner / dagloonster"
  Occ$Occupation[Occ$Occupation %in% c("boerenknecht", "boerenmeid", "landbouwknecht", "landmansknecht")] <- "boerenknecht / boerenmeid /\n landbouwknecht / landmansknecht"
  Occ$Occupation[Occ$Occupation %in% c("fabrieksarbeider", "fabrieksarbeidster")] <- "fabrieksarbeider / fabrieksarbeidster"
  Occ$Occupation[Occ$Occupation %in% c("herbergier", "herbergierster")] <- "herbergier / herbergierster"
  Occ$Occupation[Occ$Occupation %in% c("kleermaker", "kleermaakster")] <- "kleermaker / kleermaakster"
  Occ$Occupation[Occ$Occupation %in% c("koopman", "koopvrouw")] <- "koopman / koopvrouw"
  Occ$Occupation[Occ$Occupation %in% c("landarbeider", "landarbeidster")] <- "landarbeider / landarbeidster"
  Occ$Occupation[Occ$Occupation %in% c("landbouwer", "landbouwster")] <- "landbouwer / landbouwster"
  Occ$Occupation[Occ$Occupation %in% c("landbouwarbeider", "landbouwarbeidster")] <- "landbouwarbeider / landbouwarbeidster"
  Occ$Occupation[Occ$Occupation %in% c("landman", "landvrouw")] <- "landman / landvrouw"
  Occ$Occupation[Occ$Occupation %in% c("los arbeider", "los werkman")] <- "los arbeider / los werkman"
  Occ$Occupation[Occ$Occupation %in% c("particulier", "particuliere")] <- "particulier / particuliere"
  Occ$Occupation[Occ$Occupation %in% c("schoenmaker", "schoenmaakster")] <- "schoenmaker / schoenmaakster"
  Occ$Occupation[Occ$Occupation %in% c("sjouwer", "sjouwerman")] <- "sjouwer / sjouwerman"
  Occ$Occupation[Occ$Occupation %in% c("veldarbeider", "veldarbeidster")] <- "veldarbeider / veldarbeidster"
  Occ$Occupation[Occ$Occupation %in% c("werkman", "werkvrouw")] <- "werkman / werkvrouw"
  Occ$Occupation[Occ$Occupation %in% c("winkelier", "winkelierster")] <- "winkelier / winkelierster"
  write.table(Sample, paste0("ESSHC 2025/", provincie, "/Input/Oorspronkelijke beroepsgegevens vrouwen.csv"), row.names=F, sep=",", quote=T, fileEncoding="UTF-8")
  
  
  ###########################################################
  # 2a. List occupations by certifiate type for each period #
  ###########################################################
    
  saveCohorts <- function(a,b,d){
  Marriage <- Occ %>% arrange(Id_person, Date)
  Marriage <- Marriage[Marriage$Cert=="Marriage",c("Id_person", "year", "Age", "Occupation")]
  Marriage <- Marriage[!duplicated(Marriage$Id_person),]
  Marriage <- Marriage[Marriage$year>=a & Marriage$year<=b,]
  Marriage$year <- NULL
  Death <- Occ[Occ$Cert=="Death",c("Id_person", "Age", "Occupation")]
  B1 <- Occ[Occ$Cert=="B1",c("Id_person", "Age", "Occupation")]
  B2 <- Occ[Occ$Cert=="B2",c("Id_person", "Age", "Occupation")]
  B3 <- Occ[Occ$Cert=="B3",c("Id_person", "Age", "Occupation")]
  D1 <- Occ[Occ$Cert=="D1",c("Id_person", "Age", "Occupation")]
  D2 <- Occ[Occ$Cert=="D2",c("Id_person", "Age", "Occupation")]
  D3 <- Occ[Occ$Cert=="D3",c("Id_person", "Age", "Occupation")]
  
  colnames(Marriage) <- c("Id_person", "Age_marriage", "Occupation_marriage")
  colnames(Death) <- c("Id_person", "Age_death", "Occupation_death")
  colnames(B1) <- c("Id_person", "Age_B1", "Occupation_B1")
  colnames(B2) <- c("Id_person", "Age_B2", "Occupation_B2")
  colnames(B3) <- c("Id_person", "Age_B3", "Occupation_B3")
  colnames(D1) <- c("Id_person", "Age_D1", "Occupation_D1")
  colnames(D2) <- c("Id_person", "Age_D2", "Occupation_D2")
  colnames(D3) <- c("Id_person", "Age_D3", "Occupation_D3")
  
  df <- merge(Marriage, Death, by="Id_person", all.x=T)
  df <- merge(df, B1, by="Id_person", all.x=T)
  df <- merge(df, B2, by="Id_person", all.x=T)
  df <- merge(df, B3, by="Id_person", all.x=T)
  df <- merge(df, D1, by="Id_person", all.x=T)
  df <- merge(df, D2, by="Id_person", all.x=T)
  df <- merge(df, D3, by="Id_person", all.x=T)

  df <- df %>% arrange(Occupation_marriage, Occupation_death, Occupation_B1, Occupation_B2, Occupation_B3, Occupation_D1, Occupation_D2, Occupation_D3)
  assign(d, df, envir = parent.frame())
  }

  saveCohorts(1812, 1849, "df1812")
  saveCohorts(1850, 1874, "df1850")
  saveCohorts(1875, 1899, "df1875")
  saveCohorts(1900, 1929, "df1900")
  saveCohorts(1930, 1939, "df1930")
  ls()
  
  saveOccupations <- function(a,d){
  #marriage
  df_mar <- a %>% group_by(Occupation_marriage) %>% summarise(n=n())
  df_mar$perc <- round(df_mar$n / sum(df_mar$n) * 100, 2)
  #df_mar <- df_mar[df_mar$perc>1,]
  df_mar <- df_mar %>% arrange(-perc)
  colnames(df_mar)[1] <- "Occupation"
  assign(paste0(d, "_mar"), df_mar, envir = parent.frame())
  #death
  df_death <- a %>% filter(!is.na(Occupation_death) & Age_death<50) %>% group_by(Occupation_death) %>% summarise(n=n())
  df_death$perc <- round(df_death$n / sum(df_death$n) * 100, 2)
  #df_death <- df_death[df_death$perc>1,]
  df_death <- df_death %>% arrange(-perc)
  colnames(df_death)[1] <- "Occupation"
  assign(paste0(d, "_death"), df_death, envir = parent.frame())
  #B1
  df_B1 <- a %>% filter(!is.na(Occupation_B1) & Age_D1<50) %>% group_by(Occupation_B1) %>% summarise(n=n())
  df_B1$perc <- round(df_B1$n / sum(df_B1$n) * 100, 2)
  #df_B1 <- df_B1[df_B1$perc>1,]
  df_B1 <- df_B1 %>% arrange(-perc)
  colnames(df_B1)[1] <- "Occupation"
  assign(paste0(d, "_B1"), df_B1, envir = parent.frame())
  #D1
  df_D1 <- a %>% filter(!is.na(Occupation_D1) & Age_D1<50) %>% group_by(Occupation_D1) %>% summarise(n=n())
  df_D1$perc <- round(df_D1$n / sum(df_D1$n) * 100, 2)
  #df_D1 <- df_D1[df_D1$perc>1,]
  df_D1 <- df_D1 %>% arrange(-perc)
  colnames(df_D1)[1] <- "Occupation"
  assign(paste0(d, "_D1"), df_D1, envir = parent.frame())
  }

  saveOccupations(df1812, "df1812")
  saveOccupations(df1850, "df1850")
  saveOccupations(df1875, "df1875")
  saveOccupations(df1900, "df1900")
  saveOccupations(df1930, "df1930")
  
  write.table(df1812_mar, paste0("ESSHC 2025/", provincie, "/occupation tables/df1812_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1812_death, paste0("ESSHC 2025/", provincie, "/occupation tables/df1812_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1812_B1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1812_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1812_D1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1812_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1850_mar, paste0("ESSHC 2025/", provincie, "/occupation tables/df1850_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1850_death, paste0("ESSHC 2025/", provincie, "/occupation tables/df1850_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1850_B1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1850_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1850_D1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1850_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1875_mar, paste0("ESSHC 2025/", provincie, "/occupation tables/df1875_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1875_death, paste0("ESSHC 2025/", provincie, "/occupation tables/df1875_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1875_B1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1875_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1875_D1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1875_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1900_mar, paste0("ESSHC 2025/", provincie, "/occupation tables/df1900_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1900_death, paste0("ESSHC 2025/", provincie, "/occupation tables/df1900_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1900_B1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1900_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1900_D1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1900_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1930_mar, paste0("ESSHC 2025/", provincie, "/occupation tables/df1930_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1930_death, paste0("ESSHC 2025/", provincie, "/occupation tables/df1930_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1930_B1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1930_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1930_D1, paste0("ESSHC 2025/", provincie, "/occupation tables/df1930_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  

  plotOccupation <- function(a) {
  ggplot(data=a[1:10,], aes(x=reorder(Occupation, perc), y=perc, label=perc)) +
      geom_bar(stat="Identity") +
      coord_flip() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = seq(0,90,10), linetype=3, size=.1) +
      theme(panel.background = element_blank(),
            axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title = element_text(size=14),
            strip.background = element_blank(),
            strip.text.x = element_text(size = 16, face="bold"),
            legend.position="bottom", legend.title = element_blank(),
            legend.key=element_blank()) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0),
                         breaks=seq(0,100,by=10), 
                         limit=c(0,101)) +
      labs(x="Occupational title",
           y="Percentage") +
      geom_text(hjust = -0.5, size = 2.5,
                position = position_dodge(width = 1),
                inherit.aes = TRUE)
  }

  plotOccupation(df1812_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1812-1849 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1812_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1812-1849 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1812_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1812-1849 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)

  plotOccupation(df1850_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1850-1874 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1850_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1850-1874 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1850_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1850-1874 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  plotOccupation(df1875_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1875-1899 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1875_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1875-1899 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1875_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1875-1899 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)

  plotOccupation(df1900_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1900-1929 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1900_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1900-1929 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1900_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1900-1929 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)

  plotOccupation(df1930_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1930-1939 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1930_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1930-1939 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1930_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs/1930-1939 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)


  #######################################################################################
  # 2b. List occupations by certifiate type for each period, if all certs are available #
  #######################################################################################
  
  saveOccupations(df1812[!is.na(df1812$Occupation_marriage) & !is.na(df1812$Occupation_death) & !is.na(df1812$Occupation_D1),], "df1812all")
  saveOccupations(df1850[!is.na(df1850$Occupation_marriage) & !is.na(df1850$Occupation_death) & !is.na(df1850$Occupation_D1),], "df1850all")
  saveOccupations(df1875[!is.na(df1875$Occupation_marriage) & !is.na(df1875$Occupation_death) & !is.na(df1875$Occupation_D1),], "df1875all")
  saveOccupations(df1900[!is.na(df1900$Occupation_marriage) & !is.na(df1900$Occupation_death) & !is.na(df1900$Occupation_D1),], "df1900all")
  saveOccupations(df1930[!is.na(df1930$Occupation_marriage) & !is.na(df1930$Occupation_death) & !is.na(df1930$Occupation_D1),], "df1930all")
  
  write.table(df1812all_mar, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1812all_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1812all_death, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1812all_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1812all_B1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1812all_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1812all_D1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1812all_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1850all_mar, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1850all_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1850all_death, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1850all_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1850all_B1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1850all_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1850all_D1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1850all_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1875all_mar, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1875all_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1875all_death, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1875all_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1875all_B1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1875all_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1875all_D1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1875all_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1900all_mar, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1900all_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1900all_death, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1900all_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1900all_B1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1900all_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1900all_D1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1900all_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1930all_mar, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1930all_mar.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1930all_death, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1930all_death.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1930all_B1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1930all_B1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")
  write.table(df1930all_D1, paste0("ESSHC 2025/", provincie, "/occupation tables - marriage + death + child available/df1930all_D1.csv"), sep=",", col.names=T, row.names=F, quote=T, fileEncoding="UTF-8")

  
  plotOccupation(df1812all_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1812-1849 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1812all_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1812-1849 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1812all_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1812-1849 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)

  plotOccupation(df1850all_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1850-1874 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1850all_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1850-1874 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1850all_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1850-1874 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  plotOccupation(df1875all_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1875-1899 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1875all_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1875-1899 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1875all_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1875-1899 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)

  plotOccupation(df1900all_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1900-1929 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1900all_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1900-1929 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupation(df1900all_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1900-1929 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)

  #plotOccupation(df1930all_mar)
  #ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1930-1939 marriage.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  #plotOccupation(df1930all_death)
  #ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1930-1939 death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  #plotOccupation(df1930all_D1)
  #ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/top 10 graphs - marriage + death + child available/1930-1939 D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  
  ##############################################
  # 3a. Make timeline for each certifiate type #
  ##############################################
  
  colnames(df1812_mar) <- c("Occupation", "n_1812", "perc_1812")
  colnames(df1850_mar) <- c("Occupation", "n_1850", "perc_1850")
  colnames(df1875_mar) <- c("Occupation", "n_1875", "perc_1875")
  colnames(df1900_mar) <- c("Occupation", "n_1900", "perc_1900")
  colnames(df1930_mar) <- c("Occupation", "n_1930", "perc_1930")
  df_mar <- merge(df1812_mar, df1850_mar, by="Occupation", all=T)
  df_mar <- merge(df_mar, df1875_mar, by="Occupation", all=T)
  df_mar <- merge(df_mar, df1900_mar, by="Occupation", all=T)
  df_mar <- merge(df_mar, df1930_mar, by="Occupation", all=T)
  df_mar <- df_mar[df_mar$Occupation %in% head(df_mar[order(-df_mar$perc_1812),]$Occupation,3) | 
                   df_mar$Occupation %in% head(df_mar[order(-df_mar$perc_1850),]$Occupation,3) | 
                   df_mar$Occupation %in% head(df_mar[order(-df_mar$perc_1875),]$Occupation,3) | 
                   df_mar$Occupation %in% head(df_mar[order(-df_mar$perc_1900),]$Occupation,3) | 
                   df_mar$Occupation %in% head(df_mar[order(-df_mar$perc_1930),]$Occupation,3) ,]
  df_mar[is.na(df_mar)] <- 0
  df_mar <- df_mar[,c(1,3,5,7,9,11)] %>% pivot_longer(cols = perc_1812:perc_1930 , names_to = "year", values_to = "perc")
  df_mar$year <- ifelse(df_mar$year=="perc_1812", "1812-1849",
                 ifelse(df_mar$year=="perc_1850", "1850-1874",
                 ifelse(df_mar$year=="perc_1875", "1875-1899",
                 ifelse(df_mar$year=="perc_1900", "1900-1929",
                 ifelse(df_mar$year=="perc_1930", "1930-1939", "AAAAHHHHH")))))
  
  colnames(df1812_death) <- c("Occupation", "n_1812", "perc_1812")
  colnames(df1850_death) <- c("Occupation", "n_1850", "perc_1850")
  colnames(df1875_death) <- c("Occupation", "n_1875", "perc_1875")
  colnames(df1900_death) <- c("Occupation", "n_1900", "perc_1900")
  colnames(df1930_death) <- c("Occupation", "n_1930", "perc_1930")
  df_death <- merge(df1812_death, df1850_death, by="Occupation", all=T)
  df_death <- merge(df_death, df1875_death, by="Occupation", all=T)
  df_death <- merge(df_death, df1900_death, by="Occupation", all=T)
  df_death <- merge(df_death, df1930_death, by="Occupation", all=T)
  df_death <- df_death[df_death$Occupation %in% head(df_death[order(-df_death$perc_1812),]$Occupation,3) | 
                   df_death$Occupation %in% head(df_death[order(-df_death$perc_1850),]$Occupation,3) | 
                   df_death$Occupation %in% head(df_death[order(-df_death$perc_1875),]$Occupation,3) | 
                   df_death$Occupation %in% head(df_death[order(-df_death$perc_1900),]$Occupation,3) | 
                   df_death$Occupation %in% head(df_death[order(-df_death$perc_1930),]$Occupation,3) ,]
  df_death[is.na(df_death)] <- 0
  df_death <- df_death[,c(1,3,5,7,9,11)] %>% pivot_longer(cols = perc_1812:perc_1930 , names_to = "year", values_to = "perc")
  df_death$year <- ifelse(df_death$year=="perc_1812", "1812-1849",
                 ifelse(df_death$year=="perc_1850", "1850-1874",
                 ifelse(df_death$year=="perc_1875", "1875-1899",
                 ifelse(df_death$year=="perc_1900", "1900-1929",
                 ifelse(df_death$year=="perc_1930", "1930-1939", "AAAAHHHHH")))))

  colnames(df1812_D1) <- c("Occupation", "n_1812", "perc_1812")
  colnames(df1850_D1) <- c("Occupation", "n_1850", "perc_1850")
  colnames(df1875_D1) <- c("Occupation", "n_1875", "perc_1875")
  colnames(df1900_D1) <- c("Occupation", "n_1900", "perc_1900")
  colnames(df1930_D1) <- c("Occupation", "n_1930", "perc_1930")
  df_D1 <- merge(df1812_D1, df1850_D1, by="Occupation", all=T)
  df_D1 <- merge(df_D1, df1875_D1, by="Occupation", all=T)
  df_D1 <- merge(df_D1, df1900_D1, by="Occupation", all=T)
  df_D1 <- merge(df_D1, df1930_D1, by="Occupation", all=T)
  df_D1 <- df_D1[df_D1$Occupation %in% head(df_D1[order(-df_D1$perc_1812),]$Occupation,3) | 
                   df_D1$Occupation %in% head(df_D1[order(-df_D1$perc_1850),]$Occupation,3) | 
                   df_D1$Occupation %in% head(df_D1[order(-df_D1$perc_1875),]$Occupation,3) | 
                   df_D1$Occupation %in% head(df_D1[order(-df_D1$perc_1900),]$Occupation,3) | 
                   df_D1$Occupation %in% head(df_D1[order(-df_D1$perc_1930),]$Occupation,3) ,]
  df_D1[is.na(df_D1)] <- 0
  df_D1 <- df_D1[,c(1,3,5,7,9,11)] %>% pivot_longer(cols = perc_1812:perc_1930 , names_to = "year", values_to = "perc")
  df_D1$year <- ifelse(df_D1$year=="perc_1812", "1812-1849",
                 ifelse(df_D1$year=="perc_1850", "1850-1874",
                 ifelse(df_D1$year=="perc_1875", "1875-1899",
                 ifelse(df_D1$year=="perc_1900", "1900-1929",
                 ifelse(df_D1$year=="perc_1930", "1930-1939", "AAAAHHHHH")))))
  

  plotOccupationTimeLine <- function(a){
  ggplot(data=a, aes(x=year, y=perc, label=perc, group=Occupation, colour=Occupation, linetype=Occupation)) +
      geom_point() +
      geom_line() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = seq(0,100,10), linetype=3, size=.1) +
      theme(panel.background = element_blank(),
            axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title = element_text(size=14),
            strip.background = element_blank(),
            strip.text.x = element_text(size = 16, face="bold"),
            legend.position="bottom", legend.title = element_blank(),
            legend.key=element_blank()) +
      scale_y_continuous(expand = c(0, 0),
                         breaks=seq(0,100,by=10), 
                         limit=c(0,101)) +
      labs(x="Period",
           y="Percentage")
  }
  plotOccupationTimeLine(df_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/timelines/mar.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupationTimeLine(df_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/timelines/death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupationTimeLine(df_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/timelines/D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)


  ##########################################################################
  # 3b. Make timeline for each certifiate type, if all certs are available #
  ##########################################################################
  
  colnames(df1812all_mar) <- c("Occupation", "n_1812", "perc_1812")
  colnames(df1850all_mar) <- c("Occupation", "n_1850", "perc_1850")
  colnames(df1875all_mar) <- c("Occupation", "n_1875", "perc_1875")
  colnames(df1900all_mar) <- c("Occupation", "n_1900", "perc_1900")
  colnames(df1930all_mar) <- c("Occupation", "n_1930", "perc_1930")
  dfall_mar <- merge(df1812all_mar, df1850all_mar, by="Occupation", all=T)
  dfall_mar <- merge(dfall_mar, df1875all_mar, by="Occupation", all=T)
  dfall_mar <- merge(dfall_mar, df1900all_mar, by="Occupation", all=T)
  dfall_mar <- merge(dfall_mar, df1930all_mar, by="Occupation", all=T)
  dfall_mar <- dfall_mar[dfall_mar$Occupation %in% head(dfall_mar[order(-dfall_mar$perc_1812),]$Occupation,3) | 
                   dfall_mar$Occupation %in% head(dfall_mar[order(-dfall_mar$perc_1850),]$Occupation,3) | 
                   dfall_mar$Occupation %in% head(dfall_mar[order(-dfall_mar$perc_1875),]$Occupation,3) | 
                   dfall_mar$Occupation %in% head(dfall_mar[order(-dfall_mar$perc_1900),]$Occupation,3) | 
                   dfall_mar$Occupation %in% head(dfall_mar[order(-dfall_mar$perc_1930),]$Occupation,3) ,]
  dfall_mar[is.na(dfall_mar)] <- 0
  dfall_mar <- dfall_mar[,c(1,3,5,7,9,11)] %>% pivot_longer(cols = perc_1812:perc_1930 , names_to = "year", values_to = "perc")
  dfall_mar$year <- ifelse(dfall_mar$year=="perc_1812", "1812-1849",
                 ifelse(dfall_mar$year=="perc_1850", "1850-1874",
                 ifelse(dfall_mar$year=="perc_1875", "1875-1899",
                 ifelse(dfall_mar$year=="perc_1900", "1900-1929",
                 ifelse(dfall_mar$year=="perc_1930", "1930-1939", "AAAAHHHHH")))))
  
  colnames(df1812all_death) <- c("Occupation", "n_1812", "perc_1812")
  colnames(df1850all_death) <- c("Occupation", "n_1850", "perc_1850")
  colnames(df1875all_death) <- c("Occupation", "n_1875", "perc_1875")
  colnames(df1900all_death) <- c("Occupation", "n_1900", "perc_1900")
  colnames(df1930all_death) <- c("Occupation", "n_1930", "perc_1930")
  dfall_death <- merge(df1812all_death, df1850all_death, by="Occupation", all=T)
  dfall_death <- merge(dfall_death, df1875all_death, by="Occupation", all=T)
  dfall_death <- merge(dfall_death, df1900all_death, by="Occupation", all=T)
  dfall_death <- merge(dfall_death, df1930all_death, by="Occupation", all=T)
  dfall_death <- dfall_death[dfall_death$Occupation %in% head(dfall_death[order(-dfall_death$perc_1812),]$Occupation,3) | 
                   dfall_death$Occupation %in% head(dfall_death[order(-dfall_death$perc_1850),]$Occupation,3) | 
                   dfall_death$Occupation %in% head(dfall_death[order(-dfall_death$perc_1875),]$Occupation,3) | 
                   dfall_death$Occupation %in% head(dfall_death[order(-dfall_death$perc_1900),]$Occupation,3) | 
                   dfall_death$Occupation %in% head(dfall_death[order(-dfall_death$perc_1930),]$Occupation,3) ,]
  dfall_death[is.na(dfall_death)] <- 0
  dfall_death <- dfall_death[,c(1,3,5,7,9,11)] %>% pivot_longer(cols = perc_1812:perc_1930 , names_to = "year", values_to = "perc")
  dfall_death$year <- ifelse(dfall_death$year=="perc_1812", "1812-1849",
                 ifelse(dfall_death$year=="perc_1850", "1850-1874",
                 ifelse(dfall_death$year=="perc_1875", "1875-1899",
                 ifelse(dfall_death$year=="perc_1900", "1900-1929",
                 ifelse(dfall_death$year=="perc_1930", "1930-1939", "AAAAHHHHH")))))

  colnames(df1812all_D1) <- c("Occupation", "n_1812", "perc_1812")
  colnames(df1850all_D1) <- c("Occupation", "n_1850", "perc_1850")
  colnames(df1875all_D1) <- c("Occupation", "n_1875", "perc_1875")
  colnames(df1900all_D1) <- c("Occupation", "n_1900", "perc_1900")
  colnames(df1930all_D1) <- c("Occupation", "n_1930", "perc_1930")
  dfall_D1 <- merge(df1812all_D1, df1850all_D1, by="Occupation", all=T)
  dfall_D1 <- merge(dfall_D1, df1875all_D1, by="Occupation", all=T)
  dfall_D1 <- merge(dfall_D1, df1900all_D1, by="Occupation", all=T)
  dfall_D1 <- merge(dfall_D1, df1930all_D1, by="Occupation", all=T)
  dfall_D1 <- dfall_D1[dfall_D1$Occupation %in% head(dfall_D1[order(-dfall_D1$perc_1812),]$Occupation,3) | 
                   dfall_D1$Occupation %in% head(dfall_D1[order(-dfall_D1$perc_1850),]$Occupation,3) | 
                   dfall_D1$Occupation %in% head(dfall_D1[order(-dfall_D1$perc_1875),]$Occupation,3) | 
                   dfall_D1$Occupation %in% head(dfall_D1[order(-dfall_D1$perc_1900),]$Occupation,3) | 
                   dfall_D1$Occupation %in% head(dfall_D1[order(-dfall_D1$perc_1930),]$Occupation,3) ,]
  dfall_D1[is.na(dfall_D1)] <- 0
  dfall_D1 <- dfall_D1[,c(1,3,5,7,9,11)] %>% pivot_longer(cols = perc_1812:perc_1930 , names_to = "year", values_to = "perc")
  dfall_D1$year <- ifelse(dfall_D1$year=="perc_1812", "1812-1849",
                 ifelse(dfall_D1$year=="perc_1850", "1850-1874",
                 ifelse(dfall_D1$year=="perc_1875", "1875-1899",
                 ifelse(dfall_D1$year=="perc_1900", "1900-1929",
                 ifelse(dfall_D1$year=="perc_1930", "1930-1939", "AAAAHHHHH")))))

  
  plotOccupationTimeLine(dfall_mar)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/timelines - marriage + death + child available/mar.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupationTimeLine(dfall_death)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/timelines - marriage + death + child available/death.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  plotOccupationTimeLine(dfall_D1)
  ggsave(paste0("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Werkende vrouwen/ESSHC 2025/", provincie, "/timelines - marriage + death + child available/D1.png"), plot = last_plot(), unit="in", dpi=900, width=8, height=6)

