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
  
  #Old website
  SlaveVoyages <- fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/AfricanNamesDatabase (2025-03-14).csv", encoding="UTF-8")
  df <- SlaveVoyages[SlaveVoyages$voyageId %in% c(3091),]
  table(df$voyageId)

  #New website
  df2 <- rbind(fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/7659.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/7581.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/7582.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/7208.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1249.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/2464.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1367.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1372.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1396.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1403.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1569.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1631.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1724.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3846.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3855.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3484.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3611.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3613.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3614.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3618.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3670.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3677.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/5035.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/1469.csv", encoding="UTF-8"),
               fread("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Data slavevoyages.org/nieuwe website/3664.csv", encoding="UTF-8"))
  df2$id <- paste0("temp", row.names(df2))
  df2$country <- NA
  df2$"Voyage ID" <- gsub('\\[', '', df2$"Voyage ID")
  df2$"Voyage ID" <- gsub(']', '', df2$"Voyage ID")
  df2$"Ship Name" <- gsub('\\["', '', df2$"Ship Name")
  df2$"Ship Name" <- gsub('"]', '', df2$"Ship Name")
  df2$"Embarkation Region" <- gsub('\\["', '', df2$"Embarkation Region")
  df2$"Embarkation Region" <- gsub('"]', '', df2$"Embarkation Region")
  df2$"Disembarkation Port" <- gsub('\\["', '', df2$"Disembarkation Port")
  df2$"Disembarkation Port" <- gsub('"]', '', df2$"Disembarkation Port")
  df2 <- df2[,c("id", "Documented Name", "Age", "Height", "Gender", "country", "Voyage ID", "Ship Name", "Date of Arrival", "Embarkation Region", "Disembarkation Port", "Sources")]
  colnames(df2) <- colnames(df)
  #bind
  df <- rbind(df, df2)

 
  ##############################
  #### 1. plot age * height ####
  ##############################
  
  df$height2 <- round(df$height*2.54*2)/2/100
  
  #rename SlaveVoyages
  df$sex <- ifelse(df$sexage %in% c("Boy", "Man", 1), "M",
                   ifelse(df$sexage %in% c("Girl", "Woman", 2), "F", NA))
  df$period <- ifelse(df$voyageId %in% c(7659, 7581, 7582, 7508), "1810s", 
                      ifelse(df$voyageId %in% c(1249, 1367, 1372, 1396, 1403, 1569, 1631, 1724, 2464, 3091), "1830s", 
                             ifelse(df$voyageId %in% c(3846, 3855, 3484, 3611, 3613, 3614, 3618, 3670, 3677), "1840s", 
                                    ifelse(df$voyageId %in% c(1469, 3664, 5035), "unknown", NA))))
  df <- df[,c("id", "age", "height2", "sex", "majbuypt", "voyageId", "period", "datearr")]
  colnames(df) <- c("id", "Age", "Height", "Sex", "Departure", "voyageId", "Period", "Year")
  
  #all
  Age <- df[!is.na(df$Age) & !is.na(df$Sex),] %>% group_by(Sex, Age) %>% mutate(n_age=n()) %>% ungroup()
  Age <- Age %>% group_by(Sex) %>% mutate(n_age_group=n()) %>% ungroup()
  Age$perc_age <- Age$n_age / Age$n_age_group * 100
  Age <- Age[!duplicated(Age[,c("Sex", "Age")]),]
  #periode 1
  Age1 <- df[!is.na(df$Age) & !is.na(df$Sex) & df$Period=="1810s",] %>% group_by(Sex, Age) %>% mutate(n_age=n()) %>% ungroup()
  Age1 <- Age1 %>% group_by(Sex) %>% mutate(n_age_group=n()) %>% ungroup()
  Age1$perc_age <- Age1$n_age / Age1$n_age_group * 100
  Age1 <- Age1[!duplicated(Age1[,c("Sex", "Age")]),]
  #periode 2
  Age2 <- df[!is.na(df$Age) & !is.na(df$Sex) & df$Period=="1830s",] %>% group_by(Sex, Age) %>% mutate(n_age=n()) %>% ungroup()
  Age2 <- Age2 %>% group_by(Sex) %>% mutate(n_age_group=n()) %>% ungroup()
  Age2$perc_age <- Age2$n_age / Age2$n_age_group * 100
  Age2 <- Age2[!duplicated(Age2[,c("Sex", "Age")]),]
  #periode 3
  Age3 <- df[!is.na(df$Age) & !is.na(df$Sex) & df$Period=="1840s",] %>% group_by(Sex, Age) %>% mutate(n_age=n()) %>% ungroup()
  Age3 <- Age3 %>% group_by(Sex) %>% mutate(n_age_group=n()) %>% ungroup()
  Age3$perc_age <- Age3$n_age / Age3$n_age_group * 100
  Age3 <- Age3[!duplicated(Age3[,c("Sex", "Age")]),]
  
  #all
  Figure1 <- df %>% group_by(Age, Height) %>% mutate(n=n()) %>% ungroup()
  Figure1_m <- Figure1[Figure1$Sex=="M",]
  Figure1_f <- Figure1[Figure1$Sex=="F",]
  #periode 1
  length(which(df$Period=="1810s" & is.na(df$Height))) #21
  length(which(df$Period=="1810s" & !is.na(df$Height))) #445
  Figure1 <- df[df$Period=="1810s" & !is.na(df$Height),] %>% group_by(Age, Height) %>% mutate(n=n()) %>% ungroup()
  Figure1_m1 <- Figure1[Figure1$Sex=="M",]
  Figure1_f1 <- Figure1[Figure1$Sex=="F",]
  #periode 2
  length(which(df$Period=="1830s" & is.na(df$Height))) #478
  length(which(df$Period=="1830s" & !is.na(df$Height))) #2155
  Figure1 <- df[df$Period=="1830s" & !is.na(df$Height),] %>% group_by(Age, Height) %>% mutate(n=n()) %>% ungroup()
  Figure1_m2 <- Figure1[Figure1$Sex=="M",]
  Figure1_f2 <- Figure1[Figure1$Sex=="F",]
  #periode 3
  length(which(df$Period=="1840s" & is.na(df$Height))) #50
  length(which(df$Period=="1840s" & !is.na(df$Height))) #3143
  Figure1 <- df[df$Period=="1840s" & !is.na(df$Height),] %>% group_by(Age, Height) %>% mutate(n=n()) %>% ungroup()
  Figure1_m3 <- Figure1[Figure1$Sex=="M",]
  Figure1_f3 <- Figure1[Figure1$Sex=="F",]
    
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
                         limit=c(0,50.5)) +
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
  
  
  printfigure_age(Age[Age$Sex=="M",], kleur="#1fc3aa")
  #ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 1 - men - age distribution SlaveVoyages.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age1[Age1$Sex=="M",], kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 1 - men - age distribution SlaveVoyages 1810s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age2[Age2$Sex=="M",], kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 1 - men - age distribution SlaveVoyages 1830s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age3[Age3$Sex=="M",], kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 1 - men - age distribution SlaveVoyages 1840s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  printfigure_age(Age[Age$Sex=="F",], kleur="#8624f5")
  #ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 1 - women - age distribution SlaveVoyages.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age1[Age1$Sex=="F",], kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 1 - women - age distribution SlaveVoyages 1810s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age2[Age2$Sex=="F",], kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 1 - women - age distribution SlaveVoyages 1830s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure_age(Age3[Age3$Sex=="F",], kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 1 - women - age distribution SlaveVoyages 1840s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  printfigure1(Figure1_m, kleur="#1fc3aa")
  #ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 2 - men - height by age SlaveVoyages.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure1_m1, kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 2 - men - height by age SlaveVoyages 1810s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure1_m2, kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 2 - men - height by age SlaveVoyages 1830s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure1_m3, kleur="#1fc3aa")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 2 - men - height by age SlaveVoyages 1840s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  printfigure1(Figure1_f, kleur="#8624f5")
  #ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 2 - women - height by age SlaveVoyages.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure1_f1, kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 2 - women - height by age SlaveVoyages 1810s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure1_f2, kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 2 - women - height by age SlaveVoyages 1830s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  printfigure1(Figure1_f3, kleur="#8624f5")
  ggsave("/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Figure 2 - women - height by age SlaveVoyages 1840s.png", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  
  height21_25 <- df[df$Age>20 & df$Age<=25 & df$Sex=="M",] %>% group_by(voyageId) %>% summarise(Period=first(Period),
                                                                                              Year=first(Year),
                                                                                              n=n(),
                                                                                              available=length(which(!is.na(Height))),
                                                                                              height=round(mean(Height, na.rm=T),3))
  height26_30 <- df[df$Age>26 & df$Age<=30 & df$Sex=="M",] %>% group_by(voyageId) %>% summarise(n=n(),
                                                                                              available=length(which(!is.na(Height))),
                                                                                              height=round(mean(Height, na.rm=T),3))
  height1 <- merge(height21_25, height26_30, by="voyageId", all=T)
  
  height21_25 <- df[df$Age>20 & df$Age<=25 & df$Sex=="M",] %>% group_by(Period) %>% summarise(voyageId="TOTAL",
                                                                                            Year=NA,
                                                                                            n=n(),
                                                                                            available=length(which(!is.na(Height))),
                                                                                            height=round(mean(Height, na.rm=T),3))
  height26_30 <- df[df$Age>26 & df$Age<=30 & df$Sex=="M",] %>% group_by(Period) %>% summarise(n=n(),
                                                                                            available=length(which(!is.na(Height))),
                                                                                            height=round(mean(Height, na.rm=T),3))
  height2 <- merge(height21_25, height26_30, by="Period", all=T)
  height2 <- height2[,c(2:1, 3:9)]
  height <- rbind(height1, height2) %>% arrange(Period, voyageId)
  colnames(height) <- c("voyageId", "Period", "Year", "aged 21-25", "with length known 21-25", "mean height 21-25",
                                                      "aged 26-30", "with length known 26-30", "mean height 26-30")
  height$"difference in cm" <- (height$'mean height 26-30'-height$'mean height 21-25') * 100
  write.xlsx(height, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Height men by agecat and shipment.xlsx")
  
  height21_25 <- df[df$Age>20 & df$Age<=25 & df$Sex=="F",] %>% group_by(voyageId) %>% summarise(Period=first(Period),
                                                                                              Year=first(Year),
                                                                                              n=n(),
                                                                                              available=length(which(!is.na(Height))),
                                                                                              height=round(mean(Height, na.rm=T),3))
  height26_30 <- df[df$Age>26 & df$Age<=30 & df$Sex=="F",] %>% group_by(voyageId) %>% summarise(n=n(),
                                                                                              available=length(which(!is.na(Height))),
                                                                                              height=round(mean(Height, na.rm=T),3))
  height1 <- merge(height21_25, height26_30, by="voyageId", all=T)
  
  height21_25 <- df[df$Age>20 & df$Age<=25 & df$Sex=="F",] %>% group_by(Period) %>% summarise(voyageId="TOTAL",
                                                                                            Year=NA,
                                                                                            n=n(),
                                                                                            available=length(which(!is.na(Height))),
                                                                                            height=round(mean(Height, na.rm=T),3))
  height26_30 <- df[df$Age>26 & df$Age<=30 & df$Sex=="F",] %>% group_by(Period) %>% summarise(n=n(),
                                                                                            available=length(which(!is.na(Height))),
                                                                                            height=round(mean(Height, na.rm=T),3))
  height2 <- merge(height21_25, height26_30, by="Period", all=T)
  height2 <- height2[,c(2:1, 3:9)]
  height <- rbind(height1, height2) %>% arrange(Period, voyageId)
  colnames(height) <- c("voyageId", "Period", "Year", "aged 21-25", "with length known 21-25", "mean height 21-25",
                                                      "aged 26-30", "with length known 26-30", "mean height 26-30")
  height$"difference in cm" <- (height$'mean height 26-30'-height$'mean height 21-25') * 100
  write.xlsx(height, "/home/tuinschepje/Surfdrive/Onderzoek/2023 - Heights Jelmer/Analyse/Figures/temp/Height women by agecat and shipment.xlsx")
  