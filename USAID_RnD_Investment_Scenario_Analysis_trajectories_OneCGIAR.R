#\\dapadfs\workspace_cluster_6\Socioeconomia\GF_and_SF\USAIDForGFSF
#setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/CSVfiles")
setwd("//dapadfs.cgiarad.org/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/CSVfiles")
options(warn = -1); options(scipen = 999)
library(tidyverse)
#wdir <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/CSVfiles/"
this_folder <-  "//dapadfs.cgiarad.org/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/CSVfiles"
#-----------------------
#filename_vec <- c("ShareAtRiskXagg", "GreenwatXAgg", "BlueWatXAgg")
this_filename <- "PopulationAtRiskXagg.csv"
this_file <- paste0(this_folder, this_filename)
df_raw <- read.csv(this_filename, stringsAsFactors = F)
df_raw <- as.data.frame(df_raw)
for(c in 1:ncol(df_raw)){u <- df_raw[, c]; df_raw[, c] <- gsub("'","",u)}; rm(u)
colnames(df_raw) <- c("Scenario", "Country", "Year", "Val")

this_folder <- "C:/Users/bensc/OneDrive/Documents/"
this_file <- "CG6regions.csv"
this_filepath <- paste0(this_folder, this_file)
df_CG6 <- read.csv(this_filepath, stringsAsFactors = F)

this_file <- "RegionSpecs.csv"
this_filepath <- paste0(this_folder, this_file)
df_key <- read.csv(this_filepath, stringsAsFactors = F)
df_key <- df_key[, c("IMPACT.Region", "IMPACT.Name")]
colnames(df_key) <- c("CTY", "Country")

df_raw <- merge(df_raw, df_key, by = "Country")
df_raw <- merge(df_raw, df_CG6, by = "CTY")

df_raw$Val <- as.numeric(df_raw$Val)
df_raw$Year <- as.integer(df_raw$Year)

df_hunger <- df_raw %>% group_by(Scenario, CG6, Year) %>%
  summarise(Val = sum(Val))

u <- df_hunger$Scenario
ind_rm1 <- grep("IPSL|NoCC", u)
ind_rm2 <- which(is.na(df_hunger$CG6))
ind_rm <- unique(c(ind_rm1, ind_rm2))
df_hunger <- df_hunger[-ind_rm, ]
nonCG6_vec <- c("EUR", "FSU", "OEA", "ROW", "ANZ")
df_hunger <- subset(df_hunger, !(CG6 %in% nonCG6_vec))
df_hungerNoRnD <- subset(df_hunger, Scenario == "SSP2-HGEM2")
df_hunger <- subset(df_hunger, Scenario != "SSP2-HGEM2")
colnames(df_hunger)[ncol(df_hunger)] <- "R&D"
colnames(df_hungerNoRnD)[ncol(df_hungerNoRnD)] <- "No R&D"
df_hungerNoRnD$Scenario <- NULL

df_hunger <- merge(df_hunger, df_hungerNoRnD, by = c("CG6", "Year"))
df_hunger$`Pct. difference` <- (df_hunger$`R&D` / df_hunger$`No R&D` - 1) * 100

df_tabPct <- subset(df_hunger, Year %in% c(2030, 2050))

df_plot <- df_hunger
gg <- ggplot(df_plot, aes(x = Year,
                          y = `Pct. difference`,
                          group = 1))
gg <- gg + geom_line()
gg <- gg + facet_grid(CG6 ~ Scenario)
gg


# QDXAgg
# QFXAgg
# QINTXAgg
# QLXAgg
# QNXAgg
# QOTHRXAgg
# QSupXAgg
# PPXAgg
# YldXAgg
#commodity_vec <- c("R&T-Cassava", "CER-Maize", "CER-Wheat", "CER-Rice", "R&T-Sweet Potato", "R&T-Potato", "R&T-Yams", "AOT-Dairy", "AMT-Poultry", "CER", "R&T")
base_scenario <- "SSP2-HGEM2"
RnD_scenario <- c("SSP2-HGEM-HiNARS2") 
scenario_vec <- c(base_scenario, RnD_scenario)
#scenario_vec <- c("SSP2-HGEM-Pangloss2","SSP2-HGEM-HiYld2","SSP2-HGEM2","SSP2-HGEM-HiREFF2","SSP2-HGEM-HiNARS2","SSP2-HGEM-MMEFF2")
#filename_vec <- c("QDXAgg", "QFXAgg","QNXAgg", "QOTHRXAgg", "QSupXAgg")
#area_vec <- c("SSA", "MEN", "SAS", "LAC", "EAP", "NAM", "EUR", "FSU")
#PUL_commodity_vec <- c("PUL-Beans", "PUL-Chickpeas", "PUL-Cowpeas", "PUL-Lentils",
#                   "PUL-Pigeonpeas")
#CER_commodity_vec <- c("CER-Maize", "CER-Wheat", "CER-Rice")
#commodity_vec <- c(PUL_commodity_vec, CER_commodity_vec)










n_files <- length(filename_vec)
df_list <- list()
for(i in 1:n_files){
  filename <- filename_vec[i]
  open_this <- paste0(wdir, filename, ".csv")
  df_raw <- read.csv(open_this, stringsAsFactors = F)
  df_raw <- as.data.frame(df_raw)
  print(head(df_raw))
  for(c in 1:ncol(df_raw)){u <- df_raw[, c]; df_raw[, c] <- gsub("'","",u)}; rm(u)
  if(filename %in% c("YldXAgg", "AreaXAgg")){
    colnames(df_raw) <- c("Scenario", "Commodity", "Region", "System", "Year", "Value")
  }else{
    colnames(df_raw) <- c("Scenario", "Commodity", "Region", "Year", "Value")
    
  }
  
  df_raw$Value <- as.numeric(df_raw$Value)
  #unique(df_raw$Commodity)
  #unique(df_raw$Scenario)
  df <- subset(df_raw, Scenario %in% scenario_vec)
  df <- subset(df, Region %in% area_vec)
  df <- subset(df, Commodity %in% commodity_vec)
  #df <- subset(df, Year %in% c("2010", "2050"))
  # df <- df %>% spread(Year, Value)
  # df$`Pct Change` <- 100 * (df$`2050` - df$`2010`) / df$`2010`
  # df <- df[, c("Scenario","Region", "Commodity", "Pct Change")]
  # df$Scenario[grep("SSP2-HGEM-MMEFF2", u)] <- "RMM"
  # df$Scenario[grep("SSP2-HGEM-Pangloss2", u)] <- "COMP"
  # df$Scenario[grep("SSP2-HGEM-RegYld2", u)] <- "REGIONAL"
  # df <- df %>% spread(Scenario, `Pct Change`)
  # diff_list <- list()
  # t <- 0
  # for(j in 3:6)
  # {
  #   t <- t + 1
  #   diff_list[[t]] <- df[, j] - df$`SSP2-HGEM2`
  # }
  # df_add <- as.data.frame(do.call(cbind, diff_list))
  # df_wide <- cbind(df, df_add)
  # df_wide[,3:7] <- NULL
  # colnames(df_wide)[3:ncol(df_wide)] <- colnames(df)[3:6]
  # df_wide$Parameter <- filename
  # print(colnames(df_wide))
  # 
  # #print(head(df))
  # df_list[[i]] <- df_wide
  if(filename %in% c("YldXAgg", "AreaXAgg")){
    df_irr <- subset(df, System == "air")
    df_rf <- subset(df, System == "arf")
    df_irr$System <- NULL
    df_rf$System <- NULL
    if(filename == "YldXAgg"){
      df_irr$Parameter <- "Irrigated Yield"
      df_rf$Parameter <- "Rainfed Yield"
      
    }else{
      df_irr$Parameter <- "Irrigated Area"
      df_rf$Parameter <- "Rainfed Area"
      
    }
    
    df <- rbind(df_irr, df_rf)
  }else{
    df$Parameter <- filename
    
  }
  df_list[[i]] <- df
}

#head(df_list[[1]])
#df <- join_all(df_list)
df <- do.call(rbind, df_list)
df <- df %>% spread(Parameter, Value)
df$`Food Demand/Supply Ratio` <- df$QFXAgg / df$QSupXAgg
df$QSupXAgg <- NULL
df$QFXAgg <- NULL
gathercols <- colnames(df)[5:ncol(df)]
df <- df %>% gather_("Parameter", "Value", gathercols)
u <- df$Scenario
df$Scenario[grep("SSP2-HGEM-HiNARS2", u)] <- "HIGH + NARS"
df$Scenario[grep("SSP2-HGEM2", u)] <- "No R&D"
u <- df$Parameter
df$Parameter[grep("PerCapKCalCXAgg", u)] <- "Kcal / capita"
df$Scenario <- factor(df$Scenario, levels = c("No R&D", "HIGH + NARS"), ordered = T)
df$Region <- factor(df$Region, levels = area_vec, ordered = T)
df$Year <- as.integer(df$Year)
color_vec <- c("#E69F00", "#56B4E9")
#-----------------------
df_compare <- subset(df, Commodity %in% CER_commodity_vec)
#-----------------------
this_param <- "Rainfed Yield"
#-----------------------
df_plot <- subset(df_compare, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = Value, group = Scenario, color = Scenario))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Commodity, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 axis.title.y = element_blank())
gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param)
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/CER_YieldRf.png", gg)
#-----------------------
this_param <- "Irrigated Yield"
#-----------------------
df_plot <- subset(df_compare, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = Value, group = Scenario, color = Scenario))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Commodity, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 axis.title.y = element_blank())
gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param)
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/CER_YieldIrr.png", gg)
#-----------------------
#unique(df$Parameter)
this_param <- "Kcal / capita"
#-----------------------
df_plot <- subset(df_compare, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = Value, group = Scenario, color = Scenario))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Commodity, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 axis.title.y = element_blank())
gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param)
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/CER_KcalPerCap.png", gg)

#-----------------------
#unique(df$Parameter)
this_param <- "Food Demand/Supply Ratio"
#-----------------------
df_plot <- subset(df_compare, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = Value, group = Scenario, color = Scenario))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Commodity, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 axis.title.y = element_blank())
gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param)
gg

ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/CER_DemSupRatio.png", gg)


#-----------------------
#unique(df$Parameter)
this_param <- "Rainfed Area"
#-----------------------
df_plot <- subset(df_compare, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = Value, group = Scenario, color = Scenario))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Commodity, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 axis.title.y = element_blank())
gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param)
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/CER_AreaRf.png", gg)

#-----------------------
#unique(df$Parameter)
this_param <- "Irrigated Area"
#-----------------------
df_plot <- subset(df_compare, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = Value, group = Scenario, color = Scenario))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Commodity, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 axis.title.y = element_blank())
gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param)
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/CER_AreaIrr.png", gg)


#============================================
#============================================
unique(df$Parameter)
df$Group <- NA
df$Group[grep("CER", df$Commodity)] <- "CER"
df$Group[grep("PUL", df$Commodity)] <- "PUL"
df_pct <- subset(df, Parameter != "Food Demand/Supply Ratio")
df_diff <- subset(df, Parameter == "Food Demand/Supply Ratio")
df_pct <- df_pct %>% spread(Scenario, Value)
df_pct$`Year by Year Pct. Diff (R&D - No R&D)` <- (df_pct$`HIGH + NARS` - df_pct$`No R&D`) / df_pct$`No R&D` * 100
df_diff <- df_diff %>% spread(Scenario, Value)
df_diff$Diff <- df_diff$`HIGH + NARS` - df_diff$`No R&D`
#-------------------------------------------
this_param <- "Rainfed Yield"
#-------------------------------------------
df_plot <- subset(df_pct, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = `Year by Year Pct. Diff (R&D - No R&D)`, group = Commodity, color = Commodity))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Group, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param, caption = "The R&D scenario is HIGH + NARS")
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/beans_YieldRf_pctDiff.png", gg)


#-------------------------------------------
this_param <- "Kcal / capita"
#-------------------------------------------
df_plot <- subset(df_pct, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = `Year by Year Pct. Diff (R&D - No R&D)`, group = Commodity, color = Commodity))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Group, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param, caption = "The R&D scenario is HIGH + NARS")
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/beans_KcalPerCap_pctDiff.png", gg)


#-------------------------------------------
this_param <- "Irrigated Yield"
#-------------------------------------------
df_plot <- subset(df_pct, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = `Year by Year Pct. Diff (R&D - No R&D)`, group = Commodity, color = Commodity))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Group, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param, caption = "The R&D scenario is HIGH + NARS")
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/beans_YieldIrr_pctDiff.png", gg)


#-------------------------------------------
this_param <- "Rainfed Area"
#-------------------------------------------
df_plot <- subset(df_pct, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = `Year by Year Pct. Diff (R&D - No R&D)`, group = Commodity, color = Commodity))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Group, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param, caption = "The R&D scenario is HIGH + NARS")
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/beans_AreaRf_pctDiff.png", gg)


#-------------------------------------------
this_param <- "Irrigated Area"
#-------------------------------------------
df_plot <- subset(df_pct, Parameter == this_param)
gg <- ggplot(df_plot, aes(x = Year, y = `Year by Year Pct. Diff (R&D - No R&D)`, group = Commodity, color = Commodity))
gg <- gg + geom_line(lwd = 1.2)
gg <- gg + facet_grid(Region ~ Group, scales = "free_y")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#gg <- gg + scale_color_manual(values = color_vec)
gg <- gg + labs(title = this_param, caption = "The R&D scenario is HIGH + NARS")
gg
ggsave("D:/OneDrive - CGIAR/Documents/Beans/USAID RnD Scenario Analysis/beans_AreaIrr_pctDiff.png", gg)

















#df <- df %>% spread(Year, Value)
#--
df_PctChng <- df
df_PctChng$`Pct Change` <- 100 * (df_PctChng$`2050` - df_PctChng$`2010`) / df_PctChng$`2010`
df_PctChng <- df_PctChng[, c("Scenario", "Region", "Commodity", "Parameter", "Pct Change")]
df_PctChng <- df_PctChng %>% spread(Scenario, `Pct Change`)
# diff_list <- list()
# t <- 0
# for(j in 4:7)
# {
#   t <- t + 1
#   diff_list[[t]] <- df_PctChng[, j] - df_PctChng$`SSP2-HGEM2`
# }
# df_PctChng_add <- as.data.frame(do.call(cbind, diff_list))
# df_PctChng_wide <- cbind(df_PctChng, df_PctChng_add)
df_PctChng_wide <- df_PctChng
df_PctChng_wide$Diff <- df_PctChng$`HIGH + NARS` - df_PctChng$`SSP2-HGEM2`
#df_PctChng_wide[,4:8] <- NULL
df_PctChng_wide[,4:5] <- NULL
#colnames(df_PctChng_wide)[4:ncol(df_PctChng_wide)] <- colnames(df_PctChng)[4:7]
colnames(df_PctChng_wide)[4:ncol(df_PctChng_wide)] <- colnames(df_PctChng)[4]
gathercols <- colnames(df_PctChng_wide)[4:ncol(df_PctChng_wide)]
df_PctChng <- df_PctChng_wide %>% gather_("Scenario", "PP Diff", gathercols)
#--
df_Chng <- df
df_Chng$Difference <- df_Chng$`2050` - df_Chng$`2010`
df_Chng <- df_Chng[, c("Scenario", "Region", "Commodity", "Parameter", "Difference")]
df_Chng <- df_Chng %>% spread(Scenario, Difference)
# diff_list <- list()
# t <- 0
# for(j in 4:7)
# {
#   t <- t + 1
#   diff_list[[t]] <- df_Chng[, j] - df_Chng$`SSP2-HGEM2`
# }
# df_Chng_add <- as.data.frame(do.call(cbind, diff_list))
# df_Chng_wide <- cbind(df_Chng, df_Chng_add)
df_Chng_wide <- df_Chng
df_Chng_wide$Diff <- df_Chng$`HIGH + NARS` - df_Chng$`SSP2-HGEM2`
#df_Chng_wide[,4:8] <- NULL
df_Chng_wide[,4:5] <- NULL
#colnames(df_Chng_wide)[4:ncol(df_Chng_wide)] <- colnames(df_Chng)[4:7]
colnames(df_Chng_wide)[4:ncol(df_Chng_wide)] <- colnames(df_Chng)[4]
gathercols <- colnames(df_Chng_wide)[4:ncol(df_Chng_wide)]
df_Chng <- df_Chng_wide %>% gather_("Scenario", "Diff in Diff", gathercols)
