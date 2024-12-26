rm(list = ls())

# part I 加载包 & 数据读取 -----------------------------------------------------

## 加载包
packs <- c("readr", "gtsummary", "tidyverse")
invisible(lapply(packs, function(x) suppressMessages(library(x, character.only = T))))

## 导入数据
All_files <- list.files("./Original data")
Data_names <- str_sub(All_files, start = 1, end = str_locate(All_files, ".csv")[,1]-1)

for (i in 1:length(All_files)) {
  if(i %in% c(3,5,6,9)) {
    assign(paste0(Data_names[i]),read_csv(paste0("./Original data/", All_files)[i], locale = locale(encoding = "GB18030")))
  } else {
    assign(paste0(Data_names[i]),read_csv(paste0("./Original data/", All_files)[i], locale = locale(encoding = "UTF-8")))
  }
  
}


# part II 数据核查 -------------------------------------------------------------

## ADSL 重复观测数据
dat_duplicated_id <- ADSL %>% 
  filter(USUBJID %in% ADSL$USUBJID[duplicated(ADSL$USUBJID)]) %>%
  arrange(USUBJID) 


## 数据清理

### 生成VTE数据集
dat_VTE <- ADVTE %>% 
  pivot_wider(id_cols = c(USUBJID, AVISIT), names_from = PARAM, values_from = AVAL) %>% 
  mutate(VTE_base     = ifelse(AVISIT == "基线期", 1, 0),
         VTE_7        = ifelse(AVISIT %in% c("基线期", "访视1"), 1, 0),
         VTE_14       = ifelse(AVISIT %in% c("基线期", "访视1", "访视2"), 1, 0),
         VTE_time     = VTE发生时间,
         VTE_position = ifelse(发生部位 == "近端", 1, 0)) %>% 
  dplyr::select(USUBJID, VTE_base:VTE_position)

dat_VTE <- ADSL %>% 
  dplyr::left_join(dat_VTE, by = "USUBJID") %>% 
  mutate_at(.vars = c("VTE_base", "VTE_7", "VTE_14"), 
            .funs = function(x) ifelse(is.na(x), 0, x))

# part III 数据分析 ------------------------------------------------------------

group <- dat_VTE$ACTARM
value <- dat_VTE$VTE_14
group_compare <- c("PNS_ONLY", "NON_INT")
group_label <- c("B组", "A组")


efficiency_analysis_before_matching <- function(
    group,
    value,
    group_compare,
    group_label,
    method
    ) {
  
  VTE_group1 <- value[group == group_compare[1]]
  VTE_group2 <- value[group == group_compare[2]]
  
  p1 <- mean(VTE_group1)
  p2 <- mean(VTE_group2)
  v1 <- sum(VTE_group1)
  v2 <- sum(VTE_group2)
  n1 <- length(VTE_group1)
  n2 <- length(VTE_group2)
  
  if (method == "na") {
    est_dif <- p1 - p2
    est_sd <- sqrt(p1*(1-p1)/n1 + p2/(1-p2)/n2)
    est_lci <- est_dif - qnorm(0.975)*est_sd
    est_hci <- est_dif + qnorm(0.975)*est_sd
  } else {
    Test_Res <- DescTools::BinomDiffCI(v1, n1, v2, n2, conf.level = 0.95, sides = "two.sided", method = method)
    est_dif <- Test_Res[1]
    est_lci <- Test_Res[2]
    est_hci <- Test_Res[3]
  }
  
  
  
  
  Table1 <- 
    data.frame(matrix(c(c("组别",	"n", "发生", "不发生",	"发生率"),
                        group_label[1],n1,v1,n1-v1,sprintf("%0.4f",round(p1*100, 2)),
                        group_label[2],n2,v2,n2-v2,sprintf("%0.4f",round(p2*100, 2))), nrow = 3, byrow = T))
  
  Table2 <- 
    data.frame(matrix(c(NA,NA,"95%置信区间","NA",
                        "比较组别",	"差值点估计值",	"下限",	"上限",
                        paste0(group_label[1], " vs. ", group_label[2]), 
                        sprintf("%0.4f",round(est_dif*100, 4)), 
                        sprintf("%0.4f",round(est_lci*100, 4)), 
                        sprintf("%0.4f",round(est_hci*100, 4))),
                      nrow = 3, byrow = T))
  return(list(Table1, Table2))
  
}

# "LMWH_ONLY" "NON_INT"   "PNS_ONLY"  "PNS+LMWH"

Table_compare1 <- 
  efficiency_analysis_before_matching(group = dat_VTE$ACTARM,
                                      value = dat_VTE$VTE_14,
                                      group_compare = c("PNS_ONLY", "NON_INT"),
                                      group_label = c("B组", "A组"),
                                      method = "scorecc")
Table_compare2 <- 
  efficiency_analysis_before_matching(group = dat_VTE$ACTARM,
                                      value = dat_VTE$VTE_14,
                                      group_compare = c("PNS_ONLY", "LMWH_ONLY"),
                                      group_label = c("B组", "C组"),
                                      method = "scorecc")
Table_compare3 <- 
  efficiency_analysis_before_matching(group = dat_VTE$ACTARM,
                                      value = dat_VTE$VTE_14,
                                      group_compare = c("PNS+LMWH", "LMWH_ONLY"),
                                      group_label = c("D组", "C组"),
                                      method = "scorecc")

writexl::write_xlsx(list(Table_compare1[[1]], 
                         Table_compare1[[2]], 
                         Table_compare2[[1]], 
                         Table_compare2[[2]], 
                         Table_compare3[[1]], 
                         Table_compare3[[2]]), path = paste0("结果输出/", "非劣效检验_", "20241210", ".xlsx"), col_names = F)