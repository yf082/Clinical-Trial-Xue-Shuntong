rm(list = ls())

# part I 加载包 & 数据读取 -----------------------------------------------------

Packages <- c("tidyverse", "WeightIt", "MatchIt")
lapply(Packages, library, character.only = TRUE)

source("0_函数汇总.R")
load("./Cleaned data/data_com.rdata")

# part II PNS_ONLY（B组）与LMWN_ONLY（C组）的比较 -------------------------------

Table_list <- list()

## 1 查看各中心病用药患者分布 -----
table(data_comp$ACTARM[data_comp$ACTARM %in% c("PNS_ONLY", "LMWH_ONLY")], data_comp$SITEID[data_comp$ACTARM %in% c("PNS_ONLY", "LMWH_ONLY")], useNA = "ifany")

## 2 倾向性得分匹配 -----
B_C <- 
  Efficiency_analysis_after_matching(data = as.data.frame(data_comp),
                                     id = "USUBJID",
                                     center = "SITEID",
                                     group = c("PNS_ONLY", "LMWH_ONLY"),
                                     outcome = "VTE_14",
                                     treat = "ACTARM",
                                     covariates = c("AGEGR1", "SEX", "Carpini_score", "Major_surgery"),
                                     group_label = c("C组", "B组"),
                                     method = "optimal",
                                     distance = "mahalanobis",
                                     replace = FALSE,
                                     caliper = 0.2)

data_matched <- B_C$data_matched
write_csv(data_matched, file = "./Output/data_matched.csv")
Table_list <- B_C$Test

ID <- data_matched[, c("USUBJID","subclass")]
writexl::write_xlsx(ID, path = paste0("./Output/ID ", Sys.Date(), ".xlsx"))

## 3 描述性分析 -----
Table_list[[3]] <- Table_summary(data = data_matched,
                                 by = "ACTARM",
                                 group = c("LMWH_ONLY", "PNS_ONLY"),
                                 group_label = c("C组", "B组"),
                                 vars_of_summary = c("AGE", "SEX", "SMOKEFL", "DRINKFL", "AHFL", "Major_surgery", "BMIHIGHFL", "Carpini_score"),
                                 vars_of_lables = c("年龄", "性别", "吸烟", "饮酒", "过敏", "手术类型", "BMI≤25", "Carpini评分"),
                                 type_of_vars = c("c-b-b-b-b-b-b-c"))


## 4 不良事件 -----
AE <- c("皮肤黏膜发生瘀斑或青紫","血红蛋白降低","红细胞降低","黑便","咳血","咯血","尿血","便鲜血","牙龈出血","结膜红肿", "出血事件")
Table_list[[4]] <- Table_AE(data_matched, 
                            by = "ACTARM",
                            group = c("LMWH_ONLY", "PNS_ONLY"),
                            AE = AE)

Table_list[[5]] <- Table_summary(data = data_comp,
              by = "ACTARM",
              group = c("LMWH_ONLY", "PNS_ONLY"),
              group_label = c("C组", "B组"),
              vars_of_summary = c("Major_surgery", "TRTDURD"),
              vars_of_lables = c("手术类型", "住院时长"),
              type_of_vars = c("b-c"))


Table_list[[6]] <- Table_summary(data = data_matched,
              by = "ACTARM",
              group = c("LMWH_ONLY", "PNS_ONLY"),
              group_label = c("C组", "B组"),
              vars_of_summary = c("Major_surgery", "TRTDURD"),
              vars_of_lables = c("手术类型", "住院时长"),
              type_of_vars = c("b-c"))

writexl::write_xlsx(Table_list, path = "./Output/补充匹配后有效性和安全性分析.xlsx")

