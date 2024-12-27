rm(list = ls())

# part I 加载包 & 数据读取 -----------------------------------------------------

## 加载包
Packages <- c("tidyverse", "WeightIt", "MatchIt")
invisible(suppressMessages(lapply(Packages, library, character.only = TRUE)))

## 加载函数
r_files <- list.files(path = "./Functions", pattern = "\\.R$", full.names = TRUE)  
invisible(suppressMessages(lapply(r_files, source)))

## 加载数据
load("./Cleaned data/data_com.rdata")


# part II 数据分析 ------------------------------------------------------------

# data_match <- PSM(data = data_comp,
#                   id = "USUBJID",
#                   center = "SITEID",
#                   group = c("PNS_ONLY", "LMWH_ONLY"),
#                   outcome = "VTE_14",
#                   by = "ACTARM",
#                   covariates = c("AGEGR1", "SEX", "Carpini_score", "Major_surgery"),
#                   group_label = c("C组", "B组"),
#                   method = "optimal",
#                   distance = "mahalanobis",
#                   replace = FALSE,
#                   caliper = 0.2)

# 1 特征描述
Describe_characteristics(data = data_comp,
                         by = "ACTARM",
                         group =  c("LMWH_ONLY", "PNS_ONLY", "NON_INT"),
                         group_label = c("C组", "B组", "A组"),
                         vars_of_summary = c("AGE", "SEX", "Carpini_score", "Major_surgery"),
                         vars_of_lables = c("年龄", "性别", "Carpini评分", "手术类型"),
                         type_of_vars = c("c-n-c-n"))

# 2 主要疗效指标分析
Primary_outcome_analysis(data = data_comp,
                         outcome = "VTE_14",
                         by = "ACTARM",
                         group = c("LMWH_ONLY", "PNS_ONLY"),
                         group_label = c("C组", "B组"),
                         control = list(conf.level = 0.95,sides = "two.sided", method = "scorecc"))


# 3 次要疗效指标分析
Secondary_outcome_analysis(data = data_comp,
                           outcome = "VTE_14",
                           by = "ACTARM",
                           group = c("LMWH_ONLY", "PNS_ONLY"),
                           group_label = c("C组", "B组"),
                           method = "chisq.test")

# 4 不良事件分析
Adverse_events_analysis(data = data_comp,
                        by = "ACTARM",
                        group = c("LMWH_ONLY", "PNS_ONLY"),
                        AE = c("皮肤黏膜发生瘀斑或青紫","血红蛋白降低","红细胞降低","黑便","咳血","咯血","尿血","便鲜血","牙龈出血","结膜红肿"))

# 5 优势病种分析
Dominant_diseases_analysis(data = data_comp,
                           outcome = "VTE_14",
                           by = "ACTARM",
                           group = c("LMWH_ONLY"),
                           vars_of_reg = c("AGE", "SEX", "Carpini_score", "Major_surgery"),
                           vars_of_lables = c("年龄", "性别", "Carpini评分", "手术类型"),
                           type_of_vars = c("c-n-c-n"))

# 6 卫生经济学评价
Cost_effective_analysis(data = data_comp,
                        outcome = c("VTE_14"),
                        by = "ACTARM",
                        group = c("LMWH_ONLY", "PNS_ONLY"),
                        group_label = c("C组", "B组"),
                        fee = c("总费用"),
                        vars_of_lables = c("直接医疗费用(元)"),
                        type_of_vars = c("c"))

