rm(list = ls())

# part I 加载包 & 数据读取 -----------------------------------------------------

## 加载包
packs <- c("readr", "gtsummary", "tidyverse")
invisible(lapply(packs, function(x) suppressMessages(library(x, character.only = T))))

## 导入数据
list.files()
All_files <- list.files("./Original data")
Data_names <- str_sub(All_files, start = 1, end = str_locate(All_files, ".csv")[,1]-1)

for (i in 1:length(All_files)) {
  if(i %in% c(1,10)) {
    assign(paste0(toupper(Data_names[i])),read_csv(paste0("./Original data/", All_files)[i], locale = locale(encoding = "GB18030")))
  } else {
    assign(paste0(toupper(Data_names[i])),read_csv(paste0("./Original data/", All_files)[i], locale = locale(encoding = "UTF-8")))
  }
  
}



# part II 数据清理 ------------------------------------------------------------

# 1 人口学信息(ADSL) ----

## 检查是否有重复数据
if(sum(duplicated(ADSL$USUBJID))) {
  print("有重复")
} else {
  print("没有重复")
}

## 缺失为零
ADSL$Major_surgery <- ifelse(!is.na(ADSL$Major_surgery), 1, 0) %>% as.factor()
ADSL$BMIHIGHFL <- ifelse(!is.na(ADSL$BMIHIGHFL), 0, 1) %>% as.factor()

# =============================================================================.
# 2 不良事件发生信息(ADAE) ----

ADAE$AVAL <- 1
AE_name <- unique(ADAE$AETERM)
ADAE_new <- ADAE %>% 
  pivot_wider(id_cols = USUBJID, names_from = AETERM, values_from = AVAL) %>% 
  mutate_at(.vars = AE_name, .funs = function(x) unlist(map(x, function(x) sum(x, na.rm = T)))) %>% # 计算每个人出现AE的次数
  mutate(出血事件 = apply(.[,AE_name], 1, sum))

# =============================================================================.
# 3 费用信息(ADFEE) ----

## 查看访视期
unique(ADFEE$AVISITN)

## 按照USUBJID 和 PASTGRP(费用类型)求和 ????住院费用是如何分类的
ADFEE_new <- ADFEE %>% 
  group_by(USUBJID, PASTGRP) %>%             # 分组
  mutate(fee = sum(as.numeric(AVAL), na.rm = TRUE)) %>%  # 费用求和
  slice(1) %>%                               # 选取每组的第一个
  pivot_wider(id_cols = USUBJID, names_from = PASTGRP, values_from = fee) %>% # 将长数据转换成宽数据，用PASTGRP命名
  dplyr::select(-last_col()) %>%
  mutate(总费用 = rowSums(across(where(is.numeric))), na.rm = T)  

# =============================================================================.
# 4 生命体征(ADPE) -----

## 查看访视期
unique(ADPE$AVISITN)

## 将长数据转换成宽数据，用PARAMCD和AVISITN合并命名，指标缩写后面的数字代表访视阶段
ADPE_new <- ADPE %>% 
  pivot_wider(id_cols = USUBJID, names_from = c(PARAMCD, AVISITN), values_from = AVAL)

# =============================================================================.
# 5 实验室检测(ALLB) -----
## 将长数据转换成宽数据，用PARAMCD和AVISITN合并命名，指标缩写后面的数字代表访视阶段
ADLB_1 <- ADLB %>% 
  pivot_wider(id_cols = USUBJID, names_from = c(PARAMCD, AVISITN), values_from = AVAL)
ADLB_2 <- ADLB %>% 
  pivot_wider(id_cols = USUBJID, names_from = c(PARAMCD, AVISITN), values_from = BASEIND) %>% 
  rename_with(~ paste0(., "_Normal_or_not"), -USUBJID)
ADLB_new <- ADLB_1 %>% dplyr::left_join(ADLB_2, by = "USUBJID")

# =============================================================================.
# 6 问卷与量表(ADQS) ----

## 查找重复观测 ???? 重复观测处理
# a <- data.frame(table(ADQS$USUBJID, ADQS$PARAM)) %>% arrange(desc(Freq))
# aa <- ADQS[ADQS$USUBJID %in% a$Var1[a$Freq > 1],] %>% arrange(desc(PARAM))
# 
# ## 去除重复数据
# ADQS_new <- ADQS %>% group_by(USUBJID, PARAM) %>% slice(1)
# 
# ## 将长数据转换成宽数据，并计算Carpini评分
# ADQS_new <- ADQS_new %>% 
#   pivot_wider(id_cols = USUBJID, names_from = PARAM, values_from = AVALC) %>% 
#   mutate(Carpini_score = sum(c_across(`AGE_40_60`:`高纤维蛋白原血症`)))

ADQS_new <- ADSL %>% 
  dplyr::select(USUBJID, CAPRINIBL) %>% 
  rename(Carpini_score = CAPRINIBL)

# =============================================================================.
# 7 深静脉血栓(ADVTE) -----

ADVTE_new <- ADVTE %>% 
  pivot_wider(id_cols = c(USUBJID, AVISIT), names_from = PARAM, values_from = AVAL) %>% 
  mutate(VTE_0     = ifelse(AVISIT == "基线期", 1, 0),
         VTE_7        = ifelse(AVISIT %in% c("基线期", "访视1"), 1, 0),
         VTE_14       = ifelse(AVISIT %in% c("基线期", "访视1", "访视2"), 1, 0),
         VTE_time     = VTE发生时间
         # VTE_position = ifelse(发生部位 == "近端", "近端", "远端")
         ) %>% 
  dplyr::select(USUBJID, VTE_0:VTE_time)

# =============================================================================.
# 8 住院时长及入院至手术时间(ADDUR) ----

## 查看访视期
unique(ADDUR$AVISITN)

## 长数据转换成宽数据
ADDUR_new <- ADDUR %>% 
  pivot_wider(id_cols = USUBJID, names_from = PARAM, values_from = AVAL)

# =============================================================================.
# 9 VTE预防用药(ADEX) ----

## 查看访视期
unique(ADEX$AVISITN)

## 规整预防用药名称
ADEX$CMDECOD[str_detect(ADEX$CMDECOD, "肝素")] <- "肝素"
ADEX$CMDECOD[str_detect(ADEX$CMDECOD, "血栓通")] <- "血栓通"

## 规整剂量使用频次
ADEX$AVAL[ADEX$AVAL == "一次"] <- 1
ADEX$AVAL[ADEX$AVAL == "每12小时一次"] <- 0.5
ADEX$AVAL[ADEX$AVAL == "一日一次"] <- 1
ADEX$AVAL[ADEX$AVAL == "一日两次"] <- 0.2

## 计算每日平均用量和累计用量
ADEX_new <- ADEX %>% 
  pivot_wider(id_cols = c(USUBJID, AVISIT, TRTGRP), names_from = c(CMDECOD, PARAM), values_from = AVAL) %>% 
  mutate(肝素_每日每天用量 = map2(肝素_单次使用剂量, 肝素_频次, function(x, y) as.numeric(x)/as.numeric(y)),
         血栓通_每日每天用量 = map2(血栓通_单次使用剂量, 血栓通_频次, function(x, y) as.numeric(x)/as.numeric(y))) %>% 
  mutate(肝素_每疗程用量 =  map2(肝素_每日每天用量, 肝素_持续时长, function(x, y) as.numeric(x)*as.numeric(y)),
         血栓通_每疗程用量 =  map2(血栓通_每日每天用量, 血栓通_持续时长, function(x, y) as.numeric(x)*as.numeric(y))) %>% 
  mutate(肝素_每日平均用量 =  map2(肝素_每疗程用量, 肝素_持续时长, function(x,y) sum(as.numeric(x), na.rm = TRUE)/sum(as.numeric(y), na.rm = TRUE)) %>% unlist(),
         肝素_累计用量 =  map_dbl(肝素_每疗程用量, function(x) sum(x, na.rm = TRUE)),
         血栓通_每日平均用量 =  map2(血栓通_每疗程用量, 血栓通_持续时长, function(x, y) sum(as.numeric(x), na.rm = TRUE)/sum(as.numeric(y), na.rm = TRUE)) %>% unlist(),
         血栓通_累计用量 =  map_dbl(血栓通_每疗程用量, function(x) sum(x, na.rm = TRUE))) %>% 
  dplyr::select(USUBJID,肝素_每日平均用量:血栓通_累计用量)


# =============================================================================.
# 10 伴随治疗(ADPR) -----
# ???? 伴随治疗如何分析
# unique(ADPR$PARAMCD)
# aaa <- ADPR %>% filter(PARAMCD == "ROUTE")
# table(aaa$AVAL)

# =============================================================================.
# 11 合并用药(ADCM) -----

# ???? 伴随治疗如何分
# table(ADCM$CMTRT)


# part III 数据合并 ------------------------------------------------------------

objects()[str_detect(objects(), "new")]
data_comp <- ADSL %>% 
  dplyr::left_join(ADVTE_new, by = "USUBJID") %>% 
  dplyr::left_join(ADFEE_new, by = "USUBJID") %>% 
  dplyr::left_join(ADPE_new, by = "USUBJID") %>% 
  dplyr::left_join(ADLB_new, by = "USUBJID") %>% 
  dplyr::left_join(ADQS_new, by = "USUBJID") %>% 
  dplyr::left_join(ADDUR_new, by = "USUBJID") %>%
  dplyr::left_join(ADEX_new, by = "USUBJID") %>%
  dplyr::left_join(ADAE_new, by = "USUBJID")

## VTE未匹配设置成未发生
data_comp$VTE_14[is.na(data_comp$VTE_14)] <- 0

## AE未匹配设置成未发生

data_comp <- data_comp %>% 
  mutate_at(.vars = AE_name, function(x) {ifelse(is.na(x), 0, x)}) %>% 
  dplyr::mutate(出血事件 = ifelse(is.na(出血事件), 0, 1))

save(data_comp, file = "./Cleaned data/data_com.rdata")

