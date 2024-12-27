

# 2 有效性检验函数 ----

# group：数据中只是分组的变量
# value：结局
# group_compare：比较的两个组
# group_label：组的标签(字母代替)
# method：检验使用的方法

Efficiency_analysis_before_matching <- function(
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
    data.frame(matrix(c(c("组别",	"例数", "发生", "不发生",	"发生率"),
                        group_label[1],n1,v1,n1-v1,sprintf("%0.2f",round(p1*100, 2)),
                        group_label[2],n2,v2,n2-v2,sprintf("%0.2f",round(p2*100, 2))), nrow = 3, byrow = T))
  
  Table2 <- 
    data.frame(matrix(c(NA,NA,"95%置信区间","NA",
                        "比较组别",	"差值点估计值",	"下限",	"上限",
                        paste0(group_label[1], " vs. ", group_label[2]), 
                        sprintf("%0.2f",round(est_dif*100, 2)), 
                        sprintf("%0.2f",round(est_lci*100, 2)), 
                        sprintf("%0.2f",round(est_hci*100, 2))),
                      nrow = 3, byrow = T))
  return(list(Table1, Table2))
  
}


# 3 匹配后有效性检验函数 ----

# data = as.data.frame(data_comp)：数据集
# id = "USUBJID"：唯一标识符
# center = "SITEID"：中心
# treat = "ACTARM"：指示分组的变量
# group = c("PNS_ONLY", "NON_INT")：比较的两个组
# group_label = c("B组", "A组")：比较的两个组的标签
# outcome = "VTE_14"：比较的结局
# covariates = c("AGEGR1", "SEX")：用来匹配的协变量

Efficiency_analysis_after_matching <- function(
    data,
    id,
    center,
    group,
    outcome,
    treat,
    covariates,
    group_label,
    method,
    distance,
    replace,
    caliper
) {
  data_match <- data[as_vector(data[, treat]) %in% group, ]
  
  # 将处理因子化
  data_match[, treat] <- factor(data_match[, treat], levels = group[2:1], labels = c(0,1))
  
  formula <- sprintf("%s ~ %s", treat, 
                     paste(covariates, collapse = "+")) %>% as.formula()
  
  data_center_list <- list()
  data_center_model_list <- list()
  center_index <- unique(data_match[,center])
  num_index <- 0
  for (i in 1:length(center_index)) {
    data_center <- data_match[data_match[, center] == center_index[i],] %>% drop_na(all_of(covariates))
    m.out <- matchit(data = data_center,
                     formula,
                     method = method,
                     distance = distance,
                     replace = replace,
                     caliper = caliper)
    data_center_model_list[[i]] <- m.out
    out_center <- match.data(m.out)
    out_center$subclass <- as.numeric(out_center$subclass)
    out_center$subclass <- out_center$subclass + num_index
    num_index <- num_index + nrow(out_center)/2
    
    data_center_list[[i]] <- out_center
  }
  
  data_matched <- dplyr::bind_rows(data_center_list)
  data_matched[, treat] <- factor(data_matched[, treat], levels = c(0,1), labels = group[2:1])
  
  Test <- Efficiency_analysis_before_matching(group = data_matched[,treat],
                                              value = data_matched[,outcome],
                                              group_compare = group,
                                              group_label = group_label,
                                              method = "scorecc")
  return(Out <- list(data_matched = data_matched,
                     Test = Test,
                     PSM_in_center = data_center_model_list))
  
}


# data = data_matched
# by = "ACTARM"
# group = c("LMWH_ONLY", "PNS_ONLY")
# group_label = c("C组", "B组")
# vars_of_summary = c("AGE", "SEX", "Carpini_score", "Major_surgery")
# vars_of_lables = c("年龄", "性别", "Carpini评分", "手术类型")
# type_of_vars = c("c-n-c-n")

# 4 基线等特征描述 -------
Table_summary <- function(
    data,
    by,
    group,
    group_label,
    vars_of_summary,
    vars_of_lables,
    type_of_vars = NULL
) {
  ## 提取数据集
  data_need <- as.data.frame(data)
  data_summary <- data_need[data_need[,by] %in% group, c(by, vars_of_summary)]
  
  ## 确定变量类型
  if (!is.null(type_of_vars)) {
    type_of_vars <- str_split(type_of_vars, "-")[[1]]
    
    for (i in 1:length(type_of_vars)) {
      if (type_of_vars[i] == "c") {
        data_summary[,vars_of_summary[i]] <- as.numeric(data_summary[,vars_of_summary[i]])
      } else {
        data_summary[,vars_of_summary[i]] <- as.factor(data_summary[,vars_of_summary[i]])
      }
    }
  }
  
  ## 对变量做描述
  ### 定义描述函数
  
  #### 连续变量
  summary_of_continuous_variable <- function(x){
    result <- c(paste0(length(x) - sum(is.na(x)),"(",sum(is.na(x)),')'),
                paste0(sprintf("%.2f", mean(x,na.rm=T)),'±', sprintf("%.2f", sd(x,na.rm=T))),
                paste0(sprintf("%.2f", quantile(x,0.5,na.rm=T)),'(',sprintf("%.2f", IQR(x,na.rm = TRUE)),')'),
                paste0(sprintf("%.2f", min(x,na.rm=T)),',', sprintf("%.2f", max(x,na.rm=TRUE)))
    )
    return(result)
  }
  
  #### 分类变量
  summary_of_category_variable <- function(x){
    var_of_levels <- levels(x)
    one_for_each <- c()
    for (j in 1:length(var_of_levels)) {
      one_for_each <- append(one_for_each, paste0(table(x)[j],'(',sprintf("%.1f", (table(x)/sum(table(x))*100)[j]),')')) 
    }
    result <- c(paste0(length(x) - sum(is.na(x)),"(",sum(is.na(x)),')'), one_for_each)
    return(result)
  }
  
  ### 遍历每个变量作描述
  results_of_summary <- list()
  for (i in 1:length(vars_of_summary)) {
    var_of_summary_for_each <- vars_of_summary[i]
    continuous_or_category <- type_of_vars[i]
    if (continuous_or_category == "c") {
      if (i == length(vars_of_summary)) {
        summary_for_each <- data.frame(
          指标 = c(vars_of_lables[i],rep(' ',3)),
          static = c('n(missing)',
                     'Mean±SD',
                     'M(IQR)',
                     'Min, Max'),
          group1 = summary_of_continuous_variable(data_summary[data_summary[,by] == group[1],var_of_summary_for_each]),
          group2 = summary_of_continuous_variable(data_summary[data_summary[,by] == group[2],var_of_summary_for_each]),
          合计 = summary_of_continuous_variable(data_summary[,var_of_summary_for_each])
        ) 
      } else {
        summary_for_each <- data.frame(
          指标 = c(vars_of_lables[i],rep(' ',3)),
          static = c('n(missing)',
                     'Mean±SD',
                     'M(IQR)',
                     'Min, Max'),
          group1 = summary_of_continuous_variable(data_summary[data_summary[,by] == group[1],var_of_summary_for_each]),
          group2 = summary_of_continuous_variable(data_summary[data_summary[,by] == group[2],var_of_summary_for_each]),
          合计 = summary_of_continuous_variable(data_summary[,var_of_summary_for_each])
        ) %>% rbind(rep(NA, 5))
      }
    } else {
      if (i == length(vars_of_summary)) {
        summary_for_each <- data.frame(
          指标 = c(vars_of_lables[i],levels(data_summary[,var_of_summary_for_each])),
          static = c('n(missing)',
                     'n(%)',
                     'n(%)'),
          group1 = summary_of_category_variable(data_summary[data_summary[,by] == group[1],var_of_summary_for_each]),
          group2 = summary_of_category_variable(data_summary[data_summary[,by] == group[2],var_of_summary_for_each]),
          合计 = summary_of_category_variable(data_summary[,var_of_summary_for_each])
        )
      } else {
        summary_for_each <- data.frame(
          指标 = c(vars_of_lables[i],levels(data_summary[,var_of_summary_for_each])),
          static = c('n(missing)',
                     'n(%)',
                     'n(%)'),
          group1 = summary_of_category_variable(data_summary[data_summary[,by] == group[1],var_of_summary_for_each]),
          group2 = summary_of_category_variable(data_summary[data_summary[,by] == group[2],var_of_summary_for_each]),
          合计 = summary_of_category_variable(data_summary[,var_of_summary_for_each])
        ) %>% rbind(rep(NA, 5))
      }
    }
    results_of_summary[[i]] <- summary_for_each
  }
  
  Results <- dplyr::bind_rows(results_of_summary)
  names(Results) <- c("指标", NA, group_label, "合计")
  
  return(Results)
}

# data = data_matched
# by = "ACTARM"
# group = c("LMWH_ONLY", "PNS_ONLY")
# AE <- c("皮肤黏膜发生瘀斑或青紫","血红蛋白降低","红细胞降低","黑便","咳血","咯血","尿血","便鲜血","牙龈出血","结膜红肿")

# 5 不良事件 -----
Table_AE <- function(
    data,
    by,
    group,
    group_label,
    AE) {
  ## 提取数据集
  data_matched <- as.data.frame(data)
  data_summary <- data_matched[data_matched[,by] %in% group, c(by, AE)]
  
  incident_num_1 <- apply(data_summary[data_summary[,by] == group[1], AE], 2, function(x) {sum(x, na.rm = TRUE)})
  patients_num_1 <- rep(nrow(data_summary[data_summary[,by] == group[1], AE]), length(AE))
  patients_rate_1 <- sprintf("%0.2f", round(incident_num_1/patients_num_1*100,2))
  
  incident_num_2 <- apply(data_summary[data_summary[,by] == group[2], AE], 2, function(x) {sum(x, na.rm = TRUE)})
  patients_num_2 <- rep(nrow(data_summary[data_summary[,by] == group[2], AE]), length(AE))
  patients_rate_2 <- sprintf("%0.2f", round(incident_num_2/patients_num_2*100,2))
  
  data_summary <- data_summary %>% 
    mutate_at(.vars = AE, .funs = function(x) ifelse(x == 0, 0, 1))
  
  Res_of_test <- list()
  for (i in 1:length(AE)) {
    if (incident_num_1[i] == 0 & incident_num_2 [i] == 0) {
      Res_of_test[[i]] <- data.frame(NA,NA,NA,NA)
    } else if (incident_num_1[i] == 0 | incident_num_2 [i] == 0) {
      Res_of_test[[i]] <- broom::tidy(fisher.test(table(data_summary[, by],  data_summary[, AE[i]])))[,c(1,2,3,5)] %>% 
        set_names(c("statistic", "p.value", "parameter", "method"))
    } else {
      # Res_of_test[[i]] <- broom::tidy(chisq.test(table(data_summary[, by],  data_summary[, AE[i]])))
      Res_of_test[[i]] <- broom::tidy(fisher.test(table(data_summary[, by],  data_summary[, AE[i]])))[,c(1,2,3,5)] %>% 
        set_names(c("statistic", "p.value", "parameter", "method"))
    }
  }
  res_test <- dplyr::bind_rows(Res_of_test)
  
  RESult <- data.frame(AE, 
                       incident_num_1,
                       patients_num_1,
                       patients_rate_1,
                       rep(NA, length(AE)),
                       incident_num_2,
                       patients_num_2,
                       patients_rate_2,
                       rep(NA, length(AE)),
                       sprintf("%0.2f", round(res_test$statistic, 2)),
                       ifelse(res_test$p.value < 0.001, "<0.001", 
                              sprintf("%0.3f", round(res_test$p.value, 3)))) %>% 
    set_names(c("不良事件", "例次", "例数", "发生率(%)", NA, "例次", "例数", "发生率(%)", NA, "检验统计量", "P"))
  
  return(RESult)
}