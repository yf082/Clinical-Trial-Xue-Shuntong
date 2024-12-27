#' Title 次要疗效指标分析
#'
#' @param data 数据集
#' @param outcome 不良事件名称，只能一个
#' @param by 用来分组的变量
#' @param group 用来描述的分组
#' @param group_label 给分组一个标签
#' @param method 假设检验的方法
#'
#' data = data_comp
#' outcome = "VTE_14"
#' by = "ACTARM"
#' group = c("LMWH_ONLY", "PNS_ONLY")
#' group_label = c("C组", "B组")
#' method = "chisq.test"
Secondary_outcome_analysis <- 
  function(
    data,
    outcome,
    by,
    group,
    group_label,
    method) {
  data_need <- as.data.frame(data)
  data_need <- data_need[data_need[,by] %in% group, c(by,outcome)]
  
  size_of_group_1 <- sum(data_need[,by] == group[1])
  size_of_group_2 <- sum(data_need[,by] == group[2])
  vte_of_group_1 <- sum(data_need[,by] == group[1] & data_need[,outcome] == 1)
  vte_of_group_2 <- sum(data_need[,by] == group[2] & data_need[,outcome] == 1)
  non_vte_of_group_1 <- sum(data_need[,by] == group[1] & data_need[,outcome] == 0)
  non_vte_of_group_2 <- sum(data_need[,by] == group[2] & data_need[,outcome] == 0)
  p_of_group_1 <- sprintf("%0.2f", round(vte_of_group_1/size_of_group_1*100,2))
  p_of_group_2 <- sprintf("%0.2f", round(vte_of_group_2/size_of_group_2*100,2))
  broom::tidy(chisq.test(table(data_need[,by], data_need[,outcome])))
  
  Test <- do.call(what = get(method), args = list(table(data_need[,by], data_need[,outcome])))
  statistic <- ifelse(is.null(Test$statistic), NA, sprintf("%0.2f", round(Test$statistic, 2)))
  pvalue <- ifelse(Test$p.value < 0.001, "<0.001", sprintf("%0.3f", round(Test$p.value, 3)))
  
  Res <- data.frame(组别 = group_label, 
                    例数 = c(size_of_group_1, size_of_group_2),
                    发生 = c(vte_of_group_1, vte_of_group_2),
                    不发生 = c(non_vte_of_group_1, non_vte_of_group_1),
                    发生率 = c(p_of_group_1, p_of_group_2),
                    统计量 = c(statistic, NA),
                    P值 = c(pvalue, NA))
  return(Res)
}