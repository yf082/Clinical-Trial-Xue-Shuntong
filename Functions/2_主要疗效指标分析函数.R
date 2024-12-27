

#' Title 主要疗效指标分析
#'
#' @param data 数据集
#' @param outcome 不良事件名称，只能一个
#' @param by 用来分组的变量
#' @param group 用来描述的分组
#' @param group_label 给分组一个标签
#' @param control 假设检验方法的控制
#'
#' data = data_comp
#' outcome = "VTE_14"
#' by = "ACTARM"
#' group = c("LMWH_ONLY", "PNS_ONLY")
#' group_label = c("C组", "B组")
#' control = list(conf.level = 0.95,sides = "two.sided", method = "scorecc")
Primary_outcome_analysis <- 
  function(
    data,
    outcome,
    by,
    group,
    group_label,
    control) {
  data_need <- as.data.frame(data)
  data_need <- data_need[data_need[,by] %in% group, c(outcome, by)]
  
  VTE_group1 <- data_need[,outcome][data_need[,by] == group[1]]
  VTE_group2 <- data_need[,outcome][data_need[,by] == group[1]]
  
  size_of_group_1 <- sum(data_need[,by] == group[1])
  size_of_group_2 <- sum(data_need[,by] == group[2])
  vte_of_group_1 <- sum(data_need[,by] == group[1] & data_need[,outcome] == 1)
  vte_of_group_2 <- sum(data_need[,by] == group[2] & data_need[,outcome] == 1)
  non_vte_of_group_1 <- sum(data_need[,by] == group[1] & data_need[,outcome] == 0)
  non_vte_of_group_2 <- sum(data_need[,by] == group[2] & data_need[,outcome] == 0)
  p_of_group_1 <- vte_of_group_1/size_of_group_1
  p_of_group_2 <- vte_of_group_2/size_of_group_2
  
  if (control[[3]] == "na") {
    est_dif <- p_of_group_1 - p_of_group_2
    est_sd <- sqrt(p_of_group_1*(1-p_of_group_1)/size_of_group_1 + p_of_group_2/(1-p_of_group_2)/size_of_group_2)
    est_lci <- est_dif - qnorm(0.975)*est_sd
    est_hci <- est_dif + qnorm(0.975)*est_sd
  } else {
    Test_Res <- DescTools::BinomDiffCI(vte_of_group_1, size_of_group_2, vte_of_group_2, size_of_group_2, conf.level = control[[1]], sides = control[[2]], method = control[[3]])
    est_dif <- Test_Res[1]
    est_lci <- Test_Res[2]
    est_hci <- Test_Res[3]
  }
  
  
  Table1 <- 
    data.frame(matrix(c(c("组别",	"例数", "发生", "不发生",	"发生率"),
                        group_label[1],size_of_group_1,vte_of_group_1,size_of_group_1-vte_of_group_1,sprintf("%0.2f",round(p_of_group_1*100, 2)),
                        group_label[2],size_of_group_2,vte_of_group_2,size_of_group_2-vte_of_group_2,sprintf("%0.2f",round(p_of_group_2*100, 2))), nrow = 3, byrow = T))
  
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