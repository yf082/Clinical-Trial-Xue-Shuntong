#' Title 不良事件的描述
#'
#' @param data 匹配前或匹配后的数据
#' @param by 用来分组的变量
#' @param group 用来描述的分组
#' @param group_label 给分组一个标签
#' @param AE 包含不良事件的变量
#' 
#' data = data_matched
#' by = "ACTARM"
#' group = c("LMWH_ONLY", "PNS_ONLY")
#' AE <- c("皮肤黏膜发生瘀斑或青紫","血红蛋白降低","红细胞降低","黑便","咳血","咯血","尿血","便鲜血","牙龈出血","结膜红肿")

Adverse_events_analysis <- 
  function(
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