#' Title
#'
#' @param data 
#' @param outcome 
#' @param by 
#' @param group 
#' @param group_label 
#' @param fee 
#' @param vars_of_lables 
#' @param type_of_vars 
#'
#' 
#' data = data_comp
#' outcome = c("VTE_14")
#' by = "ACTARM"
#' group = c("LMWH_ONLY", "PNS_ONLY")
#' group_label = c("C组", "B组")
#' fee = c("总费用")
#' vars_of_lables = c("直接医疗费用(元)")
#' type_of_vars = c("c")

Cost_effective_analysis <- 
  function(data,
           outcome,
           by,
           group,
           group_label,
           fee,
           vars_of_lables,
           type_of_vars) {
    data_need <- as.data.frame(data)
    data_need <- data_need[data_need[,by] %in% group, c(outcome, by, fee)]
    VTE_group1 <- data_need[,outcome][data_need[,by] == group[1]]
    VTE_group2 <- data_need[,outcome][data_need[,by] == group[1]]
    size_of_group_1 <- sum(data_need[,by] == group[1])
    size_of_group_2 <- sum(data_need[,by] == group[2])
    vte_of_group_1 <- sum(data_need[,by] == group[1] & data_need[,outcome] == 1)
    vte_of_group_2 <- sum(data_need[,by] == group[2] & data_need[,outcome] == 1)
    p_of_group_1 <- vte_of_group_1/size_of_group_1
    p_of_group_2 <- vte_of_group_2/size_of_group_2
    
    data_need$CRE <- NULL
    data_need$CRE[data_need[,by] == group[1]] <- data_need[,fee][data_need[,by] == group[1]] / p_of_group_1
    data_need$CRE[data_need[,by] == group[2]] <- data_need[,fee][data_need[,by] == group[2]] / p_of_group_1
    
    Describe_characteristics(data_need, by,  group, group_label, c(fee, "CRE"), c(vars_of_lables, "成本效果比"), c("c-c"))
  }
