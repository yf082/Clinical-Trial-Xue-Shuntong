#' 数据特征描述
#'
#' @param data 匹配前或匹配后的数据
#' @param outcome 不良事件名称，只能一个
#' @param by 用来分组的变量
#' @param group 用来描述的分组
#' @param vars_of_reg 需要表述的变量
#' @param vars_of_reg 需要描述变量的标签
#' @param type_of_vars 需要描述变量的类型
#'
#' @return
#' @export
#'
#' data = data_matched
#' by = "ACTARM"
#' group = c("LMWH_ONLY", "PNS_ONLY")
#' group_label = c("C组", "B组")
#' vars_of_summary = c("AGE", "SEX", "Carpini_score", "Major_surgery")
#' vars_of_lables = c("年龄", "性别", "Carpini评分", "手术类型")
#' type_of_vars = c("c-n-c-n")
#'
#'
# data = data_comp
# outcome = "VTE_14"
# by = "ACTARM"
# group = c("LMWH_ONLY")
# vars_of_reg = c("AGE", "SEX", "Carpini_score", "Major_surgery")
# vars_of_lables = c("年龄", "性别", "Carpini评分", "手术类型")
# type_of_vars = c("c-n-c-n")

Dominant_diseases_analysis <- 
function(
    data,
    outcome,
    by,
    group,
    vars_of_reg,
    vars_of_lables,
    type_of_vars) {
  ## 提取数据集
  data_need <- as.data.frame(data)
  data_reg <- data_need[data_need[,by] %in% group, c(outcome, vars_of_reg)]
  
  ## 确定变量类型
  if (!is.null(type_of_vars)) {
    type_of_vars <- str_split(type_of_vars, "-")[[1]]
    
    for (i in 1:length(type_of_vars)) {
      if (type_of_vars[i] == "c") {
        data_reg[,vars_of_reg[i]] <- as.numeric(data_reg[,vars_of_reg[i]])
      } else {
        data_reg[,vars_of_reg[i]] <- as.factor(data_reg[,vars_of_reg[i]])
      }
    }
  }
  
  formula <- as.formula(paste0(outcome, "~", "."))
  Res <- broom::tidy(glm(formula, data = data_reg, family = binomial())) %>% 
    slice(-1) %>% 
    reframe(
      自变量 = vars_of_lables,
      Beta = sprintf("%0.2f",round(estimate, 2)),
      SE = sprintf("%0.2f",round(std.error, 2)),
      P = ifelse(p.value < 0.001, "<0.001", sprintf("%0.3f", round(p.value, 3))),
      `OR 95%CI` = sprintf("%0.2f (%0.2f, %0.2f)",
                           round(exp(estimate), 2), 
                           round(exp(estimate - qnorm(0.975)*std.error), 2),
                           round(exp(estimate + qnorm(0.975)*std.error), 2))
    )
  return(Res)
}
  
