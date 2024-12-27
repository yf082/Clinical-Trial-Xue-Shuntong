#' 数据特征描述
#'
#' @param data 匹配前或匹配后的数据
#' @param by 用来分组的变量
#' @param group 用来描述的分组
#' @param group_label 给分组一个标签
#' @param vars_of_summary 需要表述的变量
#' @param vars_of_lables 需要描述变量的标签
#' @param type_of_vars 需要描述变量的类型
#'
#' @return
#' @export
#'
#' data = data_comp
#' by = "ACTARM"
#' group = c("LMWH_ONLY", "PNS_ONLY", "NON_INT")
#' group_label = c("C组", "B组", "C组")
#' vars_of_summary = c("AGE", "SEX", "Carpini_score", "Major_surgery")
#' vars_of_lables = c("年龄", "性别", "Carpini评分", "手术类型")
#' type_of_vars = c("c-n-c-n")
#' 
#'
Describe_characteristics <- function(
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
  
  group_levels <- length(group)
  
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
          suppressMessages(
            map_dfc(1:group_levels, function(x) summary_of_continuous_variable(data_summary[data_summary[,by] == group[x],var_of_summary_for_each]))),
          合计 = summary_of_continuous_variable(data_summary[,var_of_summary_for_each])
        ) 
      } else {
        summary_for_each <- data.frame(
          指标 = c(vars_of_lables[i],rep(' ',3)),
          static = c('n(missing)',
                     'Mean±SD',
                     'M(IQR)',
                     'Min, Max'),
          suppressMessages(
            map_dfc(1:group_levels, function(x) summary_of_continuous_variable(data_summary[data_summary[,by] == group[x],var_of_summary_for_each]))),
          合计 = summary_of_continuous_variable(data_summary[,var_of_summary_for_each])
        ) %>% rbind(rep(NA, group_levels + 3))
      }
    } else {
      if (i == length(vars_of_summary)) {
        summary_for_each <- data.frame(
          指标 = c(vars_of_lables[i],levels(data_summary[,var_of_summary_for_each])),
          static = c('n(missing)',
                     'n(%)',
                     'n(%)'),
          suppressMessages(
            map_dfc(1:group_levels, function(x) summary_of_category_variable(data_summary[data_summary[,by] == group[x],var_of_summary_for_each]))),
          合计 = summary_of_category_variable(data_summary[,var_of_summary_for_each])
        )
      } else {
        summary_for_each <- data.frame(
          指标 = c(vars_of_lables[i],levels(data_summary[,var_of_summary_for_each])),
          static = c('n(missing)',
                     'n(%)',
                     'n(%)'),
          suppressMessages(
            map_dfc(1:group_levels, function(x) summary_of_category_variable(data_summary[data_summary[,by] == group[x],var_of_summary_for_each]))),
          合计 = summary_of_category_variable(data_summary[,var_of_summary_for_each])
        ) %>% rbind(rep(NA, group_levels + 3))
      }
    }
    results_of_summary[[i]] <- summary_for_each
  }
  
  Results <- dplyr::bind_rows(results_of_summary)
  names(Results) <- c("指标", NA, group_label, "合计")
  
  return(Results) 
}
