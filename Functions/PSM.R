#' Title 倾向性得分匹配
#'
#' @param data 数据集
#' @param id ID变量
#' @param center 中心变量
#' @param outcome 
#' @param by 用来分组的变量
#' @param group 用来描述的分组
#' @param group_label 给分组一个标签
#' @param covariates 匹配协变量
#' @param method 匹配方法
#' @param distance 匹配距离
#' @param replace 是否放回
#' @param caliper 卡钳值
#'
#' @return
#' @export
#'
#' @examples
PSM <- function(
    data,
    id,
    center,
    outcome,
    by,
    group,
    group_label,
    covariates,
    method,
    distance,
    replace,
    caliper) {
  
  data_match <- as.data.frame(data[as_vector(data[, by]) %in% group, ])
  
  # 将处理因子化
  data_match[, by] <- factor(data_match[, by], levels = group[2:1], labels = c(0,1))
  
  formula <- sprintf("%s ~ %s", by, 
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
  data_matched[, by] <- factor(data_matched[, by], levels = c(0,1), labels = group[2:1])
  
  return(data_matched)
}