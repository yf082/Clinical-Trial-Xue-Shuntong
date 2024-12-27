#' 查找名称
#'
#' @param patten 数据集中变量包含的字段
#' @param data 数据集 
#'
#' @return
#' @export

Search_name <- function(patten, data) {
  names(data)[grep(patten, names(data))]
}
