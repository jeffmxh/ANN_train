# 用于向量正规化-----------------------------------------

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# 把股票代码转化为标准格式-------------------------------

symbol_trans <- function(stock_symbol){
  if(grepl("^60", stock_symbol)){
    stock_symbol = paste0(stock_symbol, ".ss")
  }else{
    stock_symbol = paste0(stock_symbol, ".sz")
  }
  return(stock_symbol)
}

# 计算预测正确的百分比----------------------------------

get_score <- function(array1, array2){
  if(length(array1)!=length(array2)){
    ("向量长度不等")
  }else{
    x = array1*array2
    x[x>0] =1
    x[x<=0] = 0
    return(mean(x))
  }
}

# 获取股票数据------------------------------------------

get_stock_data <- function(stock_symbol, from_time = "2014-1-1", to_time = Sys.Date(), show_progress = TRUE) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly = TRUE))
  stopifnot("package:dplyr" %in% search() || require("dplyr", quietly = TRUE))
  getSymbols(stock_symbol, from = from_time, to = to_time)
  if(show_progress) cat("数据下载完成\n")
  data_temp = as.data.frame(get(toupper(stock_symbol)))
  colnames(data_temp) = c("open", "close", "low", "high", "volume", "adjusted")
  data_temp = data_temp %>% dplyr::filter(volume != 0)
  for (i in 1:4) {
    data_temp[, i] = data_temp[, i] * data_temp[, 6]/data_temp[, 4]
  }
  data_temp = data_temp %>% round(3) %>% dplyr::select(-adjusted)
  test_data = rep(0, 51)
  dim(test_data) = c(1, 51)
  test_data = as.data.frame(test_data)
  for (i in 1:40) colnames(test_data)[i] = paste0("p_", i)
  for (i in 41:50) colnames(test_data)[i] = paste0("v_", i - 40)
  colnames(test_data)[51] = "judge"
  for (i in 1:(nrow(data_temp) - 9)) {
    test_data[i, 1:50] = c(as.vector(t(as.matrix(data_temp[i:(i + 9), 1:4]))), 
                           as.vector(t(as.matrix(data_temp[i:(i + 9), 5]))))
  }
  test_data[, 1:40] = apply(test_data[, 1:40], 1, normalize)
  test_data[, 41:50] = apply(test_data[, 41:50], 1, normalize)
  for (i in 1:nrow(test_data)) {
    test_data[i, 51] = (data_temp[i + 10, 2] - data_temp[i + 9, 2])/data_temp[i + 9, 2]
  }
  test_data = test_data %>% dplyr::filter(!is.na(judge))
  test_data[, 2:51] = round(test_data[, 2:51], 3)
  test_data = data.frame(source = rep(stock_symbol, nrow(test_data)), 
                         test_data)
  if(show_progress) cat("计算过程完成\n")
  return(test_data)
}
