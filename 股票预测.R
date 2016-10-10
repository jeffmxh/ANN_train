choose_model <- function(stock_symbol, model_filter_index = 0.5, show_progress = TRUE){
# 设置待预测的股票代码---------------------------------------

# stock_symbol = "300090" 
stock_symbol = symbol_trans(stock_symbol)

# 设置调取模型准确率范围-------------------------------------

# model_filter_index = 0.5

# 加载所需的扩展包-------------------------------------------

stopifnot("package:neuralnet"%in%search()||require("neuralnet", quietly = TRUE))
stopifnot("package:quantmod"%in%search()||require("quantmod", quietly = TRUE))
stopifnot("package:dplyr"%in%search()||require("dplyr", quietly = TRUE))

# 加载函数库-------------------------------------------------

source('D:/R_github/ANN_train/函数库.R', encoding = 'UTF-8')

# 设置模型路径-----------------------------------------------

path = "D://神经网络训练数据//models//"

# 获取测试数据集---------------------------------------------
test_data = get_stock_data(stock_symbol = stock_symbol, from_time = "2014-1-1", to_time = Sys.Date(), show_progress = show_progress)

# 提取模型列表-------------------------------------------------

load(paste0(path, "model_table.RData"))
if(show_progress) cat("成功加载模型列表\n")
model_index_1 = model_table[model_table$召回率>model_filter_index,1]

# 生成结果数据框-----------------------------------------------

result = data.frame("stock_symbol" = stock_symbol, 
                    "model_index" = model_index_1, 
                    "accuracy" = 0
                    )

for(i in 1:nrow(result)){
  file_path = paste0(path, "model", result$model_index[i], ".RData")
  load(file_path)
  stock_results <- neuralnet::compute(stock_model, test_data[2:(ncol(test_data)-1)])
  result$accuracy[i] <- get_score(stock_results$net.result, test_data$judge)
  if(show_progress) cat("第", i, "/", nrow(result), "个模型计算完成\n", sep = "")
  rm(stock_model, stock_results)
}
choice = result %>% dplyr::arrange(desc(accuracy)) %>% dplyr::select(-stock_symbol)
return(choice[1,])
}