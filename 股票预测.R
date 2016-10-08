# 加载所需的扩展包---------------------------------------------

stopifnot("package:neuralnet"%in%search()||require("neuralnet", quietly = TRUE))
stopifnot("package:quantmod"%in%search()||require("quantmod", quietly = TRUE))
stopifnot("package:dplyr"%in%search()||require("dplyr", quietly = TRUE))

# 设置模型路径-------------------------------------------------

path = "D://神经网络训练数据//model_new//"

# 设置待预测的股票代码-----------------------------------------

stock_symbol = "300001" 
stock_symbol = symbol_trans(stock_symbol)

# 预测时间节点-------------------------------------------------

date_predict = Sys.Date()

# 提取模型列表-------------------------------------------------

load(paste0(path, "model_table.RData"))
model_index_1 = model_table[model_table$召回率>0.5,1]

# 生成结果数据框-----------------------------------------------

result = data.frame("stock_symbol" = stock_symbol, 
                    "model_index" = model_index_1, 
                    "reality" = 0, 
                    "forecast" = 0,
                    "accuracy" = 0
                    )

# 获取股票数据-------------------------------------------------

env_temp = new.env()
getSymbols(stock_symbol, from = date_predict-30, to = date_predict, env = env_temp)
data_temp = as.data.frame(get(toupper(stock_symbol), envir = env_temp)) 
rm(env_temp)
colnames(data_temp)=c("open", "close", "low", "high", "volume", "adjusted")
data_temp = data_temp %>% dplyr::filter(volume!=0)

# 除息除权-----------------------------------------------------

for(i in 1:4){
  data_temp[,i] = data_temp[,i]*data_temp[,6]/data_temp[,4]
}

days = nrow(data_temp)
profit = (data_temp$adjusted[days]-data_temp$adjusted[days-1])/data_temp$adjusted[days-1]
result$reality = profit
rm(days, profit)

data_temp=data_temp %>% 
  round(3) %>%
  dplyr::select(-adjusted) %>% 
  tail(11) %>%
  head(10)
data_temp_test = rep(0,50);dim(data_temp_test)=c(1,50);data_temp_test = as.data.frame(data_temp_test)
data_temp_test[1,1:50]=c(normalize(as.vector(t(as.matrix(data_temp[1:10,1:4])))),
                         normalize(as.vector(t(as.matrix(data_temp[1:10,5])))))
rm(data_temp)
for(i in 1:nrow(result)){
  file_path = paste0(path, "model", result$model_index[i], ".RData")
  load(file_path)
  stock_results <- neuralnet::compute(stock_model, data_temp_test[1,])
  result$forecast[i] <- stock_results$net.result
  if(result$reality[i]*result$forecast[i]>0){
    result$accuracy[i] = 1
  }else{
    result$accuracy[i] = 0
  }
  rm(stock_model, stock_results)
}
rm(data_temp_test, path, file_path, i, model_index_1, stock_symbol)
