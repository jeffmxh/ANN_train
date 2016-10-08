stopifnot("package:quantmod"%in%search()||require("quantmod",quietly=TRUE))
stopifnot("package:dplyr"%in%search()||require("dplyr",quietly=TRUE))
stock_symbol = "300002"
stock_symbol = symbol_trans(stock_symbol)
if(stock_symbol %in% train_data$source){
  cat("股票", stock_symbol, "数据已存在于训练集\n", sep = "")
}else{
  env_temp = new.env()
getSymbols(stock_symbol, from = "2014-1-1", to = Sys.time(), env = env_temp)
data_stock = as.data.frame(get(toupper(stock_symbol), envir = env_temp))
rm(env_temp)
colnames(data_stock)=c("open","close","low","high","volume","adjusted")
data_stock = data_stock %>% filter(volume!=0) 
for(i in 1:4){
  data_stock[,i] = data_stock[,i]*data_stock[,6]/data_stock[,4]
}
data_stock=data_stock %>% 
  round(3)%>%
  select(-adjusted)
train_data_temp = rep(0,51);dim(train_data_temp)=c(1,51);train_data_temp = as.data.frame(train_data_temp)
for(i in 1:40)colnames(train_data_temp)[i] = paste0("p_",i)
for(i in 41:50)colnames(train_data_temp)[i] = paste0("v_",i-40)
colnames(train_data_temp)[51] = "judge"
for(i in 1:(nrow(data_stock)-9)){
  train_data_temp[i,1:50]=c(as.vector(t(as.matrix(data_stock[i:(i+9),1:4]))),as.vector(t(as.matrix(data_stock[i:(i+9),5]))))
}
train_data_temp[,1:40]=apply(train_data_temp[,1:40],1,normalize)
train_data_temp[,41:50]=apply(train_data_temp[,41:50],1,normalize)
for(i in 1:nrow(train_data_temp)){
  train_data_temp[i,51] = (data_stock[i+10,2]-data_stock[i+9,2])/data_stock[i+9,2]
}
train_data_temp = train_data_temp %>% filter(!is.na(judge))
train_data_temp[,2:51] = round(train_data_temp[,2:51],3)
train_data_temp = data.frame("source" = rep(stock_symbol,nrow(train_data_temp)),train_data_temp)
train_data = rbind(train_data,train_data_temp)
cat("成功补充数据集", stock_symbol, "\n", sep = "")
rm(data_stock, train_data_temp)
}
rm(stock_symbol)
