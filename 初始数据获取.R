normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
symbol_trans <- function(stock_symbol){
  if(grepl("^60", stock_symbol)){
    stock_symbol = paste0(stock_symbol, ".ss")
  }else{
    stock_symbol = paste0(stock_symbol, ".sz")
  }
  return(stock_symbol)
}
stopifnot("package:quantmod"%in%search()||require("quantmod",quietly=TRUE))
stopifnot("package:dplyr"%in%search()||require("dplyr",quietly=TRUE))
stock_symbol = "300153"
stock_symbol = symbol_trans(stock_symbol)
getSymbols(stock_symbol, from = "2014-1-1", to = Sys.time())
data_temp = as.data.frame(get(toupper(stock_symbol)))
colnames(data_temp)=c("open","close","low","high","volume","adjusted")
data_temp = data_temp %>% filter(volume!=0) 
for(i in 1:4){
  data_temp[,i] = data_temp[,i]*data_temp[,6]/data_temp[,4]
}
data_temp=data_temp %>% 
  round(3)%>%
  select(-adjusted)
train_data = rep(0,51);dim(train_data)=c(1,51);train_data = as.data.frame(train_data)
for(i in 1:40)colnames(train_data)[i] = paste0("p_",i)
for(i in 41:50)colnames(train_data)[i] = paste0("v_",i-40)
colnames(train_data)[51] = "judge"
for(i in 1:(nrow(data_temp)-9)){
  train_data[i,1:50]=c(as.vector(t(as.matrix(data_temp[i:(i+9),1:4]))),as.vector(t(as.matrix(data_temp[i:(i+9),5]))))
}
train_data[,1:40]=apply(train_data[,1:40],1,normalize)
train_data[,41:50]=apply(train_data[,41:50],1,normalize)
for(i in 1:nrow(train_data)){
  train_data[i,51] = (data_temp[i+10,2]-data_temp[i+9,2])/data_temp[i+9,2]
}
train_data = train_data %>% filter(!is.na(judge))
train_data[,2:51] = round(train_data[,2:51],3)
train_data = data.frame("source" = rep(stock_symbol, nrow(train_data)), train_data) 
rm(data_temp, stock_symbol, i)

