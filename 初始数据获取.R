normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
library(quantmod)
library(dplyr)
system.time({
getSymbols("300153.sz", from = "2014-1-1", to = Sys.time())
data_300153 = as.data.frame(`300153.SZ`)
colnames(data_300153)=c("open","close","low","high","volume","adjusted")
data_300153 = data_300153 %>% filter(volume!=0) 
rm(`300153.SZ`)
for(i in 1:4){
  data_300153[,i] = data_300153[,i]*data_300153[,6]/data_300153[,4]
}
data_300153=data_300153 %>% 
  round(3)%>%
  select(-adjusted)
train_data = rep(0,51);dim(train_data)=c(1,51);train_data = as.data.frame(train_data)
for(i in 1:40)colnames(train_data)[i] = paste0("p_",i)
for(i in 41:50)colnames(train_data)[i] = paste0("v_",i-40)
colnames(train_data)[51] = "judge"
for(i in 1:(nrow(data_300153)-9)){
  train_data[i,1:50]=c(as.vector(t(as.matrix(data_300153[i:(i+9),1:4]))),as.vector(t(as.matrix(data_300153[i:(i+9),5]))))
}
train_data[,1:40]=apply(train_data[,1:40],1,normalize)
train_data[,41:50]=apply(train_data[,41:50],1,normalize)
for(i in 1:nrow(train_data)){
  train_data[i,51] = (data_300153[i+10,2]-data_300153[i+9,2])/data_300153[i+9,2]
}
train_data = train_data %>% filter(!is.na(judge))
train_data[,2:51] = round(train_data[,2:51],3)
train_data = data.frame("source" = rep("300153",nrow(train_data)),train_data) 
rm(data_300153)
})