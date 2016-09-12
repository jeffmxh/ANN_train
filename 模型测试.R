# data = read.table("/home/jeffmxh/train_data.txt",header = TRUE,sep = "\t")
library(neuralnet)
train_data_1 = rbind(train_data[1:1000,],train_data[1:1000,],train_data[1:1000,],train_data[1:1000,])
train_data_test = train_data[801:1147,]
system.time({
for(i in 1:5){
stock_model <- neuralnet(judge ~ p_1 + p_2 + p_3 + p_4 + p_5 +p_6 + p_7 + p_8 + p_9 + p_10 +
                           p_11 + p_12 + p_13 + p_14 + p_15 + p_16 + p_17 + p_18 + p_19 + p_20 +
                           p_21 + p_22 + p_23 + p_24 + p_25 + p_26 + p_27 + p_28 + p_29 + p_30 +
                           p_31 + p_32 + p_33 + p_34 + p_35 + p_36 + p_37 + p_38 + p_39 + p_40 +
                           v_1 + v_2 + v_3 + v_4 + v_5 +v_6 + v_7 + v_8 + v_9 + v_10,
                         data = train_data_1,
                         hidden = i
)
stock_results <- compute(stock_model, train_data_test[2:(ncol(train_data_test)-1)])
stock_predict <- stock_results$net.result
a <- cor(stock_predict,train_data_test$judge)
cat("第", i, "个模型拟合完毕", "\n", sep="")
cat("隐藏层节点数：" ,i ,"\n", sep = "")
cat("召回率：", a, "\n", sep="")
cat("----------------------", "\n", sep="")
rm(stock_model,stock_predict,stock_results)
for(i in 1:5){
  cat("倒计时：", 6-i, "\n", sep="")
  Sys.sleep(1)
}
}
})