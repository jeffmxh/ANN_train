# 加载所需的程序包----------------------------------------

# library(neuralnet)
# library(ggplot2)

# 初始化模型记录列表--------------------------------------

# model_index <- 1
# model_collector <- list()
# model_table = data.frame("模型序号" = c(1), "隐藏层节点数" = c(1), "召回率" = c(0))

# 载入之前获取的数据--------------------------------------

# data = read.table("/home/jeffmxh/train_data.txt", header = TRUE, sep = "\t")
# 对数据进行抽样，或许训练集和测试集
# train_data_1 = sample_n(train_data, 5000, replace = FALSE)
# train_data_test = sample_n(train_data, 1000, replace = FALSE)

# 主程序，测试模型------------------------------------

# 求解神经网络模型---------------------

for(i in 31:33){
time_temp = Sys.time()
stock_model <- neuralnet(judge ~ p_1 + p_2 + p_3 + p_4 + p_5 +p_6 + p_7 + p_8 + p_9 + p_10 +
                           p_11 + p_12 + p_13 + p_14 + p_15 + p_16 + p_17 + p_18 + p_19 + p_20 +
                           p_21 + p_22 + p_23 + p_24 + p_25 + p_26 + p_27 + p_28 + p_29 + p_30 +
                           p_31 + p_32 + p_33 + p_34 + p_35 + p_36 + p_37 + p_38 + p_39 + p_40 +
                           v_1 + v_2 + v_3 + v_4 + v_5 +v_6 + v_7 + v_8 + v_9 + v_10,
                         data = train_data_1,
                         hidden = i
)
stock_results <- neuralnet::compute(stock_model, train_data_test[2:(ncol(train_data_test)-1)])
stock_predict <- stock_results$net.result

# 计算召回率---------------------------

a <- cor(stock_predict, train_data_test$judge)

# 在屏幕打印部分结果-------------------

cat("第", i, "个模型拟合完毕", "\n", sep= "")
cat("召回率：", a, "\n", sep = "")
cat("用时：", Sys.time()-time_temp, "\n", sep = "")

# 打印记录至日志文件：d:\result.txt ---

cat("第", i, "个模型拟合完毕", "\n",file = "d://result.txt", append = TRUE, sep = "")
cat("隐藏层节点数：" ,i ,"\n",file = "d://result.txt", append = TRUE, sep = "")
cat("召回率：", a, "\n",file = "d://result.txt", append = TRUE, sep = "")
cat("模型参数", stock_model$result.matrix, "\n", "\n", file = "d://result.txt", append = TRUE, sep = "")
# model_collector[[model_index]] = stock_model

# 存储模型至硬盘-----------------------

file_name = paste0("D://神经网络训练数据//models//model", i, ".RData")
save(stock_model, file = file_name)
model_table[model_index,] = c(model_index, i, a)
cat("该模型已被记录，序号为", model_index, "\n", sep = "")
model_index = model_index + 1
cat("----------------------", "\n", sep="")
rm(stock_model, stock_predict, stock_results, a, file_name, time_temp)
}

# 绘图观察召回率变化曲线-----------------------------------

p <- ggplot(model_table, aes(x = 隐藏层节点数, y = 召回率))+ geom_line() + ggtitle("神经网络训练结果")
ggsave(file = "d://result.png", plot = p, width = 30, height = 20, units = "cm")
rm(p)
