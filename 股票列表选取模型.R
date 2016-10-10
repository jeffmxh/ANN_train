if(!exists("stock_table")){
  load("D://神经网络训练数据//models//股票代码列表.RData")
  stock_table = data.frame(stock, "模型代码" = 0, "准确率" = 0)
  rm(stock)
}
for(i in 501:510){
  if(stock_table[i,3]!=0) next
  tryCatch(
    {
      stock_table[i,3:4] = choose_model(stock_symbol = stock_table[i,1], 
                                        model_filter_index = 0.3,
                                        show_progress = FALSE
                                       )
      cat("第", i, "/", nrow(stock_table), "个股票计算完成\n", sep = "")
    },
    error = function(e){
      stock_table[i,3:4] = c(0,0)
      cat("第", i, "/", nrow(stock_table), "个股票计算失败\n", sep = "")
    },
    warnings = function(w){
      stock_table[i,3:4] = c(0,0)
      cat("第", i, "/", nrow(stock_table), "个股票计算失败\n", sep = "")
    }
  )
}
save(stock_table, file = "d://神经网络训练数据//models//stock_table.RData")
rm(i)
