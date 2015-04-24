clusterMeasures <- function(table){
  #   table <- ktable
  tp <- c()
  sum.table <- sum(table)
  row <- nrow(table)
  for(i in 1: row){
    #     i=1+1
    if(max(table) != 0){
      maxt <- max(table)
      index <- which(table == maxt)
      col.po <- index%/%row+1
      row.po <- index%%row
      tp <- c(maxt, tp)
      table[,col.po] <- 0
      table[row.po,] <- 0
      #     c(nrow(table), ncol(table))
    }
  }
  #   print(sum(tp))
  print(sum(tp)/sum.table)
}
