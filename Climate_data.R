### global ---------------------------------------------------------------------
library(readxl)
library(stringr)

### data input
file_list <- list.files("Input/Precipitation/1962", full.names = TRUE)

### dfs for each year
## 62
df_62 <- read.csv("Input/Precipitation/1962/prec_1962_1.csv", sep = ";")

for (i in 2:length(file_list)){
  df_62 <- cbind(df_62, read.csv(file_list[i], sep = ",")[4])
}; df_62 
df_62$sum_62 <- apply(as.matrix(df_62[,-c(1:3)]), 1, FUN = sum)

## 63
df_63 <- read.csv("Input/Precipitation/1963/prec_1963_1.csv", sep = ",")

for (i in 2:length(file_list)){
  df_63 <- cbind(df_63, read.csv(file_list[i], sep = ",")[4])
}; df_63
df_63$sum_63 <- apply(as.matrix(df_63[,-c(1:3)]), 1, FUN = sum)

## 64
df_64 <- read.csv("Input/Precipitation/1964/prec_1964_1.csv")

for (i in 2:length(file_list)){
  df_64 <- cbind(df_64, read.csv(file_list[i], sep = ",")[4])
}; df_64
df_64$sum_64 <- apply(as.matrix(df_64[,-c(1:3)]), 1, FUN = sum)

### Overall sum ----------------------------------------------------------------
df_prec <- cbind(df_62[,c(1:3, 16)], df_63[,16], df_64[,16])
df_prec$prec_GLF <- rowSums(df_prec[,c(4:6)])

### Adjust
colnames(df_prec)[c(1,5,6)] <- c("Glacier","sum_63", "sum_64")  
df_prec$Glacier <- str_to_title(df_prec$Glacier)

### Export ---------------------------------------------------------------------
write.csv(df_prec, "Output/precipitation.csv")
writexl::write_xlsx(df_prec, "Output/precipitation.xlsx")