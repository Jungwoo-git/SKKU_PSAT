## [ data loading ] 
setwd("C:/Users/10sop/Desktop/통계분석학회PSAT/주제분석/데이터/주제분석_데이터")

final_set <- "final_data_set.csv" %>% fread(encoding = "UTF-8")
final_set[,"Date"] <- final_set[, "Date"] %>% as.matrix %>% as.character

# Atinum
raw_atinum <- "Atinum.csv" %>% fread(encoding = "UTF-8")
raw_atinum[, "Date"] <- raw_atinum[, "Date"] %>% as.matrix %>% ymd
raw_atinum[, "Date"] <- raw_atinum[, "Date"] %>% as.matrix %>% as.character


atinum <- raw_atinum[, c("Date", "Price")]
atinum[,"Price"] <- gsub(",", "", atinum[, "Price"]%>% as.matrix)
atinum[, "Price"] <- atinum[, "Price"] %>% as.matrix %>% as.numeric
names(atinum) <- c("Date", "atinum")
atinum %>% str


# Woori
raw_woori <- "Woori.csv" %>% fread(encoding = "UTF-8")
raw_woori[, "Date"] <- raw_woori[, "Date"] %>% as.matrix %>% ymd
raw_woori[,"Date"] <- raw_woori[, "Date"] %>% as.matrix %>% as.Date


raw_woori[, "Date"] <- raw_woori[, "Date"] %>% as.matrix %>% as.character

woori <- raw_woori[, c("Date", "Price")]
woori[, "Price"] <- gsub(",", "", woori[, "Price"] %>% as.matrix)


woori[, "Price"] <- woori[, "Price"] %>% as.matrix %>% as.numeric
names(woori) <- c("Date", "woori")

BTC_price %>% str
gold_price %>% str


left_join(final_set, atinum, by = "Date") %>% 
  left_join(woori, by = "Date")






