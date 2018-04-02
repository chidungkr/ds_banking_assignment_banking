

#=======================================
#       Assignment Solutions
#=======================================


#------------------------------------------------------------
#  Assignment 1: Heron's Formula for the area of a triangle
#  Source: https://www.mathopenref.com/heronsformula.html
#------------------------------------------------------------


my_triangle <- function(a, b, c) {
  con1 <- a + b > c 
  con2 <- b + c > a
  con3 <- c + a > b
  total_con <- con1*con2*con3
  p <- 0.5*(a + b + c)
  if (total_con == 1) {
    s <- sqrt(p*(p - a)*(p - b)*(p - c))
    print(paste0("Diện tích của tam giác là: ", s))
    
  }
  if (total_con == 0) {
    print("Ba số a, b, c không phải là ba cạnh của một tam giác")
  }
}

# Test hàm: 

my_triangle(3, 4, 5)
my_triangle(3, 4, 7)

#-----------------------------------------------------------------
#  Assignment 2: Simple linear regression
#  Source: https://en.wikipedia.org/wiki/Simple_linear_regression
#-----------------------------------------------------------------


my_ols <- function(x, y) {
  x_mean <- mean(x)
  y_mean <- mean(y)
  ts <- sum((x - x_mean)*(y - y_mean))
  ms <- sum((x - x_mean)^2)
  beta <- ts / ms
  alpha <- y_mean - beta*x_mean
  
  print(paste0("Hệ số chặn là: ", alpha))
  print(paste0("Hệ số góc là: ", beta))
}

# Test hàm: 
my_ols(trees$Girth, trees$Volume)

# So sánh với hàm lm(): 

library(magrittr)
trees %>% 
  lm(Volume ~ Girth, data = .) %>% 
  summary()

  
# Hàm vừa viết chưa đưa ra thông tin về R2. Cải tiến như sau: 

my_ols_improved <- function(x, y) {
  x_mean <- mean(x)
  y_mean <- mean(y)
  ts <- sum((x - x_mean)*(y - y_mean))
  ms <- sum((x - x_mean)^2)
  beta <- ts / ms
  alpha <- y_mean - beta*x_mean
  du_bao <- alpha + beta*x
  r2 <- cor(du_bao, y)*cor(du_bao, y)
  
  
  # In ra các kết quả: 
  print(paste0("Hệ số chặn là: ", round(alpha, 4)))
  print(paste0("Hệ số góc là: ", beta %>% round(4)))
  print(paste0("Hệ số R2 là: ", r2 %>% round(4)))
  
}

# Test hàm: 
my_ols_improved(trees$Girth, trees$Volume)


#----------------------
#    Assignment 3
#----------------------


library(readxl)
library(tidyverse)

mydf <- read_excel("D:/Teaching/check.xlsx")
mydf %>% head()

# Viết hàm tách ra năm và tháng: 
library(stringr)

# str_sub(mydf$Time, start = str_count(mydf$Time) - 3, end = str_count(mydf$Time))

nam_extract <- function(x) {
  nam <- str_sub(x, start = str_count(x) - 3, end = str_count(x))
  return(nam)
  
}

thang_extract <- function(x) {
  thang <- str_sub(x, start = 1, end = str_count(x) - 5)
  return(thang)
}

# Sử dụng hàm: 

mydf %<>% mutate(Year = nam_extract(Time), 
                 Month = thang_extract(Time))

# Khoảng thời gian nghiên cứu: 
mydf$Year %>% unique()

# Hàm lấy ra danh sách các mã CK có đủ 12 tháng quan sát 
# khi cho biết thông tin đầu vào là một năm cụ thể nào đó: 

full_year_month <- function(mydf, year) {
  nam_x <- mydf %>% filter(Year == year)
  code <- nam_x$Ticker %>% unique()
  full_month <- data.frame()
  for (i in 1:length(code)) {
    m <- nam_x %>% filter(Ticker == code[i])
    if (nrow(m) == 12) {
      full_month <- bind_rows(full_month, m)
    }
  }
  return(full_month)
}  

# Ví dụ với năm 2009: 

full_2009 <- full_year_month(mydf, year = 2009)
full_2009 %>% head()

# Số lượng các mã CK có đủ 12 tháng quan sát trong năm 2009: 
full_2009$Ticker %>% unique() %>% length()




