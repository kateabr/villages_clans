# - - - библиотеки - - -

library(tidyverse)
library(GGally)
library(dplyr)
library(matrixStats)

# - - - функции - - -

# сравнение на равенство/неравенство
# > вспомогательная функция
# val: TRUE / FALSE
# return: "==" / "!="
comp <- function(val){
  return(ifelse(val, (function(a, b) return(a == b)), (function(a, b) return(a != b))))
}

# генерация пустой таблицы для хранения значений дистанций 4-х видов
# > функция-конструктор
# cols: количество столбцов, по умолчанию -- 4
# rows: количество строк, по умолчанию -- 0
# return: пустой dataframe с 4-мя столбцами ("Flat", "Weighted", "Loiselle", "Ritland")
make_dist_df <- function(cols = 4, rows = 0)
{
  res = data.frame(matrix(ncol = cols, nrow = rows))
  colnames(res) <- c("Flat", "Weighted", "Loiselle", "Ritland")
  return(res)
}

# на случай, если возникает пустая таблица
# > вспомогательная функция
# targ: таблица, которую необходимо проверить на непустоту
# src: таблица, габариты которой получит targ, если targ окажется пустой
# (если src тоже пустая, targ получит габариты 1х4)
# return: targ или таблица, заполненная нулями, имеющая габариты src или 1х4
pad <- function(targ, src){
  if (nrow(targ) == 0){
    len <- ifelse((len = nrow(src)) > 0, len, 1)
    targ <- make_dist_df(rows = len)
    for (i in seq(1, 4, 1)){
      targ[, i] = rep(0, len)
      }
  }
  return(targ)
}

# извлечение необходимых строк
# > вспомогательная функция
# tb: таблица
# same_clan: TRUE (для пар вида одна деревня:один клан) / FALSE (одна деревня:разные кланы)
# return: строки таблицы tb, удовлетворяющие условию same_clan
get_sub_table <- function(tb, same_clan = TRUE){
  tb[comp(same_clan)(tb$ClanA, tb$ClanB), ] %>%
    select(Flat, Weighted, Loiselle, Ritland) %>% return()
}

# подсчет разности средних значений/медианы/среднеквадратического отклонения дистанций двух таблиц
# dt1, dt2: таблицы
# func: тип метрики (среднее значение/медиана/среднеквадратическое отклонение)
# same_clan: TRUE (для пар вида одна деревня:один клан) / FALSE (одна деревня:разные кланы)
# return: таблица 1x4 с разностью средних значений
dist_diff <- function(dt1, dt2, func, same_clan = TRUE){
  cols1 <- get_sub_table(dt1, same_clan)
  cols2 <- get_sub_table(dt2, same_clan)
  
  cols1 <- as.matrix(pad(cols1, cols2))
  cols2 <- as.matrix(pad(cols2, cols1))
  
  res <- as.data.frame(t(func(cols1) - func(cols2)))
  colnames(res) <- c("Flat", "Weighted", "Loiselle", "Ritland")
  
  return(res)
}

# перемешивание столбца таблицы
# data: исходная таблица
# domain: список всех значений, которые может принять элемент столбца
shuffle <- function(data, domain){
  data %>%
    mutate(ClanB = domain[sample(1:length(domain), nrow(data), replace = TRUE)]) %>%
    return()
}

# - - - основной код - - -

data <- read.csv("D:\\Desktop\\Human Y-STR - Dist-for_Yulik - Human Y-STR - Dist(1).csv")
data <- as.tibble(data)

data$ClanB <- factor(data$ClanB, levels=levels(data$ClanA))
data$VillageA <- factor(data$VillageA, levels=levels(data$VillageB))

data$VillageA %>%
  unique() -> villages # список всех деревень
villages <- villages[villages != ""]

for (village in villages){
  data[data$VillageA == village, ]$ClanA %>%
    unique() %>%
    as.vector() -> clans # все кланы в деревне village

  this_village <- data[data$VillageA == village & data$VillageB == village, ] # все люди из деревни village
  
  res.same.mean = make_dist_df()
  res.diff.mean = make_dist_df()
  res.same.median = make_dist_df()
  res.diff.median = make_dist_df()
  res.same.sd = make_dist_df()
  res.diff.sd = make_dist_df()
  
  for (i in rep(0, 1000)) {
    this_village.shuffled <- shuffle(this_village, clans) # перемешивание столбца ClansB
    
    # mean
    res.same.mean <- bind_rows(res.same.mean, head(dist_diff(this_village, this_village.shuffled,
                                                             func = colMeans, same_clan = TRUE), 4))
    res.diff.mean <- bind_rows(res.diff.mean, head(dist_diff(this_village, this_village.shuffled,
                                                             func = colMeans, same_clan = FALSE), 4))
    
    # median
    res.same.median <- bind_rows(res.same.median, head(dist_diff(this_village, this_village.shuffled,
                                                             func = colMedians, same_clan = TRUE), 4))
    res.diff.median <- bind_rows(res.diff.median, head(dist_diff(this_village, this_village.shuffled,
                                                             func = colMedians, same_clan = FALSE), 4))
    
    # sd
    res.same.sd <- bind_rows(res.same.sd, head(dist_diff(this_village, this_village.shuffled,
                                                                 func = colSds, same_clan = TRUE), 4))
    res.diff.sd <- bind_rows(res.diff.sd, head(dist_diff(this_village, this_village.shuffled,
                                                                 func = colSds, same_clan = FALSE), 4)) 
  }
  
  break()
}

hist(res.same.median$Loiselle - res.diff.median$Loiselle)

shuffled_diff <- ifelse(is.na((x = median(res.same.median$Loiselle))[1]), 0, x) - median(res.diff.median$Loiselle)

observed_diff <- ifelse(is.na((x = median(this_village[this_village$ClanA == this_village$ClanB, ]$Loiselle))[1]), 0, x) -
  median(this_village[this_village$ClanA != this_village$ClanB, ]$Loiselle)
abline(v=observed_diff, col="green", lwd=2)
abline(v=shuffled_diff, col="red", lwd=2) 
