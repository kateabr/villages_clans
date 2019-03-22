# - - - библиотеки - - -

#library(tidyverse)
library(GGally)
library(dplyr)
library(matrixStats)
library(permute)

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
# return: таблица с перемешанными ярлыками кланов
shuff <- function(data){
  data.shuffled.A <- data[FALSE, ]
  
  # получение из таблицы списка людей вместе с кланом, к которому они принадлежат
  data %>%
    select(ClanA, IndA) %>%
    distinct() -> clans_inds.A
  colnames(clans_inds.A) <- c("Clan", "Ind")
  
  data %>%
    select(ClanB, IndB) %>%
    distinct() -> clans_inds.B
  colnames(clans_inds.B) <- c("Clan", "Ind")
  
  clans_inds.A %>%
    rbind(., clans_inds.B) %>%
    distinct() -> clans_inds
  
  # случайная замена ярлыков кланов
  clans_inds %>%
    mutate(ClanShuffled = as.vector(clans_inds$Clan)[sample(1:nrow(clans_inds), nrow(clans_inds), replace = TRUE)]) -> clans_inds
  
  # обновление ярлыков для столбца ClanA
  for (individual in clans_inds$Ind) {
    data[data$IndA == individual, ] %>%
      mutate(ClanA = clans_inds[clans_inds$Ind == individual, ]$ClanShuffled) %>%
      rbind(data.shuffled.A, .) -> data.shuffled.A
  }
  
  data.shuffled.B <- data[FALSE, ]
  
  # обновление ярлыков для столбца ClanB
  for (individual in clans_inds$Ind) {
    data.shuffled.A[data.shuffled.A$IndB == individual, ] %>%
      mutate(ClanB = clans_inds[clans_inds$Ind == individual, ]$ClanShuffled) %>%
      rbind(data.shuffled.B, .) -> data.shuffled.B
  }
  
  return(data.shuffled.B)
}

# - - - основной код - - -
setwd("D:\\Desktop\\villages_clans")
data <- read.csv("D:\\Desktop\\Human Y-STR - Dist-for_Yulik - Human Y-STR - Dist(1).csv")
data <- as.tibble(data)

data$ClanB <- factor(data$ClanB, levels=levels(data$ClanA))
data$VillageA <- factor(data$VillageA, levels=levels(data$VillageB))

data$VillageA %>%
  unique() -> villages # список всех деревень
villages <- villages[villages != ""]


for(cnt in seq(1, 10)) {
  for (village in villages){
    data[data$VillageA == village, ]$ClanA %>%
      unique() %>% as.vector() -> clans # все кланы в деревне village
  
    this_village <- data[data$VillageA == village & data$VillageB == village, ] # все люди из деревни village
    
    res.same.mean = make_dist_df()
    res.diff.mean = make_dist_df()
    res.same.median = make_dist_df()
    res.diff.median = make_dist_df()
    res.same.sd = make_dist_df()
    res.diff.sd = make_dist_df()
    
    shuffled_diff_mean <- c()
    
    for (i in rep(0, 100)) {
      this_village.shuffled <- shuff(this_village)
      
      # mean
      res.same.mean <- bind_rows(res.same.mean, head(dist_diff(this_village, this_village.shuffled,
                                                               func = colMeans, same_clan = TRUE), 4))
      res.diff.mean <- bind_rows(res.diff.mean, head(dist_diff(this_village, this_village.shuffled,
                                                                func = colMeans, same_clan = FALSE), 4))
  
      # median
      # res.same.median <- bind_rows(res.same.median, head(dist_diff(this_village, this_village.shuffled,
      #                                                          func = colMedians, same_clan = TRUE), 4))
      # res.diff.median <- bind_rows(res.diff.median, head(dist_diff(this_village, this_village.shuffled,
      #                                                          func = colMedians, same_clan = FALSE), 4))
      # 
      # # sd
      # res.same.sd <- bind_rows(res.same.sd, head(dist_diff(this_village, this_village.shuffled,
      #                                                              func = colSds, same_clan = TRUE), 4))
      # res.diff.sd <- bind_rows(res.diff.sd, head(dist_diff(this_village, this_village.shuffled,
      #                                                              func = colSds, same_clan = FALSE), 4))
      
      shuffled_diff_mean <- c(shuffled_diff_mean, mean(res.diff.mean$Flat) - ifelse(is.na((x = mean(res.same.mean$Flat))[1]), 0, x))
    }
    break()
  }
  
  observed_diff <- mean(this_village[this_village$ClanA != this_village$ClanB, ]$Flat) -
    ifelse(is.na((x = mean(this_village[this_village$ClanA == this_village$ClanB, ]$Flat))[1]), 0, x)
  xlims <- t.test(shuffled_diff_mean)$conf.int[1:2]
  if(observed_diff < xlims[1]) {
    xlims[1] <- observed_diff - 1
  }
  if(observed_diff > xlims[2]) {
    xlims[2] <- observed_diff + 1
  }
  
  png(sprintf("%s_flat_mean_%i.png", village, cnt))
  
  hist(shuffled_diff_mean, main = "Flat: Mean", xlim = xlims)
  
  abline(v=observed_diff, col="green", lwd=2)
  
  abline(v=mean(shuffled_diff_mean), col="red", lwd=2)
  
  abline(v=t.test(shuffled_diff_mean)$conf.int[1], col="blue", lwd=2)
  abline(v=t.test(shuffled_diff_mean)$conf.int[2], col="blue", lwd=2)
  
  dev.off()
}
