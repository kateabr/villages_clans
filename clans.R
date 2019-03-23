# - - - библиотеки - - -

library(dplyr)

# - - - рабочий каталог - - -

setwd("D:\\Desktop\\villages_clans\\output")

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

# подсчет разности средних значений/медианы/среднеквадратического отклонения дистанций двух таблиц
# dt1, dt2: таблицы
# func: тип метрики (среднее значение/медиана/среднеквадратическое отклонение)
# same_clan: TRUE (для пар вида одна деревня:один клан) / FALSE (одна деревня:разные кланы)
# return: таблица 1x4 с разностью средних значений
dist_diff <- function(dt, func){
  res <- make_dist_df(rows = 1)
  for (dist_t in c("Flat", "Weighted", "Loiselle", "Ritland")){
    res[, dist_t] <- obs_diff(this_village.shuffled, dist_t, func)
  }
  
  return(res)
}

# перемешивание столбца таблицы
# data: исходная таблица
# repeats: могут ли люди из разных кланов оказаться в одном клане в результате перемешивания (TRUE / FALSE)
# return: таблица с перемешанными ярлыками кланов
shuff <- function(data, repeats = FALSE){
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
    mutate(ClanShuffled = as.vector(clans_inds$Clan)[sample(1:nrow(clans_inds), nrow(clans_inds), replace = repeats)]) -> clans_inds
  
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

# функция метрики по названию
# > вспомогательная функция
# mtr: тип метрики ("mean", "median", "sd")
# return: функция mean, median, sd или ошибка
metrics <- function(m){
  if (m == "mean")
    return(function(a) return(mean(a)))
  else if (m == "median")
    return(function(a) return(median(a)))
  else if (m == "sd")
    return(function(a) return(sd(a)))
  else stop(sprintf("Неизвестная операция: %s", m))
}

# наблюдаемая разность
# > вспомогательная функция
# data: таблица со значениями (не перемешанная)
# dist_type: тип расстояния ("Flat", "Weighted", "Loiselle", "Ritland")
# mtr: тип метрики ("mean", "median", "sd")
# return: разность между значениями метрики для строк с совпадающими и не совпадающими кланами
obs_diff <- function(data, dist_type, mtr){
  return(metrics(mtr)(data[data$ClanA != data$ClanB, ][, dist_type]) -
           ifelse(is.na((x = metrics(mtr)(data[data$ClanA == data$ClanB, ][, dist_type]))[1]), 0, x))
}

# подсчет средних, генерация и сохранение гистограммы
# vill: таблица до перемешивания
# shuffled: значения метрик расстояний после перемешивания таблицы
# village_name: название текущей деревни
# dist_type: тип расстояния ("Flat", "Weighted", "Loiselle", "Ritland")
# mtr: тип метрики ("mean", "median", "sd")
# cnt: номер текущего изображения
# wd: рабочий каталог, в который будут сохраняться изображения
# return: -
generate_hist <- function(vill, shuffled, village_name, dist_type, mtr, cnt){
  # наблюдаемое среднее
  observed_diff <- obs_diff(vill, dist_type, mtr)
  
  # генерация и сохранение гистограммы
  png(sprintf("%s_%s_%s_%i.png", village_name, dist_type, mtr, cnt))
  
  hist(shuffled[, dist_type], main = sprintf("Вид дистанции: %s; метрика: %s", dist_type, mtr))
  
  # генерация гистограммы
  tryCatch(
    {
      # если определен доверительный интервал
      abline(v = t.test(shuffled[, dist_type])$conf.int[1], col="blue", lwd=2)
      abline(v = t.test(shuffled[, dist_type])$conf.int[2], col="blue", lwd=2)
    },
    # если доверительный интервал не определен
    error = function(cond) {
      print("Доверительный интервал не определен")
    },
    # добавление на гистограмму обозреваемого среднего и среднего по перемешанному множеству
    finally = {
      abline(v = mean(shuffled[, dist_type]), col="red", lwd=2)
      abline(v = observed_diff, col="green", lwd=2)
    })
  
  dev.off()
}

# - - - основной код - - -

data <- read.csv("D:\\Desktop\\Human Y-STR - Dist-for_Yulik - Human Y-STR - Dist(1).csv")

data$ClanB <- factor(data$ClanB, levels=levels(data$ClanA))
data$VillageA <- factor(data$VillageA, levels=levels(data$VillageB))

data$VillageA %>%
  unique() -> villages # список всех деревень
villages <- villages[villages != ""]

for(cnt in seq(1, 1)) {
  for (village in villages){
    data[data$VillageA == village, ]$ClanA %>%
      unique() %>% as.vector() -> clans # все кланы в деревне village
  
    this_village <- data[data$VillageA == village & data$VillageB == village, ] # все люди из деревни village
    
    res.mean = make_dist_df()
    res.median = make_dist_df()
    res.sd = make_dist_df()
    
    
    for (i in rep(0, 10)) {
      this_village.shuffled <- shuff(this_village, repeats = FALSE)
      
      # mean
      res.mean <- bind_rows(res.mean, head(dist_diff(this_village.shuffled,
                                                          func = "mean"), 4))
  
      # median
      res.median <- bind_rows(res.median, head(dist_diff(this_village.shuffled,
                                                          func = "median"), 4))
      
      # sd
      res.sd <- bind_rows(res.sd, head(dist_diff(this_village.shuffled,
                                                          func = "sd"), 4))
      
    }
    
    for(dist_t in c("Flat", "Weighted", "Loiselle", "Ritland")){
      generate_hist(this_village, res.mean, village, dist_t, "mean", cnt)
      generate_hist(this_village, res.median, village, dist_t, "median", cnt)
      generate_hist(this_village, res.sd, village, dist_t, "sd", cnt)
    }
  }
}
