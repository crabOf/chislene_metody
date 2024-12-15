# Загрузка необходимых библиотек
library(ggplot2)      # Для визуализации данных
library(dplyr)       # Для манипуляции данными
library(factoextra)  # Для визуализации кластеризации

# Функция линейной регрессии методом наименьших квадратов
linear_regression_manual <- function(x, y) {
  # Вычисление средних значений
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  # Вычисление коэффициентов наклона и свободного члена
  numerator <- sum((x - x_mean) * (y - y_mean))
  denominator <- sum((x - x_mean)^2)
  
  slope <- numerator / denominator
  intercept <- y_mean - slope * x_mean
  
  # Прогнозирование значений y
  y_pred <- intercept + slope * x
  ss_total <- sum((y - mean(y))^2)  # Общая сумма квадратов
  ss_residual <- sum((y - y_pred)^2)  # Остаточная сумма квадратов
  r_squared <- 1 - (ss_residual / ss_total)  # Коэффициент детерминации
  
  return(list(slope = slope, intercept = intercept, r_squared = r_squared))
}

# Функция кластеризации точек и поиска оптимального количества кластеров
find_optimal_clusters <- function(x, y, max_clusters = 10) {
  data <- data.frame(x = x, y = y)  # Создание датафрейма из x и y
  wss <- numeric(max_clusters)  # Вектор для хранения суммы квадратов внутри кластеров
  
  # Цикл для вычисления суммы квадратов для разных количеств кластеров
  for (k in 1:max_clusters) {
    kmeans_result <- kmeans(data, centers = k)
    wss[k] <- kmeans_result$tot.withinss  # Сохранение суммы квадратов
  }
  
  # Визуализация метода локтя
  plot(1:max_clusters, wss, type = "b", pch = 19, xlab = "Количество кластеров", ylab = "Сумма квадратов внутри кластеров")
  
  # Определение оптимального количества кластеров
  optimal_k <- which(diff(wss) == min(diff(wss))) + 1
  return(optimal_k)
}

# Функция поиска линий для кластеров
find_lines_in_clusters <- function(x, y, clusters) {
  unique_clusters <- unique(clusters)  # Уникальные кластеры
  lines <- list()  # Список для хранения линий
  
  # Цикл по уникальным кластерам
  for (cluster_id in unique_clusters) {
    cluster_indices <- which(clusters == cluster_id)  # Индексы точек в текущем кластере
    cluster_x <- x[cluster_indices]  # x-координаты точек в кластере
    cluster_y <- y[cluster_indices]  # y-координаты точек в кластере
    
    # Линейная регрессия для текущего кластера
    line <- linear_regression_manual(cluster_x, cluster_y)
    lines[[as.character(cluster_id)]] <- line  # Сохранение линии
  }
  
  return(lines)
}

# Визуализация результатов
visualize_clustered_lines <- function(x, y, clusters, lines) {
  plot(x, y, col = clusters, 
       main = 'Кластеризация и аппроксимация линий',
       xlab = 'Лошадиные силы (hp)', ylab = 'Мили на галлон (mpg)', pch = 16)
  
  line_colors <- rainbow(length(lines))  # Генерация цветов для линий
  
  # Цикл по линиям
  for (i in seq_along(lines)) {
    model <- lines[[i]]  # Модель для текущей линии
    
    # Диапазон x для линии
    x_line <- seq(min(x), max(x), length.out = 100)
    y_line <- model$intercept + model$slope * x_line  # Вычисление y для линии
    
    lines(x_line, y_line, 
          col = line_colors[i], 
          lwd = 2)  # Отрисовка линии
    
    # Вывод параметров линии
    legend_text <- sprintf(
      "Кластер %d: y = %.2f + %.2f*x (R² = %.2f)", 
      i, model$intercept, model$slope, model$r_squared
    )
    legend("topright", legend = legend_text, 
           text.col = line_colors[i], 
           bty = "n", cex = 0.7, 
           inset = c(0, 0.1 * (i-1)))
  }
}

# Пример использования с набором данных mtcars
data(mtcars)

# Используем переменные hp и mpg
x <- mtcars$hp
y <- mtcars$mpg

# Устанавливаем количество кластеров на 4
optimal_k <- 4

# Кластеризация точек с использованием заданного количества кластеров
kmeans_result <- kmeans(data.frame(x, y), centers = optimal_k)

# Поиск линий для каждого кластера
lines <- find_lines_in_clusters(x, y, kmeans_result$cluster)

# Визуализация результатов
visualize_clustered_lines(x, y, kmeans_result$cluster, lines)