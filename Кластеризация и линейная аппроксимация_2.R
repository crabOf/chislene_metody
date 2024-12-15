# Загрузка необходимых библиотек
library(ggplot2)
library(dplyr)
library(factoextra)

# Функция генерации зашумленных точек комбинации двух линий
generate_noisy_lines <- function(n_points = 200, noise_level = 0.5) {
  x1 <- runif(n_points, 0, 5)
  y1 <- 2 + 0.5 * x1 + rnorm(n_points, 0, noise_level)
  
  x2 <- runif(n_points, 5, 10)
  y2 <- -1 - 0.3 * x2 + rnorm(n_points, 0, noise_level)
  
  x <- c(x1, x2)
  y <- c(y1, y2)
  
  return(list(x = x, y = y))
}

# Функция линейной регрессии методом наименьших квадратов
linear_regression_manual <- function(x, y) {
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  numerator <- sum((x - x_mean) * (y - y_mean))
  denominator <- sum((x - x_mean)^2)
  
  slope <- numerator / denominator
  intercept <- y_mean - slope * x_mean
  
  y_pred <- intercept + slope * x
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_pred)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  return(list(slope = slope, intercept = intercept, r_squared = r_squared))
}

# Функция кластеризации точек и поиска оптимального количества кластеров
find_optimal_clusters <- function(x, y, max_clusters = 10) {
  data <- data.frame(x = x, y = y)
  wss <- numeric(max_clusters)
  
  for (k in 1:max_clusters) {
    kmeans_result <- kmeans(data, centers = k)
    wss[k] <- kmeans_result$tot.withinss
  }
  
  # Визуализация метода локтя
  plot(1:max_clusters, wss, type = "b", pch = 19, xlab = "Количество кластеров", ylab = "Сумма квадратов внутри кластеров")
  
  # Определение оптимального количества кластеров
  optimal_k <- which(diff(wss) == min(diff(wss))) + 1
  return(optimal_k)
}

# Функция поиска линий для кластеров
find_lines_in_clusters <- function(x, y, clusters) {
  unique_clusters <- unique(clusters)
  lines <- list()
  
  for (cluster_id in unique_clusters) {
    cluster_indices <- which(clusters == cluster_id)
    cluster_x <- x[cluster_indices]
    cluster_y <- y[cluster_indices]
    
    # Линейная регрессия для текущего кластера
    line <- linear_regression_manual(cluster_x, cluster_y)
    lines[[as.character(cluster_id)]] <- line
  }
  
  return(lines)
}

# Визуализация результатов
visualize_clustered_lines <- function(x, y, clusters, lines) {
  plot(x, y, col = clusters, 
       main = 'Кластеризация и аппроксимация линий',
       xlab = 'X', ylab = 'Y', pch = 16)
  
  line_colors <- rainbow(length(lines))
  
  for (i in seq_along(lines)) {
    model <- lines[[i]]
    
    x_line <- seq(min(x), max(x), length.out = 100)
    y_line <- model$intercept + model$slope * x_line
    
    lines(x_line, y_line, 
          col = line_colors[i], 
          lwd = 2)
    
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

# Пример использования
set.seed(123)

# Генерация зашумленных точек
noisy_data <- generate_noisy_lines(n_points = 200, noise_level = 0.5)

# Поиск оптимального количества кластеров
optimal_k <- find_optimal_clusters(noisy_data$x, noisy_data$y, max_clusters = 10)

# Кластеризация точек с использованием найденного количества кластеров
kmeans_result <- kmeans(data.frame(noisy_data$x, noisy_data$y), centers = optimal_k)

# Поиск линий для каждого кластера
lines <- find_lines_in_clusters(noisy_data$x, noisy_data$y, kmeans_result$cluster)

# Визуализация результатов
visualize_clustered_lines(noisy_data$x, noisy_data$y, kmeans_result$cluster, lines)
