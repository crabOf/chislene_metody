# Функция генерации зашумленных точек комбинации двух линий
generate_noisy_lines <- function(n_points = 200, noise_level = 0.5) {
  # Создание точек для первой линии
  x1 <- runif(n_points, 0, 5)
  y1 <- 2 + 0.5 * x1 + rnorm(n_points, 0, noise_level)
  
  # Создание точек для второй линии
  x2 <- runif(n_points, 5, 10)
  y2 <- -1 - 0.3 * x2 + rnorm(n_points, 0, noise_level)
  
  # Объединение точек
  x <- c(x1, x2)
  y <- c(y1, y2)
  
  return(list(x = x, y = y))
}

# Функция расчета расстояния точки до линии
point_line_distance <- function(x, y, line) {
  # line: список с коэффициентами a, b линии вида y = ax + b
  a <- line$slope
  b <- line$intercept
  
  # Расчет расстояния
  abs(y - (a * x + b)) / sqrt(1 + a^2)
}

# Функция линейной регрессии методом наименьших квадратов
linear_regression_manual <- function(x, y) {
  # Центрирование данных
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  # Вычисление коэффициентов
  numerator <- sum((x - x_mean) * (y - y_mean))
  denominator <- sum((x - x_mean)^2)
  
  # Коэффициент наклона
  slope <- numerator / denominator
  
  # Свободный член
  intercept <- y_mean - slope * x_mean
  
  # Расчет R-квадрат
  y_pred <- intercept + slope * x
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_pred)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  return(list(
    slope = slope,
    intercept = intercept,
    r_squared = r_squared
  ))
}

# Функция поиска линий в наборе точек
find_lines <- function(x, y, max_lines = 2, distance_threshold = 0.5) {
  # Копия входных данных
  x_remaining <- x
  y_remaining <- y
  
  # Список для хранения линий
  lines <- list()
  
  # Цикл поиска линий
  for (line_num in 1:max_lines) {
    best_line <- NULL
    max_inliers_count <- 0
    
    # Перебор всех подмножеств точек
    for (i in 1:(length(x_remaining) - 1)) {
      for (j in (i+1):length(x_remaining)) {
        # Линейная регрессия по двум точкам
        current_line <- linear_regression_manual(
          x_remaining[c(i, j)], 
          y_remaining[c(i, j)]
        )
        
        # Подсчет инлайеров
        distances <- sapply(
          1:length(x_remaining), 
          function(k) point_line_distance(
            x_remaining[k], 
            y_remaining[k], 
            current_line
          )
        )
        
        # Количество точек, близких к линии
        inliers <- sum(distances <= distance_threshold)
        
        # Обновление лучшей линии
        if (inliers > max_inliers_count) {
          best_line <- current_line
          max_inliers_count <- inliers
          best_inliers_indices <- which(distances <= distance_threshold)
        }
      }
    }
    
    # Если линия не найдена, прекращаем поиск
    if (is.null(best_line)) break
    
    # Сохранение линии
    lines[[line_num]] <- best_line
    
    # Удаление точек линии
    x_remaining <- x_remaining[-best_inliers_indices]
    y_remaining <- y_remaining[-best_inliers_indices]
    
    # Остановка если мало точек
    if (length(x_remaining) < 10) break
  }
  
  return(list(
    lines = lines,
    x_remaining = x_remaining,
    y_remaining = y_remaining
  ))
}

# Визуализация результатов
visualize_line_approximation <- function(x, y, approximation_result) {
  # Создание графика
  plot(x, y, col = 'gray', 
       main = 'Аппроксимация комбинации линий',
       xlab = 'X', ylab = 'Y')
  
  # Цвета для линий
  line_colors <- c('red', 'blue', 'green', 'purple')
  
  # Отрисовка аппроксимированных линий
  for (i in 1:length(approximation_result$lines)) {
    model <- approximation_result$lines[[i]]
    
    # Диапазон x для линии
    x_line <- seq(min(x), max(x), length.out = 100)
    y_line <- model$intercept + model$slope * x_line
    
    # Отрисовка линии
    lines(x_line, y_line, 
          col = line_colors[i %% length(line_colors) + 1], 
          lwd = 2)
    
    # Вывод параметров линии
    legend_text <- sprintf(
      "Линия %d: y = %.2f + %.2f*x (R² = %.2f)", 
      i, model$intercept, model$slope, model$r_squared
    )
    legend("topleft", legend = legend_text, 
           text.col = line_colors[i %% length(line_colors) + 1], 
           bty = "n", cex = 0.7, 
           inset = c(0, 0.1 * (i-1)))
  }
  
  # Точки оставшихся точек
  if (length(approximation_result$x_remaining) > 0) {
    points(approximation_result$x_remaining, 
           approximation_result$y_remaining, 
           col = 'black', pch = 16)
  }
}

# Пример использования
set.seed(123)

# Генерация зашумленных точек
noisy_data <- generate_noisy_lines(n_points = 200, noise_level = 0.5)

# Выполнение аппроксимации линий
approximation_result <- find_lines(
  noisy_data$x, noisy_data$y, 
  max_lines = 2, 
  distance_threshold = 0.5
)

# Визуализация результатов
visualize_line_approximation(noisy_data$x, noisy_data$y, approximation_result)

lines_signal <- find_all_lines(time, noisy_signal, angles_deg)

# Ограничение на количество линий
max_lines <- 2
if (length(lines_signal) > max_lines) {
  lines_signal <- lines_signal[1:max_lines]
}

# Визуализация
plot(time, noisy_signal, col = "blue", pch = 16, xlab = "Time", ylab = "Signal", main = "Анализ сигналов")
for (line in lines_signal) {
  abline(a = line$b, b = line$k, col = "red", lwd = 2)
}
legend("topleft", legend = c("Зашумленный сигнал", "Аппроксимация линий"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
