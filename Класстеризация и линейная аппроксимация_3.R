# Функция генерации зашумленных точек с хаотичным разбросом
generate_noisy_lines_v3 <- function(n_points = 300, noise_level = 1) {
  # Создание точек для первой линии с хаотичным разбросом
  x1 <- runif(n_points / 3, 0, 5)
  y1 <- 1 + 0.5 * x1 + rnorm(n_points / 3, 0, noise_level * runif(n_points / 3))
  
  # Создание точек для второй линии с хаотичным разбросом
  x2 <- runif(n_points / 3, 5, 10)
  y2 <- -2 + 0.8 * x2 + rnorm(n_points / 3, 0, noise_level * runif(n_points / 3))
  
  # Создание точек для третьей линии с хаотичным разбросом
  x3 <- runif(n_points / 3, 10, 15)
  y3 <- 3 - 0.4 * x3 + rnorm(n_points / 3, 0, noise_level * runif(n_points / 3))
  
  # Объединение точек
  x <- c(x1, x2, x3)
  y <- c(y1, y2, y3)
  
  return(data.frame(x = x, y = y))
}

# Функция линейной регрессии методом наименьших квадратов
linear_regression_manual <- function(x, y) {
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  numerator <- sum((x - x_mean) * (y - y_mean))
  denominator <- sum((x - x_mean)^2)
  
  slope <- numerator / denominator
  intercept <- y_mean - slope * x_mean
  
  return(list(slope = slope, intercept = intercept))
}

# Функция для анализа и аппроксимации линий
analyze_and_fit_lines <- function(data, k) {
  lines <- list()
  
  # K-средние
  cl <- kmeans(data, centers = k)
  
  for (i in 1:k) {
    group_data <- data[cl$cluster == i, ]
    
    if (nrow(group_data) < 2) next
    
    line <- linear_regression_manual(group_data$x, group_data$y)
    lines[[i]] <- line
  }
  
  return(list(lines = lines, groups = cl$cluster))
}

# Визуализация результатов
visualize_results <- function(data, lines, groups, k) {
  plot(data$x, data$y, col = 'gray', 
       main = 'Аппроксимация линий по группам (хаотичный разброс)',
       xlab = 'X', ylab = 'Y')
  
  line_colors <- rainbow(k)
  
  for (i in 1:length(lines)) {
    model <- lines[[i]]
    
    x_line <- seq(min(data$x), max(data$x), length.out = 100)
    y_line <- model$intercept + model$slope * x_line
    
    lines(x_line, y_line, 
          col = line_colors[i], 
          lwd = 2)
  }
  
  # Добавление меток кластеров
  points(data$x, data$y, col = line_colors[groups], pch = 19)
}

# Функция для графика "колена"
elbow_method <- function(data, max_k = 10) {
  wss <- numeric(max_k)
  
  for (k in 1:max_k) {
    kmeans_result <- kmeans(data, centers = k)
    wss[k] <- kmeans_result$tot.withinss
  }
  
  plot(1:max_k, wss, type = "b", pch = 19, 
       xlab = "Количество кластеров (k)", 
       ylab = "Сумма квадратов расстояний (WSS)", 
       main = "Метод колена (Elbow Method)")
}

# Пример использования
set.seed(123)

# Генерация зашумленных точек с хаотичным разбросом
noisy_data_v3 <- generate_noisy_lines_v3(n_points = 300, noise_level = 1)

# Визуализация графика "колена"
elbow_method(noisy_data_v3)

# Проведение анализа и аппроксимации линий
k <- 3  # Количество групп, выбранное на основе графика "колена"
result <- analyze_and_fit_lines(noisy_data_v3, k)

# Визуализация результатов
visualize_results(noisy_data_v3, result$lines, result$groups, k)
