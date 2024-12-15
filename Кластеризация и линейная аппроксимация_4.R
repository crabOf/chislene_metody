# Функция генерации зашумленных точек с новым расположением
generate_noisy_lines_v4 <- function(n_points = 400, noise_level = 1) {
  # Создание точек для первой линии
  x1 <- runif(n_points / 4, 0, 5)
  y1 <- 2 + 1.2 * x1 + rnorm(n_points / 4, 0, noise_level * runif(n_points / 4))
  
  # Создание точек для второй линии
  x2 <- runif(n_points / 4, 5, 10)
  y2 <- -1 + 0.5 * x2 + rnorm(n_points / 4, 0, noise_level * runif(n_points / 4))
  
  # Создание точек для третьей линии
  x3 <- runif(n_points / 4, 10, 15)
  y3 <- 4 - 0.7 * x3 + rnorm(n_points / 4, 0, noise_level * runif(n_points / 4))
  
  # Создание точек для четвертой линии
  x4 <- runif(n_points / 4, 15, 20)
  y4 <- 1 + 0.3 * x4 + rnorm(n_points / 4, 0, noise_level * runif(n_points / 4))
  
  # Объединение точек
  x <- c(x1, x2, x3, x4)
  y <- c(y1, y2, y3, y4)
  
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
       main = 'Аппроксимация линий по группам (новое расположение)',
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

# Генерация зашумленных точек с новым располож ением
noisy_data_v4 <- generate_noisy_lines_v4(n_points = 400, noise_level = 1)

# Визуализация графика "колена"
elbow_method(noisy_data_v4)

# Проведение анализа и аппроксимации линий
k <- 4  # Количество групп, выбранное на основе графика "колена"
result <- analyze_and_fit_lines(noisy_data_v4, k)

# Визуализация результатов
visualize_results(noisy_data_v4, result$lines, result$groups, k)
