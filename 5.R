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

# Функция для выполнения K-средних
k_means_manual <- function(x, y, k) {
  # Инициализация центров кластеров случайными точками
  set.seed(123)
  centers <- data.frame(x = sample(x, k), y = sample(y, k))
  
  # Переменная для хранения предыдущих центров
  prev_centers <- data.frame(x = numeric(k), y = numeric(k))
  
  while (!all(centers == prev_centers)) {
    # Присвоение точек к ближайшему центру
    distances <- as.matrix(dist(rbind(centers, data.frame(x = x, y = y))))
    cluster_assignments <- apply(distances[1:k, (k + 1):(k + length(x))], 2, which.min)
    
    # Сохранение предыдущих центров
    prev_centers <- centers
    
    # Обновление центров кластеров
    centers <- data.frame(
      x = tapply(x, cluster_assignments, mean),
      y = tapply(y, cluster_assignments, mean)
    )
  }
  
  return(cluster_assignments)
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
analyze_and_fit_lines <- function(x, y, k) {
  # Кластеризация
  cluster_assignments <- k_means_manual(x, y, k)
  
  # Список для хранения линий
  lines <- list()
  
  # Для каждой группы точек
  for (i in 1:k) {
    group_x <- x[cluster_assignments == i]
    group_y <- y[cluster_assignments == i]
    
    # Пропуск группы, если в ней недостаточно точек
    if (length(group_x) < 2) next
    
    # Линейная регрессия для группы
    line <- linear_regression_manual(group_x, group_y)
    lines[[i]] <- line
  }
  
  return(lines)
}

# Визуализация результатов
visualize_results <- function(x, y, lines, k) {
  plot(x, y, col = 'gray', 
       main = 'Аппроксимация линий по группам',
       xlab = 'X', ylab = 'Y')
  
  # Цвета для линий
  line_colors <- rainbow(k)
  
  # Отрисовка аппроксимированных линий
  for (i in 1:length(lines)) {
    model <- lines[[i]]
    
    # Диапазон x для линии
    x_line <- seq(min(x), max(x), length.out = 100)
    y_line <- model$intercept + model$slope * x_line
    
    # Отрисовка линии
    lines(x_line, y_line, 
          col = line_colors[i], 
          lwd = 2)
  }
}

# Пример использования
set.seed(123)

# Генерация зашумленных точек
noisy_data <- generate_noisy_lines(n_points = 200, noise_level = 0.5)

# # Проведение анализа и аппроксимации линий
k <- 2  # Количество групп
lines <- analyze_and_fit_lines(noisy_data$x, noisy_data$y, k)

# Визуализация результатов
visualize_results(noisy_data$x, noisy_data$y, lines, k)