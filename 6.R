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
  
  return(list(x = x, y = y))
}

# Функция для выполнения K-средних
k_means_manual <- function(x, y, k) {
  set.seed(123)
  centers <- data.frame(x = sample(x, k), y = sample(y, k))
  prev_centers <- data.frame(x = numeric(k), y = numeric(k))
  
  while (!all(centers == prev_centers)) {
    distances <- as.matrix(dist(rbind(centers, data.frame(x = x, y = y))))
    cluster_assignments <- apply(distances[1:k, (k + 1):(k + length(x))], 2, which.min)
    
    prev_centers <- centers
    
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
  cluster_assignments <- k_means_manual(x, y, k)
  lines <- list()
  
  for (i in 1:k) {
    group_x <- x[cluster_assignments == i]
    group_y <- y[cluster_assignments == i]
    
    if (length(group_x) < 2) next
    
    line <- linear_regression_manual(group_x, group_y)
    lines[[i]] <- line
  }
  
  return(lines)
}

# Визуализация результатов
visualize_results <- function(x, y, lines, k) {
  plot(x, y, col = 'gray', 
       main = 'Аппроксимация линий по группам (хаотичный разброс)',
       xlab = 'X', ylab = 'Y')
  
  line_colors <- rainbow(k)
  
  for (i in 1:length(lines)) {
    model <- lines[[i]]
    
    x_line <- seq(min(x), max(x), length.out = 100)
    y_line <- model$intercept + model$slope * x_line
    
    lines(x_line, y_line, 
          col = line_colors[i], 
          lwd = 2)
  }
}

# Пример использования
set.seed(123)

# Генерация зашумленных точек с хаотичным разбросом
noisy_data_v3 <- generate_noisy_lines_v3(n_points = 300, noise_level = 1)

# Проведение анализа и аппроксимации линий
k <- 3  # Количество групп
lines_v3 <- analyze_and_fit_lines(noisy_data_v3$x, noisy_data_v3$y, k)

# Визуализация результатов
visualize_results(noisy_data_v3$x, noisy_data_v3$y, lines_v3, k)