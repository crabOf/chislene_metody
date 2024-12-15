# Функция линейной аппроксимации методом наименьших квадратов
linear_approximation <- function(x, y) {
  # Количество точек
  n <- length(x)
  
  # Вычисление сумм
  sum_x <- sum(x)
  sum_y <- sum(y)
  sum_xy <- sum(x * y)
  sum_x_squared <- sum(x^2)
  
  # Вычисление коэффициентов методом МНК
  # k - тангенс угла наклона
  # b - смещение
  k <- (n * sum_xy - sum_x * sum_y) / (n * sum_x_squared - sum_x^2)
  b <- (sum_y - k * sum_x) / n
  
  # Функция линейной регрессии
  linear_func <- function(t) {
    k * t + b
  }
  
  # Расчет коэффициента детерминации R^2
  y_mean <- mean(y)
  ss_total <- sum((y - y_mean)^2)
  ss_residual <- sum((y - (k * x + b))^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Возвращаем результаты
  return(list(
    slope = k,           # Угол наклона
    intercept = b,       # Смещение
    func = linear_func,  # Функция регрессии
    r_squared = r_squared  # Коэффициент детерминации
  ))
}

# Пример использования с генерацией тестовых данных
set.seed(123)  # Для воспроизводимости
n <- 100
x <- seq(1, 10, length.out = n)
y <- 2 * x + 1 + rnorm(n, 0, 2)  # Линейная функция с добавлением шума

# Линейная аппроксимация
approx_linear <- linear_approximation(x, y)

# Создаем последовательность для графика
x_plot <- seq(min(x), max(x), length.out = 100)
y_plot <- sapply(x_plot, approx_linear$func)

# Построение графиков
plot(x, y, type='p', col='red', main='Линейная аппроксимация')  # Исходные точки
lines(x_plot, y_plot, col='blue')  # Аппроксимированная линия

# Вывод результатов
cat("Угол наклона (k):", approx_linear$slope, "\n")
cat("Смещение (b):", approx_linear$intercept, "\n")
cat("Коэффициент детерминации R^2:", approx_linear$r_squared, "\n")