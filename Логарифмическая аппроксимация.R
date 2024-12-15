# Функция логарифмической аппроксимации методом МНК
logarithmic_approximation <- function(x, y) {
  # Проверка на неотрицательность
  if (any(x <= 0)) {
    stop("Все значения x должны быть положительными для логарифмической аппроксимации.")
  }
  
  # Логарифмирование значений x
  log_x <- log(x)
  
  # Количество точек
  n <- length(x)
  
  # Вычисление промежуточных сумм
  sum_log_x <- sum(log_x)
  sum_y <- sum(y)
  sum_log_x2 <- sum(log_x^2)
  sum_log_x_y <- sum(log_x * y)
  
  # Расчет коэффициентов методом наименьших квадратов
  # Уравнение: y = a + b * ln(x)
  
  # Знаменатель для коэффициентов
  denominator <- n * sum_log_x2 - sum_log_x^2
  
  # Коэффициент b
  b <- (n * sum_log_x_y - sum_log_x * sum_y) / denominator
  
  # Коэффициент a
  a <- (sum_y - b * sum_log_x) / n
  
  # Функция логарифмической аппроксимации
  logarithmic_func <- function(t) {
    a + b * log(t)
  }
  
  # Расчет R-квадрат
  y_pred <- sapply(x, logarithmic_func)
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_pred)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Возвращаем результаты
  return(list(
    a = a,                  # Коэффициент a
    b = b,                  # Коэффициент b
    func = logarithmic_func,# Функция аппроксимации
    r_squared = r_squared   # Коэффициент детерминации
  ))
}

# Пример использования
set.seed(123)
n <- 100

# Генерация тестовых данных с логарифмической зависимостью
x <- seq(1, 10, length.out = n)  # x должен быть положительным
y <- 2 + 3 * log(x) + rnorm(n, 0, 0.5)  # Логарифмическая зависимость с шумом

# Аппроксимация логарифмической функции
approx_result <- logarithmic_approximation(x, y)

# Создание точек для графика
x_plot <- seq(min(x), max(x), length.out = 200)
y_plot <- sapply(x_plot, approx_result$func)

# Построение графика
plot(x, y, col = 'red', main = 'Логарифмическая аппроксимация')
lines(x_plot, y_plot, col = 'blue', lwd = 2)

# Вывод результатов
cat("Коэффициент a:", approx_result$a, "\n")
cat("Коэффициент b:", approx_result$b, "\n")
cat("R-квадрат:", approx_result$r_squared, "\n")

# Формула аппроксимирующей функции
cat("Формула: y =", round(approx_result$a, 3), 
    "+", round(approx_result$b, 3), "* ln(x)\n")