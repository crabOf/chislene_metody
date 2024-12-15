# Функция степенной аппроксимации методом МНК
power_approximation <- function(x, y) {
  # Проверка на неотрицательность
  if (any(x <= 0) || any(y <= 0)) {
    stop("Все значения x и y должны быть положительными для степенной аппроксимации.")
  }
  
  # Логарифмирование значений x и y
  log_x <- log(x)
  log_y <- log(y)
  
  # Количество точек
  n <- length(x)
  
  # Вычисление промежуточных сумм
  sum_log_x <- sum(log_x)
  sum_log_y <- sum(log_y)
  sum_log_x2 <- sum(log_x^2)
  sum_log_x_log_y <- sum(log_x * log_y)
  
  # Расчет коэффициентов методом наименьших квадратов
  # Уравнение: ln(y) = ln(a) + b * ln(x)
  
  # Знаменатель для коэффициентов
  denominator <- n * sum_log_x2 - sum_log_x^2
  
  # Коэффициент b
  b <- (n * sum_log_x_log_y - sum_log_x * sum_log_y) / denominator
  
  # Коэффициент ln(a)
  ln_a <- (sum_log_y - b * sum_log_x) / n
  
  # Восстановление a
  a <- exp(ln_a)
  
  # Функция степенной аппроксимации
  power_func <- function(t) {
    a * t^b
  }
  
  # Расчет R-квадрат
  y_pred <- sapply(x, power_func)
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_pred)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Возвращаем результаты
  return(list(
    a = a,                  # Коэффициент a
    b = b,                  # Коэффициент b
    func = power_func,      # Функция аппроксимации
    r_squared = r_squared    # Коэффициент детерминации
  ))
}

# Пример использования
set.seed(123)
n <- 100

# Генерация тестовых данных с степенной зависимостью
x <- seq(1, 10, length.out = n)  # x должен быть положительным
y <- 2 * x^1.5 + rnorm(n, 0, 2)    # Степенная зависимость с шумом

# Аппроксимация степенной функции
approx_result <- power_approximation(x, y)

# Создание точек для графика
x_plot <- seq(min(x), max(x), length.out = 200)
y_plot <- sapply(x_plot, approx_result$func)

# Построение графика
plot(x, y, col = 'red', main = 'Степенная аппроксимация')
lines(x_plot, y_plot, col = 'blue', lwd = 2)

# Вывод результатов
cat("Коэффициент a:", approx_result$a, "\n")
cat("Коэффициент b:", approx_result$b, "\n")
cat("R-квадрат:", approx_result$r_squared, "\n")

# Формула аппроксимирующей функции
cat("Формула: y =", round(approx_result$a, 3), 
    "* x^", round(approx_result$b, 3), "\n")