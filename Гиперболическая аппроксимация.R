# Функция гиперболической аппроксимации методом МНК
hyperbolic_approximation <- function(x, y) {
  # Проверка на неотрицательность
  if (any(x <= 0)) {
    stop("Все значения x должны быть положительными для гиперболической аппроксимации.")
  }
  
  # Преобразование x в x_inv
  x_inv <- 1 / x
  
  # Количество точек
  n <- length(x)
  
  # Вычисление промежуточных сумм
  sum_x_inv <- sum(x_inv)
  sum_y <- sum(y)
  sum_x_inv2 <- sum(x_inv^2)
  sum_x_inv_y <- sum(x_inv * y)
  
  # Расчет коэффициентов методом наименьших квадратов
  # Уравнение: y = a * (1/x) + b
  # Система уравнений:
  # a * sum(1/x^2) + b * sum(1/x) = sum(y)
  # a * sum(1/x) + b * n = sum(y)
  
  # Знаменатель для коэффициентов
  denominator <- n * sum_x_inv2 - sum_x_inv^2
  
  # Коэффициент a
  a <- (n * sum_x_inv_y - sum_x_inv * sum_y) / denominator
  
  # Коэффициент b
  b <- (sum_y - a * sum_x_inv) / n
  
  # Функция гиперболической аппроксимации
  hyperbolic_func <- function(t) {
    a / t + b
  }
  
  # Расчет R-квадрат
  y_pred <- sapply(x, hyperbolic_func)
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_pred)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Возвращаем результаты
  return(list(
    a = a,                  # Коэффициент a
    b = b,                  # Коэффициент b
    func = hyperbolic_func, # Функция аппроксимации
    r_squared = r_squared    # Коэффициент детерминации
  ))
}

# Пример использования
set.seed(123)
n <- 100

# Генерация тестовых данных с гиперболической зависимостью
x <- seq(1, 10, length.out = n)  # x должен быть положительным
y <- 10 / x + rnorm(n, 0, 0.5)    # Гиперболическая зависимость с шумом

# Аппроксимация гиперболической функции
approx_result <- hyperbolic_approximation(x, y)

# Создание точек для графика
x_plot <- seq(min(x), max(x), length.out = 200)
y_plot <- sapply(x_plot, approx_result$func)

# Построение графика
plot(x, y, col = 'red', main = 'Гиперболическая аппроксимация')
lines(x_plot, y_plot, col = 'blue', lwd = 2)

# Вывод результатов
cat("Коэффициент a:", approx_result$a, "\n")
cat("Коэффициент b:", approx_result$b, "\n")
cat("R-квадрат:", approx_result$r_squared, "\n")

# Формула аппроксимирующей функции
cat("Формула: y =", round(approx_result$a, 3), 
    "/ x +", round(approx_result$b, 3), "\n")