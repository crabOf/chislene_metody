# Функция аппроксимации показательной функции методом логарифмической линеаризации
exponential_approximation <- function(x, y) {
  # Логарифмирование значений y
  log_y <- log(y)
  
  # Количество точек
  n <- length(x)
  
  # Вычисление промежуточных сумм
  sum_x <- sum(x)
  sum_log_y <- sum(log_y)
  sum_x2 <- sum(x^2)
  sum_x_log_y <- sum(x * log_y)
  
  # Расчет коэффициентов методом наименьших квадратов
  # Линеаризованное уравнение: ln(y) = ln(a) + b*x
  # Где a - масштабный множитель, b - показатель экспоненты
  
  # Знаменатель для коэффициентов
  denominator <- n * sum_x2 - sum_x^2
  
  # Коэффициент b (показатель экспоненты)
  b <- (n * sum_x_log_y - sum_x * sum_log_y) / denominator
  
  # Коэффициент ln(a)
  ln_a <- (sum_log_y - b * sum_x) / n
  
  # Восстановление a
  a <- exp(ln_a)
  
  # Функция показательной аппроксимации
  exponential_func <- function(t) {
    a * exp(b * t)
  }
  
  # Расчет R-квадрат
  y_pred <- sapply(x, exponential_func)
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_pred)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Возвращаем результаты
  return(list(
    a = a,                  # Масштабный множитель
    b = b,                  # Показатель экспоненты
    func = exponential_func,# Функция аппроксимации
    r_squared = r_squared   # Коэффициент детерминации
  ))
}

# Пример использования
set.seed(123)
n <- 100

# Генерация тестовых данных с экспоненциальной зависимостью
x <- seq(0, 10, length.out = n)
y <- 2 * exp(0.5 * x) + rnorm(n, 0, 3)

# Проверка на неотрицательность
if (any(y <= 0)) {
  warning("Некоторые значения y <= 0. Логарифмирование может вызвать ошибку.")
  # Опционально: фильтрация или трансформация данных
  y <- y - min(y) + 1
}

# Аппроксимация показательной функции
approx_result <- exponential_approximation(x, y)

# Создание точек для графика
x_plot <- seq(min(x), max(x), length.out = 200)
y_plot <- sapply(x_plot, approx_result$func)

# Построение графика
plot(x, y, col = 'red', main = 'Показательная аппроксимация')
lines(x_plot, y_plot, col = 'blue', lwd = 2)

# Вывод результатов
cat("Масштабный множитель (a):", approx_result$a, "\n")
cat("Показатель экспоненты (b):", approx_result$b, "\n")
cat("R-квадрат:", approx_result$r_squared, "\n")

# Формула аппроксимирующей функции
cat("Формула: y =", round(approx_result$a, 3), 
    "* exp(", round(approx_result$b, 3), " * x)\n")