# Функция параболической аппроксимации методом МНК
parabolic_approximation <- function(x, y) {
  # Количество точек
  n <- length(x)
  
  # Вычисление промежуточных сумм
  sum_x <- sum(x)
  sum_y <- sum(y)
  sum_x2 <- sum(x^2)
  sum_x3 <- sum(x^3)
  sum_x4 <- sum(x^4)
  sum_xy <- sum(x * y)
  sum_x2y <- sum(x^2 * y)
  
  # Создание системы нормальных уравнений
  matrix_coef <- matrix(c(
    n, sum_x, sum_x2,
    sum_x, sum_x2, sum_x3,
    sum_x2, sum_x3, sum_x4
  ), nrow = 3, byrow = TRUE)
  
  # Вектор свободных членов
  vector_free <- c(sum_y, sum_xy, sum_x2y)
  
  # Решение системы методом обратной матрицы
  tryCatch({
    # Попытка стандартного решения
    coeffs <- solve(matrix_coef, vector_free)
  }, error = function(e) {
    # Резервный метод с псевдообращением
    coeffs <- ginv(matrix_coef) %*% vector_free
  })
  
  # Функция параболы
  parabola_func <- function(t) {
    coeffs[1] + coeffs[2] * t + coeffs[3] * t^2
  }
  
  # Расчет R-квадрат
  y_pred <- sapply(x, parabola_func)
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_pred)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Возвращаем результаты
  return(list(
    coefficients = coeffs,  # a0, a1, a2
    func = parabola_func,   # Функция параболы
    r_squared = r_squared   # Коэффициент детерминации
  ))
}

# Пример использования
set.seed(123)
n <- 100
x <- seq(0, 10, length.out = n)
y <- 2 + 3*x - 0.5*x^2 + rnorm(n, 0, 3)

# Параболическая аппроксимация
approx_result <- parabolic_approximation(x, y)

# Создание точек для графика
x_plot <- seq(min(x), max(x), length.out = 200)
y_plot <- sapply(x_plot, approx_result$func)

# Построение графика
plot(x, y, col = 'red', main = 'Параболическая аппроксимация')
lines(x_plot, y_plot, col = 'blue', lwd = 2)

# Вывод результатов
cat("Коэффициенты (a0, a1, a2):", approx_result$coefficients, "\n")
cat("R-квадрат:", approx_result$r_squared, "\n")
