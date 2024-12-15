# Функция МНК-аппроксимации
least_squares_approximation <- function(x, y, degree) {
  # Проверка входных данных
  if (length(x) != length(y)) {
    stop("Длины векторов x и y должны совпадать")
  }
  
  # Создание матрицы дизайна
  X <- matrix(1, nrow = length(x), ncol = degree + 1)
  for (i in 1:degree) {
    X[, i + 1] <- x^i
  }
  
  # Вычисление коэффициентов методом МНК
  # Формула: beta = (X^T * X)^-1 * X^T * y
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  
  # Robust решение с обработкой вырожденных матриц
  tryCatch({
    coeffs <- solve(XtX, Xty)
  }, error = function(e) {
    coeffs <- ginv(XtX) %*% Xty
  })
  
  # Создание функции аппроксимации
  approx_func <- function(t) {
    result <- coeffs[1]
    for (i in 1:degree) {
      result <- result + coeffs[i + 1] * t^i
    }
    return(result)
  }
  
  # Расчет статистик
  y_pred <- X %*% coeffs
  residuals <- y - y_pred
  
  # Расчет R-квадрат
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum(residuals^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Стандартная ошибка
  std_error <- sqrt(ss_residual / (length(x) - degree - 1))
  
  # Возврат результатов
  return(list(
    coefficients = coeffs,    # Коэффициенты
    approximation_func = approx_func,  # Функция аппроксимации
    r_squared = r_squared,    # Коэффициент детерминации
    std_error = std_error,    # Стандартная ошибка
    residuals = residuals     # Остатки
  ))
}

# Пример использования
set.seed(123)
n <- 100
x <- seq(0, 10, length.out = n)

# Генерация тестовых данных с полиномиальной зависимостью
y <- 2 + 3*x - 0.5*x^2 + rnorm(n, 0, 5)

# Аппроксимация различной степени
degrees <- c(1, 2, 3)

# Графическое представление
par(mfrow = c(length(degrees), 1), mar = c(4, 4, 2, 1))

for (degree in degrees) {
  # Выполнение аппроксимации
  approx_result <- least_squares_approximation(x, y, degree)
  
  # Создание точек для графика аппроксимации
  x_plot <- seq(min(x), max(x), length.out = 200)
  y_plot <- sapply(x_plot, approx_result$approximation_func)
  
  # Построение графика
  plot(x, y, col = 'red', main = paste('Аппроксимация степени', degree))
  lines(x_plot, y_plot, col = 'blue', lwd = 2)
  
  # Вывод результатов
  cat("Степень полинома:", degree, "\n")
  cat("Коэффициенты:", approx_result$coefficients, "\n")
  cat("R-квадрат:", approx_result$r_squared, "\n")
  cat("Стандартная ошибка:", approx_result$std_error, "\n\n")
}
# Сброс параметров графика
par(mfrow = c(1, 1))