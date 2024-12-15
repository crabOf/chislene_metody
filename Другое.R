par(mfrow = c(1, 1))
# Реализация метода линейной аппроксимации комбинации линий по набору зашумленных точек
# без использования встроенных функций, кроме функций для визуализации результатов

# Вводим функцию для расчета коэффициентов линейной регрессии
linear_regression <- function(x, y) {
  # Проверяем, что длина x и y совпадает
  if (length(x) != length(y)) {
    stop("Длины x и y должны совпадать.")
  }
  
  # Число точек
  n <- length(x)
  
  # Рассчитываем средние значения x и y
  mean_x <- sum(x) / n
  mean_y <- sum(y) / n
  
  # Рассчитываем коэффициенты k (наклон) и b (смещение)
  numerator <- sum((x - mean_x) * (y - mean_y))
  denominator <- sum((x - mean_x)^2)
  
  if (denominator == 0) {
    stop("Деление на ноль: данные x не должны быть все одинаковыми.")
  }
  
  k <- numerator / denominator
  b <- mean_y - k * mean_x
  
  return(list(k = k, b = b))
}

# Функция для предсказания значений y по модели
predict <- function(x, k, b) {
  y_pred <- numeric(length(x))
  for (i in 1:length(x)) {
    y_pred[i] <- k * x[i] + b
  }
  return(y_pred)
}

# Создание зашумленных точек
set.seed(42) # Фиксируем случайное число для воспроизводимости
n_points <- 100
x <- seq(-10, 10, length.out = n_points)
true_k <- 2  # Настоящий наклон
true_b <- 3  # Настоящее смещение
noise <- rnorm(n_points, mean = 0, sd = 2) # Шум

y <- true_k * x + true_b + noise

# Выполняем линейную регрессию
coefficients <- linear_regression(x, y)
cat("Коэффициенты:", "k =", coefficients$k, "b =", coefficients$b, "\n")

# Предсказываем значения y на основе модели
y_pred <- predict(x, coefficients$k, coefficients$b)

# Визуализация результатов
plot(x, y, col = "blue", pch = 16, xlab = "x", ylab = "y", main = "Линейная аппроксимация")
lines(x, y_pred, col = "red", lwd = 2)
legend("topleft", legend = c("Зашумленные точки", "Модель"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
