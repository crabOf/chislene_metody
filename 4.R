# Загрузка необходимых библиотек
library(ggplot2)      # Для визуализации данных
library(dplyr)       # Для манипуляции данными
library(cluster)     # Для иерархической кластеризации
library(factoextra)  # Для визуализации кластеризации

# Используем переменные hp и mpg
data(mtcars)
x <- mtcars$hp
y <- mtcars$mpg

# Объединяем x и y в датафрейм
data <- data.frame(hp = x, mpg = y)

# Стандартизация данных
data_scaled <- scale(data)

# Вычисление расстояний
dist_matrix <- dist(data_scaled)

# Иерархическая кластеризация с использованием метода "связи"
hc <- hclust(dist_matrix, method = "ward.D2")

# Определение количества кластеров
# Здесь мы выбираем 4 кластера
k <- 4
clusters <- cutree(hc, k)

# Добавление кластеров в исходные данные
data$cluster <- as.factor(clusters)

# Визуализация результатов кластеризации
ggplot(data, aes(x = hp, y = mpg, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Иерархическая кластеризация (4 кластера)",
       x = "Лошадиные силы (hp)",
       y = "Мили на галлон (mpg)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Линейная аппроксимация для каждого кластера
lines <- list()  # Список для хранения моделей

for (i in 1:k) {
  cluster_data <- data[data$cluster == i, ]  # Данные текущего кластера
  model <- lm(mpg ~ hp, data = cluster_data)  # Линейная регрессия
  lines[[i]] <- model  # Сохранение модели
  
  # Добавление линии регрессии на график
  ggplot(data, aes(x = hp, y = mpg)) +
    geom_point(aes(color = cluster), size = 3) +
    geom_smooth(data = cluster_data, aes(x = hp, y = mpg), method = "lm", se = FALSE, color = "black") +
    labs(title = paste("Линейная аппроксимация для кластера", i),
         x = "Лошадиные силы (hp)",
         y = "Мили на галлон (mpg)") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    geom_smooth(method = "lm", se = FALSE, color = "black")  # Линия регрессии
}

# Визуализация всех кластеров с линиями регрессии
ggplot(data, aes(x = hp, y = mpg, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Иерархическая кластеризация с линейной аппроксимацией",
       x = "Лошадиные силы (hp)",
       y = "Мили на галлон (mpg)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(method = "lm", se = FALSE, aes(group = cluster), color = "black")  # Линия регрессии для каждого кластера