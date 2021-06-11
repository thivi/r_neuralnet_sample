library(tidyverse)
library(neuralnet)

print(iris)
iris <- iris %>% 
  mutate(Species=as_factor(Species) )

draw_boxplot <- function(){
  iris %>% 
    pivot_longer(1:4, names_to="attributes") %>% 
    ggplot(aes(attributes, value, fill=attributes)) +
    geom_boxplot()
}

draw_boxplot()

iris <- iris %>% 
  mutate(across(Sepal.Width, ~squish(.x, quantile(.x, c(0.05, 0.95)))))

iris <- iris %>% 
  mutate(across(1:4, scale))

draw_boxplot()

training_data_rows <- floor(0.70 * nrow(iris))         
set.seed(123)
training_indices <- sample(c(1:nrow(iris)), training_data_rows)

training_data <- iris[training_indices,]
test_data <- iris[-training_indices,]

nn=neuralnet(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
             data=training_data, hidden=c(5), linear.output = FALSE)
plot(nn)

predict <- function(data){
  prediction <- data.frame(neuralnet::compute(nn, 
                                              data.frame(data[,-5]))$net.result)
  labels <- c("setosa", "versicolor", "virginca")
  prediction_label <- data.frame(max.col(prediction)) %>% 
    mutate(prediction=labels[max.col.prediction.]) %>% 
    select(2) %>% 
    unlist()

  table(data$Species, prediction_label)
}

predict(training_data)
predict(test_data)
