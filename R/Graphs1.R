library(ggplot2)

ggplot(Data3, 
       aes(x = Board, 
           fill = EscapeResponse)) + 
  geom_bar(position = "stack")+ scale_fill_grey() + theme_classic()

ggplot(hives, 
       aes(x = Predator, 
           fill = EscapeResponse)) + 
  geom_bar(position = "stack")+ scale_fill_grey() + theme_classic()
