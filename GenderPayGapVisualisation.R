library(ggplot2)
library(plotly)
library(tidyverse)
library(viridis)

#data

pay_gap <- read_csv("https://raw.githubusercontent.com/Tomasz-Olczyk/wizualizacjaR/main/case%20studies%20/pay_gap_uk.csv")
head(pay_gap)

pay_gap$women_thou = pay_gap$women_average_annual_salary/1000
pay_gap$men_thou = pay_gap$men_average_annual_salary/1000
pay_gap$pay_gap_thou = pay_gap$pay_gap/1000
pay_gap_ord <- pay_gap[order(pay_gap$pay_gap, decreasing=T),]

# way2

vect_m = rep("Men", times=81)
vect_f = rep("Women", times=81)
pay_gap2_men = cbind(vect_m, pay_gap[,c("occupation","men_thou", "pay_gap_thou")])
colnames(pay_gap2_men)[1] = "gender"
colnames(pay_gap2_men)[3] = "salary"
pay_gap2_women = cbind(vect_f, pay_gap[,c("occupation","women_thou", "pay_gap_thou")])
colnames(pay_gap2_women)[1] = "gender"
colnames(pay_gap2_women)[3] = "salary"
pay_gap2 = rbind(pay_gap2_men, pay_gap2_women)


x_pre= pay_gap[,c(1,7:8)]
x_pre2 = x_pre[,c("occupation", 
                  pmin("women_thou","men_thou"), pmax("women_thou","men_thou"))] 
colnames(x_pre2)[2] = "xmin"
colnames(x_pre2)[3] = "xmax"
pay_gap3 = merge(pay_gap2, x_pre2, 
                 by.x = "occupation", by.y = "occupation", ALL.x = T, ALL.y = T)

pay_gap3_ord <- pay_gap3[order(pay_gap$pay_gap, decreasing=T),]

df_new <- data.frame(pay_gap3_ord)

gpg2_plot <- ggplot(data= df_new, aes(x = pay_gap_thou, 
                         y=reorder(occupation, pay_gap_thou),
                         text = paste('avg salary', format(salary, digits =3),
                                      '<br>for:', gender
                                      )))+
  geom_point(aes(x = salary, y = occupation, colour = factor(gender)))+
  geom_segment(aes(x = xmin, xend=xmax, 
                   yend = reorder(occupation, pay_gap_thou))) +
  scale_color_manual(values = c("Women" = "violet", "Men" = "green"))+
  labs(title = "Gender Pay Gap", 
       x = "yearly salary (thousands)", 
       y = "occupation",  
       colour = "gender" )+
  theme(legend.position ="top", legend.title = element_text(size = 8),
        plot.title = element_text(size = 10, hjust = 0, vjust = 1),
        axis.title.y = element_text(hjust=1, size = 8),
        axis.title.x = element_text(size = 8))

ggplotly(gpg2_plot, tooltip = "text")

  