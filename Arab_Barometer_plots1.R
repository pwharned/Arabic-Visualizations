
###One of the main issues we came across in producing simple visualizations in struggling with Arabic language support.
##Since Arabic is a right-left language, it is difficult both to work with inside a user-interface, as well as coerce graphs
##to display the language correctly. In R for example, it is extremely difficult to edit captions, which is why I began a project
##where i would create the captions we needed for our visualizations in a large excel file, apply them as labels across all variables
## and then use a few simple functions and for loops to automatically apply and produce the labels. 
##the code below is not an example of that ongoing project, but you can see that I had to pass the ggplots into plotly, as ggplot by itself
##does not support arabic language by itself.

library(tidyverse)
library(plotly)



R104_frame <- data.frame("Mean" = c(50,45,44, 33, 33, 30, 28, 27, 26, 22, 21), "Country" = c("السودان", "الاردن", "المغرب", "العراق","تونس","الجزائر","مصر","فلسطين","لبنان","اليمن","لبيبا"))
R104_frame=as.tibble(R104_frame)

plot1 = function(dtaf,var1, var2){
  use_labels(dtaf,{
    ggplot(data = dtaf, aes(x=reorder(var2,+var1), y=var1)) + 
      geom_bar(stat = "identity", width = .5, color ="#FF9900", fill = "#FF9900")+ 
      geom_text(aes(label = var1),nudge_y = 4)+
      coord_flip() +
      theme(axis.text.x=element_text(angle=45, hjust=1))+ 
      ylab("النسبة")+
      xlab("البلد")+
      ylim(0,100)+
      theme(panel.background = element_blank())+
      theme(plot.title = element_text(hjust=0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.text.y=element_text(angle=45, hjust=1.5, size = 15))
  })}
R104_plot = plot1(R104_frame,R104_frame$Mean, R104_frame$Country)
R104_plot=R104_plot+ggtitle("الذين يفكرون في الهجرة")
R104_plot= ggplotly(R104_plot) 
R104_plot

R211  <- data.frame("Mean" = c(59,56,45, 44, 44, 43, 36, 31, 28, 22), "Country" = c("السودان", "الاردن", "المغرب", "العراق","تونس","مصر","فلسطين","لبنان","اليمن","لبيبا"))



R211_plot=plot1(R211, R211$Mean, R211$Country)
R211_plot=R211_plot+ggtitle(" الذين يقولون ان الحكومة تحارب الفساد إلى درجة كبيرة او متوسطة٪")
ggplotly(R211_plot)

R211_plot= ggplotly(R211_plot) 

R211_plot

R210  <- data.frame("Mean" = c(77,89,71, 93, 90, 79, 83, 91, 72, 92), "Country" = c("السودان", "الاردن", "المغرب", "العراق","تونس","مصر","فلسطين","لبنان","اليمن","لبيبا"))
R210_plot=plot1(R210, R210$Mean, R210$Country)

R210_plot=R210_plot+ggtitle(" الذين يقولون أن الفساد منتشر  إلى درجة كبيرة أو متوسطة٪ ")
R210_plot= ggplotly(R210_plot) 
R210_plot

