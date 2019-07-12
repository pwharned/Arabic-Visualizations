library(tidyverse)
library(haven)
library(plotly)
remove(list = ls()) 

##create Arabic Language Labels for the Graphs

Q211c_label="النسبة المئوية للذين يقولون أن الرشوة ضرورية إلى حد ما أو ضرورية جداً للحصول على خدمات صحية أفضل"

Q211b_label="النسبة المئوية للذين يقولون أن الرشوة ضرورية إلى حد ما أو ضرورية جداً للحصول على خدمات تعليم أفضل"


Q855A_label="النسبة المئوية للذين يقولون أنهم شعروا بالتوتر في كثير من الأحيان أو معظم الأحيان خلال السنة الماضية"

Q855b_label="النسبة المئوية للذين يقولون أنهم شعروا بالاكتـئاب في كثير من الأحيان أو معظم الأحيان خلال السنة الماضية"


pathname="/Volumes/GoogleDrive/My Drive/R Projects/Captions and Titles.xlsx"
labeling = read_excel(pathname, sheet = 4) ##import arabic language labels from an excel sheet. this is to ensure 

country=c("Tunisia","Iraq","Jordan","Palestine","Libya","Lebanon","Morocco","Yemen","Egypt","Algeria","Sudan")

for (i in seq(1:11)){
  country[[i]]=labeling[[i]]
}


Q855A_dataframe =data_frame("Percent"=c(53, 49, 42,40,35,30,29,28,27,27,22), "Country"=country)  ##create data frames
Q211c_dataframe=data.frame("Percent"=c(47,56,26,34,29,66,64,54,69,56,55),"Country"=country)
Q211b_dataframe=data_frame("Percent"=c(43,52,25,31,28,63,46,50,60,45,51),"Country"=country)
Q855b_dataframe=data_frame("Percent"=c(40,43,34,37,23,30,20,26,25,20,15),"Country"=country)
dataframe_list=list(Q855b_dataframe,Q211b_dataframe,Q211c_dataframe,Q855A_dataframe)  ##create a list of data frames

###Create a plotting function 
plot1 = function(dtaf){
  use_labels(dtaf,{
   tempplot= ggplot(data = dtaf, aes(x=reorder(Country,+Percent), y=Percent)) + 
      geom_bar(stat = "identity", width = .5, color ="#FF9900", fill = "#FF9900")+ 
      geom_text(aes(label = Percent),nudge_y = 4)+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      coord_flip() +
      ylab("النسبة")+
      xlab("البلد")+
      ylim(0,100)+
      theme(panel.background = element_blank())+
      theme(axis.text.y=element_text(angle=45, hjust=.5, size = 15))+
      theme(plot.title = element_text(hjust=0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.ticks = element_blank())
  })}


temp = lapply(dataframe_list, plot1) #lapply to apply the function to all data frames

names(temp) <- c("Q855b", "Q211b","Q211c","Q855A") ##Names of plots
labels_list=list(Q855b_label,Q211b_label,Q211c_label,Q855A_label) ##create a list of labels that we will use to attach labels to graphs
list2env(temp, envir = .GlobalEnv)  ##unpack graphs into the environment 


library(orca)
plot_list=list()
for (j in labels_list){ ##iterate through each element of the labels list
  for (i in temp){ ### iterate through the list of temporary ggplots ( to which we need to add arabic labels)
    tmp=ggplotly(i+ggtitle(j)) ##create a new temporary plot that is a a plotly object with a corresponding arabic title
    for (y in names(temp)) { #lets append them to a list based on their name
      name <- paste(y)
      plot_list[[name]]=tmp  ##now each item in the list is one of our temprorary plot objects with the appropriate name
      for(plot in plot_list){ #lets iterate through our list of appopriately labeled plot objects so that we can export them
        orca(plot, file = paste(name,".png",sep=""))##time to export them with orca as .png
    }
    
  }
  
  }
}









