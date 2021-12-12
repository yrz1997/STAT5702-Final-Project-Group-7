############################################################################################
#Title:"PSet 4"
#Output: Functions to produce missing plot

############################################################################################
### loading library ###

library(tidyverse)
library(patchwork)

############################################################################################

### Functions ###


main_plot<-function(tidydata){
  
  main<-ggplot(tidydata, aes(y = fct_rev(id),x = fct_reorder(key, -value, sum),  fill = as.factor(value))) +
    geom_tile(color = "white") + 
    scale_fill_manual(values = c(alpha('lightgrey',1), alpha('lightblue',1),alpha('grey',1)))+
    theme_classic()+
    xlab("variable")+
    ylab("missing patterns") +
    theme(legend.position = "None",
          axis.text.x=element_text(angle=15,vjust=0.6)) + 
    geom_text(label="complete cases",x=4,y=tidydata$id[which(tidydata$complete_group==0)][1], size=5)
  
  main
}


variable_count_plot<-function(dataset,pd, percent){
  
  pr="num"
  if(percent){
    pd$num=pd$num*100/nrow(dataset)
    pr="%"
    lt=c(0,100)
    br=seq(0,100,25)
  }
  
  side1=ggplot(pd,aes(y=num,x=fct_reorder(col,-num))) +
    geom_bar(stat="identity",fill="skyblue") +
    theme_bw() +
    labs(title = "Missing value patterns") +
    ylab(paste(pr,"rows \n missing:"))+ xlab("") +
    theme(axis.title = element_text(size=12,face="bold"),
          axis.text.x=element_text(angle=15,vjust=0.6))
  
  if(percent){
    side1=side1+
      scale_y_continuous(limits =lt ,breaks=br) 
  }
  
  side1
}


row_count_plot<-function(dataset,missing_patterns, percent){
  xlab="row count"
  if(percent){
    missing_patterns$count=missing_patterns$count*100/nrow(dataset)
    pr="%"
    lt=c(0,100)
    br=seq(0,100,25)
    xlab="% rows"
  }
  
  side2<-ggplot(missing_patterns,aes(y=id,x=count,fill=as.factor(complete_group))) +
    geom_bar(stat="identity") +
    theme_bw() +
    scale_fill_manual(values=c("#3C5488FF","skyblue")) +
    ylab("") + xlab(xlab) +
    theme(legend.position = "None",
          axis.title = element_text(size=12,face="bold"))
  
  if(percent){
    side2=side2+
      scale_x_continuous(limits =lt ,breaks=br) 
  }
  
  side2 
}



### 

missing_plot<-function(dataset,percent){
  missing_patterns <- data.frame(is.na(dataset)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  complete_group<-apply(missing_patterns[,1:(ncol(missing_patterns)-1)], 1, 
                        function(x) sum(as.numeric(x)))
  
  complete_group[complete_group>1]=1
  missing_patterns<-mutate(missing_patterns,complete_group)
  
  complete_index=which(missing_patterns$complete_group==0)
  
  tidydata <- missing_patterns %>% 
    rownames_to_column("id") %>% 
    gather(key, value,-id, -count, -complete_group)
  
  tidydata$value[which(tidydata$complete_group==0)] <-2
  
  main=main_plot(tidydata)
  
  pd=apply(dataset, 2, function(x) sum(is.na(x)))
  pd=data.frame(col=names(pd),num=as.numeric(pd))
  
  side1=variable_count_plot(dataset,pd, percent)
  
  missing_patterns <- missing_patterns %>% 
    rownames_to_column("id")
  
  missing_patterns$id<-factor(missing_patterns$id,levels=seq(nrow(missing_patterns),1))
  
  side2=row_count_plot(dataset,missing_patterns, percent)
  
  side1 +plot_spacer()+main+side2+plot_layout(ncol = 2, heights = c(0.2,0.7),widths = c(0.8,0.2))
}