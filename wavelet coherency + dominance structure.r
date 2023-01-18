library(WaveletComp)
library(tidyr)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

# Исходные данные - 4 таблицы (XX век левый и правый берега Енисея, XXI век то же самое)
# Исходные таблицы имеют следующую структуру: первый столбец - дата (год), последующие столбцы - численность каждого вида по отдельности
Shrew_name <- c("S._araneus", "S._caecutiens", 
                "S._isodon",	"S._minutus",		"S._roboratus",	"S._tundrensis") # Вектор с видовыми названиями землероек
Rodent_name <- c("M._oeconomus","C._rufocanus",
                 "C._rutilus",	"M._agrestis",	
                 "M._schisticolor",	"S._betulina") #То же самое для грызунов
Com <- c("Bсе сообщество", "Без доминантов")  #И для всего сообщества в целом
Com <- c("Bсе сообщество")


# Оценка когерентности вейвлетов временных рядов колебаний численности на обоих берегах

cross_wavelet <- function(L,R,Spec){
#подготовка датафрейма
  setwd("A:/numbers_of_mammals/wavelet+cluster+autocorrelation")
  if (L[1,1] > 1999 & R[1,1] > 1991){
    cat <- "XXI"
  } 
  else {
    if (L[1,1] < 1999 & R[1,1] < 1999){
      cat <- "XX"
    }
    else {warning("ошибка! таблицы из разных отловов!")}
  }
  
  df_average <- data.frame()
  L <- L%>%
    rename_with(~paste0("LB_", .x), .cols=-1)
  R <- R%>%
    rename_with(~paste0("RB_", .x), .cols=-1)
  total <- full_join(L,R)%>%
    mutate(date = as.Date(as.character(date), format = "%Y"))
	
	
	#Функция, которая проводит расчет когерентности и строит спектрограммы, которые сохранит в виде изображений в рабочих директориях
	#а осредненные по временной шкале значения когерентности сохранит в отдельный датафрейм
  pl <- function(name){
    w <- analyze.coherency(total, c(paste("LB_",name, sep = ''), paste("RB_",name, sep = '')),
                          loess.span=0, 
                          upperPeriod = 5,
                          lowerPeriod=2, 
                          make.pval=T, n.sim=10)
    w2 <- data.frame(Power = w$Power.xy.avg,
                     Period = w$Period,
                     Spec = name)
    
  png(filename = paste(cat, '/', name,  '.png', sep = ''), width = 1430, height = 870, pointsize = 15, res = 100)
  wc.image(w, which.image = "wp", plot.coi = T, plot.arrow = F, main = "", siglvl.contour = 0.05, show.date = F,
                  periodlab = '', timelab = '', plot.legend = F, label.time.axis = F, clear.area = F, siglvl.area = 0.05, lwd = 20)
  dev.off()
  return(w2) 
  }
  for (i in Spec){
    new <-  pl(i)
    df_average <- rbind(df_average, new)
  }
return(df_average)
 } 
 
 
 
 #Эта функция выведет график численности по векам и видам (нужно подавать на вход Spec_for_plot вектор с видовыми названиями)
 # для обоих берегов Енисея
num_both_bank <- function(L_XX, R_XX,L_XXI, R_XXI, Spec_for_plot){
  df <- L_XX%>%
    mutate(Bank = "L", Period = "XX")
  df2 <- R_XX%>%
    mutate(Bank = "R", Period = "XX")
  
  df3 <- L_XXI%>%
    mutate(Bank = "L", Period = "XXI")
  df4 <- R_XXI%>%
    mutate(Bank = "R", Period = "XXI")
  
  data <- bind_rows(df,df2, df3, df4)%>%
    pivot_longer(cols = c(-date, -Bank, - Period), names_to = "Spec", values_to = "Num")%>%
    filter(Spec %in% Spec_for_plot)

  data$Spec <- gsub('_', ' ', data$Spec)
  
  p <- ggplot(data, aes(date, Num, linetype = Bank))+
    geom_line()+
    geom_point()+
    facet_grid(Spec~Period, scales = "free_x", switch = "y")+
    xlab('Год')+
    ylab('Численность (экз/100 ц-с)')+
    labs(linetype = "Берег")+
    scale_x_continuous(breaks = c(1976,1980,1985,1990,1994, 2008,2013,2017, 2022))+
    #theme_bw()+
    theme(text=element_text(family="sans"),
          axis.title.x = element_text(size = 14, colour = "black"),
          axis.title.y = element_text(size = 13, colour = "black"),
          axis.text.x = element_text(size = 14,angle = 90, vjust = .5, colour = "black"),
          axis.text.y = element_text(size = 14, colour = "black"),
          plot.title = element_text(vjust = -6, size = 18),
          legend.text = element_text(size = 14, colour = "black"),
          legend.title = element_text(size = 14), 
          legend.background = element_blank(),
          legend.box.background = element_rect(fill = "transparent"),
          legend.position = c("bottom"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_rect(fill = "transparent"),
          strip.text = element_blank(),
          strip.placement = "outside")
          
  return(p)
  
} 


#  Построение гистограмм осредненных по временной шкале значений когерентности
# Которые ранее получены из функции pl
plot_average_cross_wavelet <- function(df, setting){
  df$Spec <- gsub('_', ' ', df$Spec)
    
  ggplot(df, aes(Period, Power))+
    geom_col()+
    facet_grid(Spec~., scales = "free_x", switch = "y")+
    xlab('Период')+
    ylab('')+
    coord_flip()+
    theme(text=element_text(family="sans"),
          axis.title.x = element_text(size = 14, colour = "black"),
          axis.title.y = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(size = 14,angle = 90, vjust = .5, colour = "black"),
          axis.text.y = element_text(size = 14, colour = "black"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_rect(fill = "transparent"),
          strip.text = setting,
          strip.placement = "outside"
          ) 
}


# Сохраним результат выполненных функций в датафреймы
shrew_XX <- cross_wavelet(LB_XX, RB_XX,Shrew_name)
shrew_XXI <- cross_wavelet(LB_XXI, RB_XXI,Shrew_name)

#Графики для землероек
plot_shrew <- num_both_bank(LB_XX, RB_XX,LB_XXI, RB_XXI, Shrew_name)
pl1 <- plot_average_cross_wavelet(shrew_XX, element_text(size = 14, colour = "black"))
pl2 <- plot_average_cross_wavelet(shrew_XXI, element_blank())

p <- pl1 + plot_shrew + pl2 +
  plot_layout(widths = c(1,3.5,1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(device = png, filename = 'Shrew.png',
       plot = p, bg ='transparent', width = 2481, height = 3000, units = "px" )



#Графики для грызунов
rodent_XX <- cross_wavelet(LB_XX, RB_XX,Rodent_name)
rodent_XXI <- cross_wavelet(LB_XXI, RB_XXI,Rodent_name)

plot_rodent <- num_both_bank(LB_XX, RB_XX,LB_XXI, RB_XXI, Rodent_name)
pl3 <- plot_average_cross_wavelet(rodent_XX, element_text(size = 14, colour = "black"))
pl4 <- plot_average_cross_wavelet(rodent_XXI, element_blank())

p_rodent <- pl3 + plot_rodent + pl4 +
  plot_layout(widths = c(1,5,1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.background = element_rect(fill = "transparent", color = NA))


ggsave(device = png, filename = 'Rodent.png',
       plot = p_rodent, bg ='transparent', width = 2481, height = 3000, units = "px" )


# Графики для всего сообщества
community_XX <- cross_wavelet(LB_XX, RB_XX, Com)
community_XXI <- cross_wavelet(LB_XXI, RB_XXI, Com)

plot_community <- num_both_bank(LB_XX, RB_XX,LB_XXI, RB_XXI, Com)
pl5 <- plot_average_cross_wavelet(community_XX, element_blank())
pl6 <- plot_average_cross_wavelet(community_XXI, element_blank())

p_comm <- pl5 + plot_community + pl6 +
  plot_layout(widths = c(1,4,1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(device = png, filename = 'Community.png',
       plot = p_comm, bg ='transparent', width = 2481, height = 1150, units = "px" )

#Сохраненные в виде изображений спектрограммы были наложены на фон в фотошопе



#Расчет структуры доминирования
dominant <- function(L_XX, R_XX,L_XXI, R_XXI){
  name <- c("S._araneus", "S._caecutiens", 
            "S._isodon",	"S._minutus",		"S._roboratus",	"S._tundrensis",
            "M._oeconomus","C._rufocanus", "C._rutilus",	"M._agrestis",	
            "M._schisticolor",	"S._betulina")
  com_L_XX <- L_XX%>%
    rename(Community= "Bсе сообщество")%>%
    select(date,Community)%>%
    mutate(Bank = "L", Period = "XX")
  com_R_XX <- R_XX%>%
    rename(Community= "Bсе сообщество")%>%
    select(date,Community)%>%
    mutate(Bank = "R", Period = "XX")
  
  com_L_XXI <- L_XXI%>%
    rename(Community= "Bсе сообщество")%>%
    select(date,Community)%>%
    mutate(Bank = "L", Period = "XXI")
  com_R_XXI <- R_XXI%>%
    rename(Community= "Bсе сообщество")%>%
    select(date,Community)%>%
    mutate(Bank = "R", Period = "XXI")
  community <- bind_rows(com_L_XX, com_R_XX, com_L_XXI, com_R_XXI)
  
  df <- L_XX%>%
    mutate(Bank = "L", Period = "XX")%>%
    select( -"Bсе сообщество", -Com_without_ara, -Without_ara_iso_cae, -"Без доминантов")
  df2 <- R_XX%>%
    mutate(Bank = "R", Period = "XX")%>%
    select( -"Bсе сообщество", -Com_without_ara, -Without_ara_iso_cae, -"Без доминантов")
 
  df3 <- L_XXI%>%
    mutate(Bank = "L", Period = "XXI")%>%
    select( -"Bсе сообщество", -"Без доминантов")
  df4 <- R_XXI%>%
    mutate(Bank = "R", Period = "XXI")%>%
    select( -"Bсе сообщество", -"Без доминантов")
  
  data <- bind_rows(df,df2, df3, df4)%>%
    pivot_longer(cols = c(-date, -Bank, - Period), names_to = "Spec", values_to = "Num")%>%
    left_join(community, by = c("date", "Bank", "Period"))%>%
    mutate(Dom = Num/Community)%>%
    mutate_if(is.numeric, round, digits=2)%>%
    arrange(date, Bank, desc(Dom))%>%
    filter(Spec %in% name)
    
    
  data$Spec <- gsub('_', ' ', data$Spec)
  data <- data %>%
    mutate(Spec = factor(Spec, levels=c("M. agrestis",  "M. schisticolor",
                                        "S. betulina", "M. oeconomus",
                                        "C. rufocanus",  "C. rutilus",
                                        "S. roboratus","S. isodon","S. minutus",
                                        "S. tundrensis","S. caecutiens",
                                        "S. araneus"))) %>%
    filter(is.na(Spec) == F)
   	
  data$Dom[data$Dom == 0] <- NA
  return(data)

}

b <- dominant(LB_XX, RB_XX, LB_XXI, RB_XXI)



#Построение тепловой карты со структурой доминирования
pl <- ggplot(b, aes(date,Spec, fill = Dom ))+
  geom_tile()+
  facet_grid(Bank ~ Period, scales = "free_x")+
  scale_fill_gradientn(colours = c("LightCyan", "Pink","Red"),
                       breaks = c(0.2,0.4,0.6),
                       na.value = "white")+
  ylab('')+
  xlab('')+
  labs(fill = "Доля вида")+
  scale_x_continuous(breaks = c(1976,1980,1985,1990,1994, 2008,2013,2017, 2022))+
  theme(text=element_text(family="sans"),
        axis.title.x = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 14,angle = 90, vjust = .5, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        legend.text = element_text(size = 14, colour = "black"),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(colour = "black", fill=NA))
        
ggsave(device = png, filename = 'dominant.png',
       plot = pl, bg ='transparent', width = 2481, height = 1900, units = "px" )