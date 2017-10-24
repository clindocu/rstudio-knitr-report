## ---- tabCont ----
# SD has one decimal place more than other statistics;
tableContinuous(vars = vars, cap = cap, caption.placement = "top", lab = lab, 
        prec = c(0, 1, 1, 1, 1, 1, 1, 2, 1, 0), longtable = TRUE)

## ---- tabContBy ----
tableContinuous(vars = vars, cap = cap, caption.placement = "top", group = bye, lab = lab,  
        prec = c(0, 1, 1, 1, 1, 1, 1, 2, 1, 0), longtable = TRUE, print.pval = "kruskal")

## ---- tabNom ----
tableNominal(vars = vars, cap = cap, caption.placement = "top", vertical = FALSE, 
        lab = lab, longtable = TRUE, cumsum = FALSE, miss.cat = c(1:ncol(vars)))

## ---- tabNomBy ----
tableNominal(vars = vars, cap = cap, caption.placement = "top", group = bye, vertical = FALSE, 
        lab = lab, longtable = TRUE, cumsum = FALSE, 
        miss.cat = NA, print.pval = "fisher", fisher.B = Inf)

## ---- tabNomBy2 ----
tableNominal(vars = vars, cap = cap, caption.placement = "top", group = bye, vertical = FALSE, 
        lab = lab, longtable = TRUE, cumsum = FALSE, 
        miss.cat = c(1:ncol(vars)))

## ---- texreglm ----
texreg(model, caption.above = T, digits = 3, caption = cap, float.pos = "!htpb", 
       label = runif(1))

## ---- reglm ----
latex(xtable(model), digits = 2, file = "", title = "", here = TRUE, caption = cap, 
      label = runif(1))

## ---- HistPlot ----
# Histogram, density, normal distr., saphiro wilk;
p1 <- ggplot(df_hist, aes(x = Param)) + 
  geom_histogram(aes(y = ..density..), binwidth = myWidth, colour = "blue", fill = "white") +
  geom_density(colour = "blue") +
  stat_function(fun = dnorm, colour = "black", 
                args = list(mean = mean(df_hist$Param), sd = sd(df_hist$Param))) +
  xlab(myLabel) +
  theme_pubr()

# from https://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2/:
qqplot_data <- function(vec) {
  # argument: vector of numbers
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = vec)
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)
}

p2 <- qqplot_data(df_hist$Param) + theme_pubr()

grid.arrange(p1, p2, ncol = 2)

# Normality Test;
m1 <- shapiro.test(df_hist$Param)
d1 <- data.frame(Variable = myLabel, Test = m1$method, P.value = m1$p.value)
my_table <- xtable(d1, digits = 6)
print(my_table, caption.placement = "top", size = "small", include.rownames = FALSE,
      table.placement = "htb", digits = 6, tabular.environment = "longtable", floating = FALSE)

## ---- CatPlot ----
# polar chart;
p1 <- ggplot(df_cat, aes(x = Level, fill = Level)) +
  geom_bar() +
  coord_polar() +
  theme_bw() +
  theme(legend.position = "none") 

# bar chart;
p2 <- ggplot(df_cat, aes(x = Level, fill = Level)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percent") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# pie chart;
p3 <- ggplot(df_cat, aes(x = factor(1), fill = Level)) +
  geom_bar(width = 1, colour = "black") +
  coord_polar(theta = "y") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  guides(fill = guide_legend(title = NULL)) +
  theme_pubr() +
  theme(legend.text = element_text(angle = 45)) +
  theme(legend.position = "top")

grid.arrange(p1, p2, p3, ncol = 3)

## ---- KmPlot ----
ggsurvplot(fit, data             = df_KM,
                pval             = TRUE, 
                conf.int         = TRUE,
                xlab             = "Time in months",
                risk.table       = TRUE,              # Add risk table
                risk.table.col   = "strata",          # Change risk table color by groups
                linetype         = "strata",          # Change line type by groups
                surv.median.line = "hv",              # Specify median survival
                ggtheme          = theme_pubr()       # Change ggplot2 theme
)
fit
fit2

## ---- KmPlotCens ----
ggsurvplot(fit, data = df_KM,
                pval             = TRUE, 
                conf.int         = TRUE,
                xlab             = "Time in months",
                risk.table       = TRUE,             # Add risk table
                risk.table.col   = "strata",         # Change risk table color by groups
                linetype         = "strata",         # Change line type by groups
                surv.median.line = "hv",             # Specify median survival
                cumcensor        = TRUE,
                ggtheme          = theme_pubr()      # Change ggplot2 theme
)
fit
fit2

## ---- BoxPlotNoColor ----
ggplot(df_BOX, aes(Xaxis, Yaxis)) +
  geom_boxplot() +
  labs(x = xLab, y = yLab) + 
  stat_summary(fun.y = mean, colour = "blue", geom = "point", shape = 18, size = 2) +
  theme_pubr()

## ---- BoxPlot ----
ggplot(df_BOX, aes(Xaxis, Yaxis, fill = Xaxis)) +
  geom_boxplot() +
  labs(x = xLab, y = yLab) + 
  stat_summary(fun.y = mean, colour = "blue", geom = "point", shape = 8, size = 2) +
  guides(fill = FALSE) +
  theme_pubr()
  
## ---- HistSex ----
# Please adapt code, when you use other and/or more Groups than SEX Female/Male;
# Boxplot, density plot, qqplot, kruskall wallis, sphiro wilk;

# Boxplot 2 Groups of SEX;
p1 <- ggplot(mydata, aes(SEX, Param, fill = SEX)) +
  geom_boxplot() +
  labs(x = "Group", y = myLab) + 
  stat_summary(fun.y = mean, colour = "blue", geom = "point", shape = 18, size = 2) +
  guides(fill = FALSE) +
  theme_pubr()

# Histogramme Density Plots by SEX;
p2 <- ggplot(mydata, aes(x =  Param, fill = SEX)) +
  geom_density(alpha = .5) + guides(fill = FALSE) + 
  xlab("") + 
  theme_pubr()

# QQ-Plot 2 Groups of SEX;
p3 <- ggplot(mydata) +
  stat_qq(aes(sample = Param, colour = factor(SEX))) +
  guides(colour = "none") + 
  theme_pubr()

mydata1 <- subset(mydata,  SEX == "Female")
mydata2 <- subset(mydata,  SEX == "Male")
pNORM <- shapiro.test(mydata$Param)
pNORM1 <- shapiro.test(mydata1$Param)
pNORM2 <- shapiro.test(mydata2$Param)

# Kruskal test;
p_All <- kruskal.test(Param ~ SEX, data = mydata)	

test_res_mat <- matrix(c("Wilcoxon test p",
                         "Shapiro-Wilk p (overall)",
                         "p (Female)",
                         "p (Male)",
                         round(p_All$p.value, digits = 7),
                         round(pNORM$p.value, digits = 7),
                         round(pNORM1$p.value, digits = 7),
                         round(pNORM2$p.value, digits = 7)), byrow = FALSE, nrow = 4)
# Print matrix;
p4 <- tableGrob(test_res_mat)

# Grid;
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
