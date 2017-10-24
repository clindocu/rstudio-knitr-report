# Function NoObs: number of non-missing values;
NoObs <- function(x) {
  sum(!is.na(x))
}

# Function LbyCI: Confidence Intervals;
LbyCI <- function(x, y, ...) {
  latex(
    bystats(x, y,
    fun = function(x) c("Mean" = mean(x), 
            "Median"    = median(x), 
            "SD"        = apply(x, 2, sd), 
            "95LowerCI" = (mean(x) - qt(.975, NoObs(x) - 1) * apply(x, 2, sd) / sqrt(NoObs(x))),
            "95UpperCI" = (mean(x) + qt(.975, NoObs(x) - 1) * apply(x, 2, sd) / sqrt(NoObs(x))),
            "Min"       = min(x),
            "Max"       = max(x)), nmiss = FALSE),
    file        = "",
    caption.loc = "top",
    caption     = myCaption,
    cdec        = c(0, 1, 1, 2, 1, 1, 1, 1),   # decimal places;
    label       = runif(1),                    # to avoid error: There were multiply-defined labels;
    rowlabel    = "",    
    here        = T, ...)
}

# Function LbyGEO: Geometric Mean, CIs;
# If x = 0 then e.g. converted to one (1) so that log will be set to zero (0) for the calculation (t.b.d.)
LbyGEO <- function(x, y, ...) {
  latex(
    bystats(x, y,
    fun = function(x) c('Geo.Mean' = exp(mean(log(x))), 
            'Median' = median(x), 
            'Min' = min(x),
            'Max' = max(x), 
            "95LowerCI" = exp(mean(log(x)) 
                          - qt(0.975, NoObs(x) - 1) * sd(log(x) / sqrt(NoObs(x)))),
            "95UpperCI" = exp(mean(log(x)) 
                          + qt(0.975, NoObs(x) - 1) * sd(log(x) / sqrt(NoObs(x))))), 
                          nmiss = FALSE), 
    file        = "",
    caption.loc = 'top',
    caption     = myCaption,
    cdec        = c(0, 1, 1, 0, 0, 1, 1),       # decimal places;
    label       = runif(1),
    rowlabel    = "",
    here        = T, ...)
}

# Function LbyQUART: Mean, Median, Quartils;
LbyQUART <- function(x, y, ...){
  latex(
    bystats(x, y,
    fun = function(x) c("Mean"   = mean(x), 
                        "Median" = median(x), 
                        "SD"     = apply(x, 2, sd),
                        "Q1"     = apply(x, 2, quantile, c(.25)),
                        "Q3"     = apply(x, 2, quantile, c(.75)),
                        "Min"    = min(x),
                        "Max"    = max(x)), nmiss = FALSE),
     file        = "",
     caption.loc = "top",
     caption     = myCaption,
     cdec        = c(0, 1, 1, 2, 1, 1, 1, 1),      # decimal places;
     label       = runif(1),
     rowlabel    = "",    
     here        = T, ...)
}

# Function BiFreqCI: Wilson score CIs;
BiFreqCI <- function(x, cap) {
  a <- table(x)
  b <- cbind(a, "all" = sum(a))
  erg <- binconf(b[, "a"], b[, "all"], method = "wilson")
  erg2 <- cbind("EVENT" = b[, "a"],"N" = b[, "all"], round(erg * 100, 1))
  latex(as.data.frame(erg2), file = "", here = TRUE, title = "95\\% Wilson CIs", 
        caption = cap, label = runif(1))
}

# Function KMquart: Kaplan Meier quartiiles;
KMquart <- function(fit, cap){
  Category <- as.data.frame(quantile(fit))
  latex(Category, file = "", digits = 3, landscape = FALSE, here = T, size = "small", 
        long = TRUE, caption = cap, label = runif(1))
}

