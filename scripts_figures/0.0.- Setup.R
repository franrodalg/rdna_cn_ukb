suppressWarnings(suppressMessages(library(tidyverse)))
library(patchwork)
library(ggbreak)
library(glue)
library(ggtext)

root_fd <- file.path(here::here(), '..', '..', '..')

ci <- (1-0.05/2)

## VISUALISATION

fancy_scientific <- function(l, remove_zero = TRUE) {
  zero_pos <- 0
  l <- format(l, scientific = TRUE)
  
  if (remove_zero) {
    if (l[1] == "0e+00") {
      zero_pos <- 1
      l <- gsub("0e\\+00", "", l)
    } else if (sum(grepl(' 0e\\+00', l)) > 0) {
      zero_pos <- which(grepl(' 0e\\+00', l))
      l <- gsub("0e\\+00", "", l)
    }
  }
  else {
      l <- gsub("^0e\\+00", "0^// ", gsub(" 0e\\+00", "0^// ", l)) }
  l <- gsub("^(.*)e", "'\\1'e", l)
  l <- gsub("e", "%*%10^", l)
  if(remove_zero & zero_pos > 0) {
    if(zero_pos == 1) {
      c('', parse(text=l)) 
    } else {
      c(parse(text=l[1:(zero_pos-1)]), '',
        parse(text=l[zero_pos:length(l)])) }
  } else {
    parse(text = l)
  }
}

fancy_scientific_noz <- function(l) {fancy_scientific(l, remove_zero = FALSE)}

cor_format <- function(rvalue, pvalue) {
  paste0("`R`~`=`~", 
         ifelse(
           abs(rvalue) < 0.01, 
           fancy_scientific(rvalue),
           as.expression(rvalue)),
         "*`,`~`p`~`=`~", 
         ifelse(
           pvalue < 0.01,
           fancy_scientific(pvalue),
           as.expression(pvalue)))
}


theme_pub <- function(){ 
  
  theme_bw() +
    theme(
      panel.grid = element_blank(),  
      panel.border = element_rect(linewidth = 1.5),
      panel.spacing = unit(0.2, 'lines'),
      axis.ticks = element_line(linewidth = 1.3),
      axis.title = element_text(size = 15, face = 'bold'),
      axis.text = element_text(size = 13, colour = 'black'),
      strip.background = element_blank(),
      strip.text = element_text(size = 14)
    )
}

## FUNCTIONS

clean_phesant <- function(df, min_cases = 200, min_recorded = 157227/2) {
  df |> 
    filter(varType != 'CAT-SIN' | ! grepl('-', varName)) |>
    group_by(varName) |>
    separate(n, into = c('aux', 'total_n'),
             sep = '\\(', fill = 'left',remove = F) |>
    separate(aux, into = c('controls', 'cases'), sep='\\/' ) |> 
    filter(is.na(cases) | as.numeric(cases) >= min_cases) |>
    mutate(total_n = gsub('\\)', '', total_n)) |>
    filter(! is.na(as.numeric(total_n))) |>
    select(-n) |>
    mutate(total_n = as.numeric(total_n)) |>
    ungroup() |>
    filter(total_n >= min_recorded)
} 

fit_ratio <- function(df, the_ratio = 'NLR') {
  flm <- lm(CN ~ . , df |>
             select(Sex, starts_with('Age'), contains('Centre'), 
                    starts_with('PC'), starts_with('Adj'),
                    all_of(the_ratio),
                    CN))
  broom::tidy(lm.beta::lm.beta(flm),
    conf.int = TRUE) |> 
    mutate(lpval = -log10(p.value),
           scoeff = sign(estimate)) |>
    filter(term == the_ratio)
}

fit_ratio_scale <-function(df, the_ratio = 'NLR') {
  flm <- lm(as.formula(paste0(the_ratio,  '~ .')) , df |>
              mutate(across(where(is.numeric), scale)) |>
              select(Sex, starts_with('Age'), contains('Centre'), 
                     starts_with('PC'), starts_with('Adj'),
                     all_of(the_ratio),
                     CN))
  broom::tidy(flm, conf.int = TRUE) |> 
    mutate(lpval = -log10(p.value),
           scoeff = sign(estimate)) |>
    filter(term == 'CN') |>
    mutate(term = the_ratio) |>
    relocate(term)
}
