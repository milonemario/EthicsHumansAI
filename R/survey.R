#library(tidyverse)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
#library(fillpattern)
library(stargazer)
library(modelsummary)
library(tinytable)
library(correlation)
#library(textab)
library(Hmisc)

library(lmtest)

results_all <- function(folder="Results") {
  
  #################
  # Summary Stats #
  #################
  table_human_sample(folder)
  table_llms(folder)
  
  ########################################
  # Main results (consistent with pilot) #
  ########################################
  results_policy_ethic(folder="Results", models="pilot", surveys="all", type="type")
  results_policy_characteristics(folder="Results", models="pilot", surveys="all", type="type")
  results_policy_change(folder="Results", models="pilot", surveys="all", type="type")
  regression_policy_change(folder="Results", models="pilot", surveys="all", type="type")
  results_textual_analysis(folder="Results", models="pilot", surveys="all", type="type")
  
  ################################################################
  # Main results with higher temperature (consistent with pilot) #
  ################################################################
  results_policy_ethic(folder="Results", models="pilot_temperature", surveys="all", type="type")
  results_policy_characteristics(folder="Results", models="pilot_temperature", surveys="all", type="type")
  results_policy_change(folder="Results", models="pilot_temperature", surveys="all", type="type")
  regression_policy_change(folder="Results", models="pilot_temperature", surveys="all", type="type")
  results_textual_analysis(folder="Results", models="pilot_temperature", surveys="all", type="type")
  
  #######################
  # Additional Analyses #
  #######################
  
  # All models by access (Human vs Online LLMs vs Local LLMs)
  results_policy_ethic(folder="Results", models="all", surveys="all", type="access")
  results_policy_characteristics(folder="Results", models="all", surveys="all", type="access")
  results_policy_change(folder="Results", models="all", surveys="all", type="access")
  #results_textual_analysis(folder="Results", models="all", surveys="all", type="access")
  
  # Online VS Local (gemini-1.5-flash-8b VS gemma2 9b)
  results_policy_ethic(folder="Results", models="onlinelocal", surveys="all", type="model")
  results_policy_characteristics(folder="Results", models="onlinelocal", surveys="all", type="model")
  results_policy_change(folder="Results", models="onlinelocal", surveys="all", type="model")
  #results_textual_analysis(folder="Results", models="onlinelocal", surveys="all", type="model")
  
  # Model size (Gemma and Llama models)
  results_policy_ethic(folder="Results", models="size", surveys="all", type="size")
  results_policy_characteristics(folder="Results", models="size", surveys="all", type="size")
  results_policy_change(folder="Results", models="size", surveys="all", type="size")
  #results_textual_analysis(folder="Results", models="size", surveys="all", type="size")
  
  # Model size with humans (Gemma and Llama models)
  results_policy_ethic(folder="Results", models="size_human", surveys="all", type="size")
  results_policy_characteristics(folder="Results", models="size_human", surveys="all", type="size")
  results_policy_change(folder="Results", models="size_human", surveys="all", type="size")
  #results_textual_analysis(folder="Results", models="size_human", surveys="all", type="size")
  
  # Model size - by model
  results_policy_ethic(folder="Results", models="size", surveys="all", type="model", plot_scale=scale_fill_discrete)
  results_policy_characteristics(folder="Results", models="size", surveys="all", type="model", plot_scale=scale_fill_discrete)
  results_policy_change(folder="Results", models="size", surveys="all", type="model", plot_scale=scale_fill_discrete)
  #results_textual_analysis(folder="Results", models="size", surveys="all", type="model", plot_scale=scale_fill_discrete)
  
  # Regressions
  
}

data_survey <- function(models="all") {
  
  df <- read.csv("data_survey.csv") %>%
    mutate(model = if_else(type=="human", "human", model))
  
  # Local vs Online Models
  online = c("gpt-4o", "gpt-4o-mini",
             "gemini-1.5-flash-002", "gemini-1.5-flash-8b")
  local = c("llama3.2:3b", "llama3.1:8b", "llama3.1:70b",
           "gemma2:2b", "gemma2:9b", "gemma2:27b",
           "mistral:7b", "phi3.5:3.8b")
  df <- df %>%
    mutate(access = "human") %>%
    mutate(access = if_else(model %in% online, "online", access)) %>%
    mutate(access = if_else(model %in% local, "local", access))
  
  # LLM model size
  big = c("gpt-4o", "gemini-1.5-flash-002", "gemma2:27b", "llama3.1:70b")
  medium = c("llama3.1:8b", "gemma2:9b", "mistral:7b", "phi3.5:3.8b", "gemini-1.5-flash-8b")
  small = c("llama3.2:3b", "gemma2:2b")
  df <- df %>%
    mutate(size = "Humans") %>%
    mutate(size = if_else(model %in% big, "Big", size)) %>%
    mutate(size = if_else(model %in% medium, "Medium", size)) %>%
    mutate(size = if_else(model %in% small, "Small", size))
  
  
  # Only keep some LLM for pilot consistency
  if (models=="all") {
    # In the base case, use default temperature
    df <- df %>%
      filter(is.na(temperature))
  } else if (models=="pilot") {
    df <- df %>%
      filter(is.na(temperature)) %>%
      filter(model %in% c("human",
                          "gpt-4o", "gemini-1.5-flash-002",
                          "llama3.1:8b", "mistral:7b"))
  } else if (models=="pilot_temperature") {
    df <- df %>%
      filter(temperature==1.4 | model=="human") %>%
      filter(model %in% c("human",
                          "gpt-4o", "gemini-1.5-flash-002",
                          "llama3.1:8b", "mistral:7b"))
  } else if (models=="onlinelocal") {
    df <- df %>%
      filter(is.na(temperature)) %>%
      filter(model %in% c("gemini-1.5-flash-8b", "gemma2:9b"))
  } else if (models=="size") {
    df <- df %>%
      filter(is.na(temperature)) %>%
      filter(model %in% c("gemma2:2b", "gemma2:9b", "gemma2:27b",
                          "llama3.2:3b", "llama3.1:8b", "llama3.1:70b"))
  } else if (models=="size_human") {
    df <- df %>%
      filter(is.na(temperature)) %>%
      filter(model %in% c("human",
                          "gemma2:2b", "gemma2:9b", "gemma2:27b",
                          "llama3.2:3b", "llama3.1:8b", "llama3.1:70b"))
  }
  
  # Create a weight for equalizing Online and Local LLM effect when pooled
  if (models %in% c("size", "size_human")) {
    df <- df %>%
      mutate(weight = 1)
  } else {
    # Number of llm vs sml
    n_models <- df %>%
      filter(type != "humans") %>%
      group_by(access) %>%
      summarise(n = length(unique(model)))
    n_online <- n_models %>% filter(access=="online") %>% pull(n)
    n_local <- n_models %>% filter(access=="local") %>% pull(n)
    
    df <- df %>%
      mutate(weight = 1) %>%
      mutate(weight = if_else(access=="online", n_local / n_online, weight))
  }
  
  # Printable names
  df <- df %>%
    mutate(across('type', \(x) str_replace(x, 'human', 'Humans'))) %>%
    mutate(across('type', \(x) str_replace(x, 'llm', 'LLMs'))) %>%
    mutate(across('access', \(x) str_replace(x, 'human', 'Humans'))) %>%
    mutate(across('access', \(x) str_replace(x, 'online', 'Online LLMs'))) %>%
    mutate(across('access', \(x) str_replace(x, 'local', 'Local LLMs'))) %>%
    mutate(across('model', \(x) str_replace(x, 'human', 'Humans'))) %>%
    mutate(across('model', \(x) str_replace(x, 'gpt-4o-mini', 'GPT 4o Mini'))) %>%
    mutate(across('model', \(x) str_replace(x, 'gpt-4o', 'GPT 4o'))) %>%
    mutate(across('model', \(x) str_replace(x, 'gemini-1.5-flash-002', 'Gemini'))) %>%
    mutate(across('model', \(x) str_replace(x, 'gemini-1.5-flash-8b', 'Gemini 8B'))) %>%
    mutate(across('model', \(x) str_replace(x, 'llama3.2:3b', 'Llama 3B'))) %>%
    mutate(across('model', \(x) str_replace(x, 'llama3.1:8b', 'Llama 8B'))) %>%
    mutate(across('model', \(x) str_replace(x, 'llama3.1:70b', 'Llama 70B'))) %>%
    mutate(across('model', \(x) str_replace(x, 'gemma2:2b', 'Gemma 2B'))) %>%
    mutate(across('model', \(x) str_replace(x, 'gemma2:9b', 'Gemma 9B'))) %>%
    mutate(across('model', \(x) str_replace(x, 'gemma2:27b', 'Gemma 27B'))) %>%
    mutate(across('model', \(x) str_replace(x, 'phi3.5:3.8b', 'Phi 3.5'))) %>%
    mutate(across('model', \(x) str_replace(x, 'mistral:7b', 'Mistral 7B')))
  
  # Order
  df <- df %>%
    mutate(type = factor(type, c("Humans", "LLMs"))) %>%
    mutate(access = factor(access, c("Humans", "Online LLMs", "Local LLMs"))) %>%
    mutate(recommend_new_policy = factor(recommend_new_policy, c("No", "No opinion", "Yes"))) %>%
    mutate(board_vote = factor(board_vote, c("Against", "Abstain", "Follow Consensus", "In Favor"))) %>%
    mutate(current_policy_ethic = factor(current_policy_ethic, c(1, 2, 3, 4, 5))) %>%
    mutate(new_policy_ethic = factor(new_policy_ethic, c(1, 2, 3, 4, 5))) %>%
    arrange(type, access)
  
  return(df)
}

wtd.stderror <- function(x, weights){
  var <- Hmisc::wtd.var(x, weights)
  weights <- sum( (weights / sum(weights))^2 )
  sqrt(var*weights)
}

table_human_sample <- function(folder="Results") {
  
  ########################
  # Summary stats pooled #
  ########################
  df <- data_survey()
  
  dfs <- df %>%
    filter(type=="Humans") %>%
    select(male, under_represented, business_econ, age,
           political_affiliation, gpa,
           #misreporting, cheating, trust
           ) %>%
    rename(Male = male,
           `Under-Represented` = under_represented,
           `Business or Econ` = business_econ,
           `Age` = age,
           `Right-Leaning` = political_affiliation,
           GPA = gpa,
           #Misreporting = misreporting,
           #Cheating = cheating,
           #Trust = trust
           ) %>%
    map_df(~c(length(.x[!is.na(.x)]),
              mean(.x, na.rm=TRUE),
              sd(.x, na.rm=TRUE),
              min(.x, na.rm=TRUE),
              quantile(.x, prob=.25, na.rm=TRUE),
              median(.x, na.rm=TRUE),
              quantile(.x, prob=.75, na.rm=TRUE),
              max(.x, na.rm=TRUE))) %>%
    mutate(name = c('N', 'Mean', 'Std', 'Min', '25\\%', 'Median', '75\\%', 'Max')) %>%
    gather(var_name, value, -name) %>% 
    pivot_wider(names_from=name, values_from=value) %>%
    rename(` `=var_name)
  
  filename = paste0(folder, "/tables/sample_human.tex")
  
  tt(dfs) %>%
    format_tt(digits=2, num_fmt = "decimal") %>%
    style_tt(j=2:ncol(dfs), align="c") %>%
    theme_tt("tabular", style="tabularray") %>%
    save_tt(filename, overwrite=T)
  
  
  ##########################################
  # Summary stats Over and Under reporting #
  ##########################################
  df <- data_survey()
  dfs <- list()
  for (s in c("overreporting", "underreporting")) {
    dfs[[s]] <- df %>%
      filter(type=="Humans", survey==!!s) %>%
      select(male, under_represented, business_econ, age, political_affiliation, gpa) %>%
      rename(Male = male,
             `Under-Represented` = under_represented,
             `Business or Econ` = business_econ,
             `Age` = age,
             `Right-Leaning` = political_affiliation,
             GPA = gpa) %>%
      map_df(~c(mean(.x, na.rm=TRUE),
                sd(.x, na.rm=TRUE),
                min(.x, na.rm=TRUE),
                quantile(.x, prob=.25, na.rm=TRUE),
                median(.x, na.rm=TRUE),
                quantile(.x, prob=.75, na.rm=TRUE),
                max(.x, na.rm=TRUE))) %>%
      mutate(name = c('Mean', 'Std', 'Min', '25\\%', 'Median', '75\\%', 'Max')) %>%
      gather(var_name, value, -name) %>% 
      pivot_wider(names_from=name, values_from=value) %>%
      rename(` `=var_name)
  }
  
  # Table
  filename = paste0(folder, "/tables/sample_human_overunderreporting.tex")
  rbind(dfs$overreporting, dfs$underreporting) %>%
    tt() %>%
    format_tt(digits=2, num_fmt = "decimal") %>%
    group_tt(i=list("Over-Reporting"=1, "Under-Reporting"=nrow(dfs$overreporting)+1)) %>%
    style_tt(i=1, italic=T) %>%
    style_tt(i=nrow(dfs$overreporting)+2, italic=T) %>%
    theme_tt("tabular", style="tabularray") %>%
    save_tt(filename, overwrite=T)
  
} 

table_llms <- function(folder="Results") {
  
  dfm <- tibble(
    model = c(
      "gpt-4o", "gpt-4o-mini", "gemini-1.5-flash-002", "gemini-1.5-flash-8b",
      "llama3.2:3b", "llama3.1:8b", "llama3.1:70b",
      "gemma2:2b", "gemma2:9b", "gemma2:27b",
      "phi3.5:3.8b", "mistral:7b"
      ),
    model_name = c(
      "GPT 4o", "GPT 4o Mini", "Gemini", "Gemini 8B",
      "Llama 3B", "Llama 8B", "Llama 70B",
      "Gemma 2B", "Gemma 9B", "Gemma 27B",
      "Phi", "Mistral 7B"
      ),
    model_full_name = c(
      "GPT 4o", "GPT 4o Mini", "Gemini 1.5 Flash", "Gemini 1.5 Flash 8B",
      "Llama 3.2 3B", "Llama 3.1 8B", "Llama 3.1 70B",
      "Gemma 2 2B", "Gemma 2 9B", "Gemma 2 27B",
      "Phi 3.5 3.8B", "Mistral 7B"
    ),
    baseline = c(
      "Yes", "No", "Yes", "No",
      "No", "Yes", "No",
      "No", "No", "No",
      "No", "Yes"
      ),
    company = c(
      "OpenAI", "OpenAI", "Google", "Google",
      "Meta", "Meta", "Meta",
      "Google", "Google", "Google",
      "Microsoft", "Mistral"
    ),
    version = c(
      "", "", "1.5", "1.5",
      "3.2", "3.1", "3.1",
      "2", "2", "2",
      "3.5", ""
    ),
    access = c(
      "Online", "Online", "Online", "Online",
      "Local", "Local", "Local",
      "Local", "Local", "Local",
      "Local", "Local"
      ),
    params = c(
      "Unknown", "Unknown", "Unknown", "8",
      "3", "8", "70",
      "2", "9", "27",
      "8", "7"
      ),
    )
  
  # Table for pilot
  model_names <- data_survey(models="pilot") %>%
    filter(type == "LLMs") %>%
    distinct(model) %>%
    pull(model)
  
  dft <- dfm %>%
    filter(model_name %in% model_names) %>%
    select(model_name, version, access, params, company) %>%
    rename(Model = model_name, Version = version,
           Access = access, `Parameters (Billions)` = params,
           Company = company)
  
  filename = paste0(folder, "/tables/stats_llms_pilot.tex")
  tt(dft) %>%
    style_tt(j=2:4, align="c") %>%
    theme_tt("tabular", style="tabularray") %>%
    save_tt(filename, overwrite=T)
  
  # All models
  model_names <- data_survey() %>%
    filter(type == "LLMs") %>%
    distinct(model) %>%
    pull(model)
  
  dft <- dfm %>%
    filter(model_name %in% model_names) %>%
    select(model_name, version, access, params, company, baseline) %>%
    rename(Model = model_name, Version = version,
           Access = access, `Parameters (Billions)` = params,
           Company = company, Baseline = baseline)
  
  filename = paste0(folder, "/tables/stats_llms_all.tex")
  tt(dft) %>%
    style_tt(j=2:4, align="c") %>%
    theme_tt("tabular", style="tabularray") %>%
    save_tt(filename, overwrite=T)
  
}

results_policy_ethic <- function(folder="Results", models="all", surveys="all", type="type", plot_scale=scale_fill_discrete) {
  df <- data_survey(models) %>%
      mutate(type=get(!!type))
  if (surveys != "all") { df <- df %>% filter(survey==!!surveys) }
  
  vs = c("current_policy_ethic", "new_policy_ethic")
  
  # For text values
  if (F) {
    # High score
    df %>% select(type, all_of(vs)) %>%
      mutate(across(vs, ~ as.numeric(.))) %>%
      mutate(current_high = ifelse(current_policy_ethic>=4, 1, 0),
             new_high = ifelse(new_policy_ethic>=4, 1, 0)) %>%
      group_by(type) %>%
      summarise(across(c("current_high", "new_high"), ~ mean(.)), .groups = "keep")
      
    # Percentage of subjects that view new policy as more (less) ethical
    df %>% select(type, all_of(vs))  %>%
      mutate(across(vs, ~ as.numeric(.))) %>%
      mutate(new_more_ethical = sign(new_policy_ethic-current_policy_ethic)) %>%
      group_by(type, new_more_ethical) %>%
      summarise(n = n()) %>%
      left_join(df %>% group_by(type) %>% summarise(N=n())) %>%
      mutate(perc = n/N)
    
    df %>% select(type, all_of(vs)) %>%
      mutate(across(vs, ~ as.numeric(.))) %>%
      group_by(type) %>%
      summarise(across(vs, list(mean=mean, sd=sd)))
  }
  
  for (p in vs) {
    dfsp <- df %>%
      rename(p = !!p) %>%
      select(c(type, weight, p))
    
    # Plot
    dfp <- dfsp %>%
      group_by(type, p) %>%
      summarise(n = sum(weight), .groups = "keep") %>%
      left_join(dfsp %>% group_by(type) %>% summarise(N=sum(weight), .groups="keep")) %>%
      mutate(type = as.character(type)) %>%
      mutate(v = n / N)
    
    # Always have humans in the first group
    types = unique(dfp$type)
    if ("Humans" %in% types) {
      dfp <- dfp %>%
        mutate(type = factor(type, c("Humans", sort(types[types!="Humans"], decreasing=T))))
    }
   
    pl <- ggplot(data=dfp, aes(x=p, y=v, fill=type)) +
      geom_bar(stat="identity",
               position=position_dodge2(preserve = "single"),
               width = .8) + 
      xlab("Ethical Score") + ylab(NULL) + labs(fill="") +
      ylim(0, 1) +
      plot_scale() +
      scale_x_discrete(drop=F) +
      theme_classic() +
      guides(fill=guide_legend(nrow=ceiling(length(unique(dfp$type))/4))) +
      theme(text=element_text(size=12,  family="serif"),
            panel.grid.major.y = element_line(),
            legend.position="bottom",
            plot.margin = margin(0,0,0,0))
    
    ns = c(p, surveys, type, models)
    filename = paste0(folder, "/plots/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".pdf")
    ggsave(filename, width=4, height=3)
  }
  
  # Table means
  dfst <- df %>%
    select(type, all_of(vs)) %>%
    mutate_at(all_of(vs), as.numeric) %>%
    rename(`Current Policy` = current_policy_ethic,
           `New Policy` = new_policy_ethic) %>%
    pivot_longer(!type, names_to = "policy") %>%
    pivot_wider(names_from = type, values_from = value, values_fn = list) %>%
    group_by(policy) %>%
    mutate(across(everything(), list(
      obs = ~ length(unlist(.x)),
      mean = ~ mean(unlist(.x)),
      se = ~ sd(unlist(.x)) / sqrt(length(unlist(.x)))
    )))
  # p-values
  types = as.character(unique(df$type))
  types_names = list(
    "Humans"="H", "LLMs"="L",
    "Online LLMs"="OL", "Local LLMs"="LL",
    "Small"="S", "Medium"="M", "Big"="B"
    )
  for (i in 1:(length(types)-1)) {
    for (j in (i+1):length(types)) {
      ti = types[i]
      tj = types[j]
      ti_name = types_names[ti]
      tj_name = types_names[tj]
      varname = paste0(ti_name,"-",tj_name,"_mean")
      dfst <- dfst %>%
        mutate(
          !! varname := max(t.test(unlist(get(ti)), unlist(get(tj)))$p.value, 1e-6)
        )
    }
  }
  
  dft <- dfst %>%
    ungroup(policy) %>%
    select(-types) %>%
    gather(var_name, value, -policy) %>%
    mutate(across('var_name', \(x) str_replace(x, '_se', ':se_mean'))) %>%
    separate(var_name, c("var", "stat"), sep="_") %>%
    pivot_wider(names_from = var, values_from = value) %>%
    arrange(policy) #%>%
  # Add between policies p-value
  type_pvalue = dft[1,]
  type_pvalue$policy= "p-value New-Current"
  type_pvalue$stat = "New-Current"
  for (t in types) {
    type_pvalue[[t]] = max(t.test(
      unlist(dfst %>% ungroup(policy) %>% filter(policy=="Current Policy") %>% select(types) %>% pull(t)),
      unlist(dfst %>% ungroup(policy) %>% filter(policy=="New Policy") %>% select(types) %>% pull(t))
    )$p.value, 1e-6)
  }
  # Observations
  robs = dft %>% filter(stat=="obs")
  robs = robs[1,]
  robs$policy = "Observations"
  
  ns = c("ethic", surveys, type, models)
  filename = paste0(folder, "/tables/stats_", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
  
  dft %>%
    filter(stat!="obs") %>%
    add_row(robs, .before=1) %>%
    add_row(type_pvalue) %>%
    mutate(policy = ifelse(stat=="se", "", policy)) %>%
    select(-stat) %>%
    rename(` `=policy) %>%
    tt() %>%
    format_tt(i=1, digits=0) %>%
    format_tt(i=c(2,3), j=2*(1:length(types)), digits=2, num_fmt="decimal", num_zero=T) %>%
    format_tt(i=c(2,3), j=2*(1:length(types))+1, sprintf="(%.2f)", num_fmt="decimal", num_zero=T) %>%
    format_tt(j=(2*length(types)+2):ncol(.), digits=3, num_fmt="decimal", num_zero=T) %>%
    format_tt(i=4, digits=3, num_fmt="decimal", num_zero=T) %>%
    style_tt(j=2*(1:length(types)), i=c(0,1,4), colspan=2, align="c") %>%
    style_tt(align="c") %>%
    group_tt(j=list("p-value"=(2*length(types)+2):ncol(.))) %>%
    #format_tt(i=c(2,4), j=2:(length(types)+1), digits=2, num_fmt="decimal", num_zero=T) %>%
    #format_tt(i=c(3,5), j=2:(length(types)+1), sprintf="(%.2f)", num_fmt="decimal", num_zero=T) %>%
    #format_tt(j=(length(types)+2):ncol(.), digits=3, num_fmt="decimal", num_zero=T) %>%
    #format_tt(i=6, digits=3, num_fmt="decimal", num_zero=T) %>%
    #style_tt(align="c") %>%
    #group_tt(j=list("p-value"=(length(types)+2):ncol(.))) %>%
    theme_tt("tabular", style="tabularray") %>%
    save_tt(filename, overwrite=T)

  if (F) {
    # Summary table
    dft <- dfsp %>%
      mutate(p = as.numeric(p)) %>%
      # TODO: Compute weighted p-values
      select(-weight) %>%
      pivot_wider(names_from=type, values_from=p, values_fn=list) %>%
      mutate(across(everything(), list(
        obs = ~ length(unlist(.x)),
        mean = ~ mean(unlist(.x)),
        se = ~ sd(unlist(.x)) / sqrt(length(unlist(.x)))
      )))
    # p-values
    types = as.character(unique(dfsp$type))
    types_names = list(
      "Humans"="H", "LLMs"="L",
      "Online LLMs"="OL", "Local LLMs"="LL",
      "Small"="S", "Medium"="M", "Big"="B"
      )
    for (i in 1:(length(types)-1)) {
      for (j in (i+1):length(types)) {
        ti = types[i]
        tj = types[j]
        ti_name = types_names[ti]
        tj_name = types_names[tj]
        varname = paste0(ti_name,"-",tj_name,"_mean")
        dft <- dft %>%
          mutate(
            !! varname := max(t.test(unlist(get(ti)), unlist(get(tj)))$p.value, 1e-6)
          )
      }
    }
    
    ns = c(p, surveys, type, models)
    filename = paste0(folder, "/tables/stats_", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
    
    dft %>%
      select(-types) %>%
      gather(var_name, value) %>%
      separate(var_name, c("var", "stat"), sep="_") %>%
      pivot_wider(names_from = var, values_from = value) %>%
      mutate(across('stat', \(x) str_replace(x, 'obs', 'Obs'))) %>%
      mutate(across('stat', \(x) str_replace(x, 'mean', 'Mean'))) %>%
      mutate(across('stat', \(x) str_replace(x, 'se', ''))) %>%
      rename(` `=stat) %>%
      tt() %>%
      format_tt(i=1, digits=1, num_fmt="decimal") %>%
      format_tt(i=2, j=2:(length(types)+1), digits=2, num_fmt="decimal", num_zero=T) %>%
      format_tt(j=(length(types)+2):ncol(.), digits=3, num_fmt="decimal", num_zero=T) %>%
      format_tt(i=3, j=2:(length(types)+1), sprintf="(%.2f)", num_fmt="decimal", num_zero=T) %>%
      style_tt(j=2:4, align="c") %>%
      group_tt(j=list("p-value"=(length(types)+2):ncol(.))) %>%
      theme_tt("tabular", style="tabularray") %>%
      save_tt(filename, overwrite=T)
  }
}

results_policy_characteristics <- function(folder="Results", models="all", surveys="all", type="type", plot_scale=scale_fill_discrete) {
  df <- data_survey(models)
  
  dfsp <- df %>%
    mutate(type=get(!!type))
  if (surveys != "all") { dfsp <- dfsp %>% filter(survey==!!surveys) }
  
  dfs <- dfsp %>%
    select(c(type,
             current_policy_lying, new_policy_lying,
             current_policy_deceptive, new_policy_deceptive,
             current_policy_dishonest, new_policy_dishonest,
             current_policy_untruthful, new_policy_untruthful,
             weight
             )) %>%
    pivot_longer(!c(type, weight), names_to = "question", values_to = "value") %>%
    mutate(across('question', \(x) str_replace(x, 'policy_', 'policy:'))) %>%
    separate(question, c("policy", "characteristic"), sep=":") %>%
    mutate(across('characteristic', \(x) str_replace(x, 'deceptive', 'Deceptive'))) %>%
    mutate(across('characteristic', \(x) str_replace(x, 'dishonest', 'Dishonest'))) %>%
    mutate(across('characteristic', \(x) str_replace(x, 'lying', 'Lying'))) %>%
    mutate(across('characteristic', \(x) str_replace(x, 'untruthful', 'Untruthful')))
  
  # For text values
  if (F) {
    # Difference across all scores
    dfs %>%
      select(-weight) %>%
      group_by(type, policy) %>%
      summarise(
        mean = mean(value), std = sd(value), .groups="keep") %>%
      pivot_wider(names_from = type, values_from = c(mean, std)) %>%
      mutate(`Human-LLMS` = mean_Humans - mean_LLMs)
    
  }
    
      
  # Per policy
  for (p in c("current_policy", "new_policy")) {
    # Plot
    dfp <- dfs %>%
      filter(policy==!!p) %>%
      select(-policy) %>%
      group_by(type, characteristic) %>%
      #summarise(mean=mean(value), se = sd(value) / sqrt(n())) %>%
      mutate(type = as.character(type)) %>%
      summarise(
        mean=weighted.mean(value, weight),
        se = wtd.stderror(value, weight),
        .groups="keep") 
    
    # Always have humans in the first group
    types = unique(dfp$type)
    if ("Humans" %in% types) {
      dfp <- dfp %>%
        mutate(type = factor(type, c("Humans", sort(types[types!="Humans"], decreasing=T))))
    }
    
    pl <- ggplot(data=dfp, aes(x=characteristic, y=mean, fill=type)) +
      geom_bar(stat="identity",
               #position=position_dodge2(preserve = "single"),
               position=position_dodge(.8),
               width = .6) + 
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                    #position=position_dodge2(preserve = "single"),
                    position=position_dodge(.8),
                    width=0.2) +
      xlab("Characteristic") + ylab(NULL) + labs(fill="") +
      ylim(0, 5) +
      plot_scale() +
      theme_classic() +
      guides(fill=guide_legend(nrow=ceiling(length(unique(dfp$type))/4))) +
      theme(text=element_text(size=12,  family="serif"),
            panel.grid.major.y = element_line(),
            legend.position="bottom",
            plot.margin = margin(0,0,0,0))
    
    ns = c(p, "characteristics", surveys, type, models)
    filename = paste0(folder, "/plots/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".pdf")
    ggsave(filename, width=4, height=3)
    
    
    # Summary table
    dft <- dfs %>%
      filter(policy==!!p) %>%
      select(-policy) %>%
      # TODO: Compute weighted p-values
      select(-weight) %>%
      pivot_wider(names_from=type, values_from=value, values_fn=list) %>%
      group_by(characteristic) %>%
      mutate(across(everything(), list(
        obs = ~ length(unlist(.x)),
        mean = ~ mean(unlist(.x)),
        se = ~ sd(unlist(.x)) / sqrt(length(unlist(.x)))
      )))
    # p-values
    types = as.character(unique(dfsp$type))
    types_names = list(
      "Humans"="H", "LLMs"="L",
      "Online LLMs"="OL", "Local LLMs"="LL",
      "Small"="S", "Medium"="M", "Big"="B"
      )
    for (i in 1:(length(types)-1)) {
      for (j in (i+1):length(types)) {
        ti = types[i]
        tj = types[j]
        ti_name = types_names[ti]
        tj_name = types_names[tj]
        varname = paste0(ti_name,"-",tj_name,"_mean")
        dft <- dft %>%
          mutate(
            !! varname := max(t.test(unlist(get(ti)), unlist(get(tj)))$p.value, 1e-6)
          )
      }
    }
    
    ns = c(p, "characteristics", surveys, type, models)
    filename = paste0(folder, "/tables/stats_", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
    
    dft <- dft %>%
      ungroup(characteristic) %>%
      select(-types) %>%
      gather(var_name, value, -characteristic) %>%
      separate(var_name, c("var", "stat"), sep="_") %>%
      pivot_wider(names_from = var, values_from = value) %>%
      arrange(characteristic) %>%
      mutate(characteristic=ifelse(stat=="mean", characteristic, ""))
    obs = dft[1,] %>% mutate(characteristic="Obs")
    rbind(obs, dft %>% filter(stat != "obs")) %>%
      select(-stat) %>%
      rename(` `=characteristic) %>%
      tt() %>%
      format_tt(i=1, digits=1, num_fmt="decimal") %>%
      format_tt(i=c(2,4,6,8), j=2:(length(types)+1), digits=2, num_fmt="decimal", num_zero=T) %>%
      format_tt(j=(length(types)+2):ncol(.), digits=3, num_fmt="decimal", num_zero=T) %>%
      format_tt(i=c(3,5,7,9), j=2:(length(types)+1), sprintf="(%.2f)", num_fmt="decimal", num_zero=T) %>%
      style_tt(j=2:4, align="c") %>%
      group_tt(j=list("p-value"=(length(types)+2):ncol(.))) %>%
      theme_tt("tabular", style="tabularray") %>%
      save_tt(filename, overwrite=T)
    
  }
}

results_policy_change <- function(folder="Results", models="all", surveys="all", type="type", plot_scale=scale_fill_discrete) {
  df <- data_survey(models) %>%
    mutate(recommend_new_policy_numeric=recode(recommend_new_policy, "Yes"=1, "No opinion"=0, "No"=-1)) %>%
    mutate(board_vote_numeric=recode(board_vote, "In Favor"=1, "Follow Consensus"=0, "Abstain"=0, "Against"=-1)) %>%
    mutate(type=get(!!type))
  if (surveys != "all") { df <- df %>% filter(survey==!!surveys) }
  
  vs = c("recommend_new_policy", "board_vote")
  
  # For text values
  if (F) {
    # Original Recommendation
    dft <- df %>%
      select(type, recommend_new_policy) %>%
      mutate(recommend_new_policy = as.character(recommend_new_policy))
    # Proportions
    dft %>% 
      group_by(type, recommend_new_policy) %>%
      summarise(n=n()) %>%
      left_join(dft %>% group_by(type) %>% summarise(N=n())) %>%
      mutate(perc = n / N)
    
    # Proportions without no opinion
    dft <- dft %>%
      filter(recommend_new_policy != "No opinion")
    dft %>%
      mutate(recommend_new_policy = as.character(recommend_new_policy)) %>%
      group_by(type, recommend_new_policy) %>%
      summarise(n=n()) %>%
      left_join(dft %>% group_by(type) %>% summarise(N=n())) %>%
      mutate(perc = n / N)
    
    # Revised Recommendation
    dft <- df %>%
      select(type, board_vote) %>%
      mutate(board_vote = as.character(board_vote))
    # Proportions
    dft %>% 
      group_by(type, board_vote) %>%
      summarise(n=n()) %>%
      left_join(dft %>% group_by(type) %>% summarise(N=n())) %>%
      mutate(perc = n / N)
    
    # p-value Humans Revised average score
    t.test(df %>%
      filter(type=="Humans") %>%
      pull(board_vote_numeric))
      
  }
  
  for (p in vs) {
    
    dfsp <- df %>%
      rename(p = !!p) %>%
      rename(p_numeric = paste0(!!p, "_numeric")) %>%
      select(c(type, weight, p, p_numeric))
        
    # Plot
    dfp <- dfsp %>%
      select(type, weight, p) %>%
      group_by(type, p) %>%
      summarise(n = sum(weight), .groups = "keep") %>%
      left_join(dfsp %>% group_by(type) %>% summarise(N=sum(weight), .groups="keep")) %>%
      mutate(v = n / N) %>%
      mutate(type = as.character(type)) %>%
      arrange(p)
    
    # Always have humans in the first group
    types = unique(dfp$type)
    if ("Humans" %in% types) {
      dfp <- dfp %>%
        mutate(type = factor(type, c("Humans", sort(types[types!="Humans"], decreasing=T))))
    }
   
    pl <- ggplot(data=dfp, aes(x=p, y=v, fill=type)) +
      geom_bar(stat="identity",
               position=position_dodge2(preserve = "single"),
               width = .8) + 
      xlab(NULL) + ylab(NULL) + labs(fill="") +
      ylim(0, 1) +
      plot_scale() +
      scale_x_discrete(drop=F) +
      theme_classic() +
      guides(fill=guide_legend(nrow=ceiling(length(unique(dfp$type))/4))) +
      theme(text=element_text(size=12,  family="serif"),
            panel.grid.major.y = element_line(),
            legend.position="bottom",
            plot.margin = margin(0,0,0,0))
    
    ns = c(p, surveys, type, models)
    filename = paste0(folder, "/plots/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".pdf")
    ggsave(filename, width=4, height=3)
    
    
    if (F) {
      # Summary table
      dft <- dfsp %>%
        mutate(p = p_numeric) %>%
        # TODO: Compute weighted p-values
        select(-weight, -p_numeric) %>%
        pivot_wider(names_from=type, values_from=p, values_fn=list) %>%
        mutate(across(everything(), list(
          obs = ~ length(unlist(.x)),
          mean = ~ mean(unlist(.x)),
          se = ~ sd(unlist(.x)) / sqrt(length(unlist(.x)))
        )))
      # p-values
      types = as.character(unique(dfsp$type))
      types_names = list(
        "Humans"="H", "LLMs"="L",
        "Online LLMs"="OL", "Local LLMs"="LL",
        "Small"="S", "Medium"="M", "Big"="B"
        )
      for (i in 1:(length(types)-1)) {
        for (j in (i+1):length(types)) {
          ti = types[i]
          tj = types[j]
          ti_name = types_names[ti]
          tj_name = types_names[tj]
          if (is.null(ti_name)) {ti_name = ti}
          if (is.null(tj_name)) {tj_name = tj}
          tj_name = types_names[tj]
          varname = paste0(ti_name,"-",tj_name,"_mean")
          dft <- dft %>%
            mutate(
              !! varname := max(t.test(unlist(get(ti)), unlist(get(tj)))$p.value, 1e-6)
            )
        }
      }
      
      ns = c(p, surveys, type, models)
      filename = paste0(folder, "/tables/stats_", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
      
      dft %>%
        select(-types) %>%
        gather(var_name, value) %>%
        separate(var_name, c("var", "stat"), sep="_") %>%
        pivot_wider(names_from = var, values_from = value) %>%
        mutate(across('stat', \(x) str_replace(x, 'obs', 'Obs'))) %>%
        mutate(across('stat', \(x) str_replace(x, 'mean', 'Mean'))) %>%
        mutate(across('stat', \(x) str_replace(x, 'se', ''))) %>%
        rename(` `=stat) %>%
        tt() %>%
        format_tt(i=1, digits=1, num_fmt="decimal") %>%
        format_tt(i=2, j=2:(length(types)+1), digits=2, num_fmt="decimal", num_zero=T) %>%
        format_tt(j=(length(types)+2):ncol(.), digits=3, num_fmt="decimal", num_zero=T) %>%
        format_tt(i=3, j=2:(length(types)+1), sprintf="(%.2f)", num_fmt="decimal", num_zero=T) %>%
        style_tt(j=2:4, align="c") %>%
        group_tt(j=list("p-value"=(length(types)+2):ncol(.))) %>%
        theme_tt("tabular", style="tabularray") %>%
        save_tt(filename, overwrite=T)
    }
      
    
    # Over vs Under Reporting
    if (type != "model") {
      ns = c(p, surveys, type, "overunder", models)
      filename = paste0(folder, "/tables/stats_", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
      
      dfsp <- df %>%
        rename(p = !!p) %>%
        rename(p_numeric = paste0(!!p, "_numeric")) %>%
        mutate(type=get(!!type)) %>%
        select(c(survey, type, weight, p_numeric)) %>%
        rename(p=p_numeric) %>%
        # TODO: Compute weighted p-values
        select(-weight) %>%
        pivot_wider(names_from=type, values_from=p, values_fn=list) %>%
        group_by(survey) %>%
        mutate(across(everything(), list(
          obs = ~ length(unlist(.x)),
          mean = ~ mean(unlist(.x)),
          se = ~ sd(unlist(.x)) / sqrt(length(unlist(.x)))
        )))
      # p-values
      types = as.character(unique(df %>% mutate(type=get(!!type)) %>% pull(type)))
      types_names = list(
        "Humans"="H", "LLMs"="L",
        "Online LLMs"="OL", "Local LLMs"="LL",
        "Small"="S", "Medium"="M", "Big"="B"
        )
      for (i in 1:(length(types)-1)) {
        for (j in (i+1):length(types)) {
          ti = types[i]
          tj = types[j]
          ti_name = types_names[ti]
          tj_name = types_names[tj]
          varname = paste0(ti_name,"-",tj_name,"_mean")
          dfsp <- dfsp %>%
            mutate(
              !! varname := max(t.test(unlist(get(ti)), unlist(get(tj)))$p.value, 1e-6)
            )
        }
      }
      
      dft <- dfsp %>%
        ungroup(survey) %>%
        select(-types) %>%
        gather(var_name, value, -survey) %>%
        separate(var_name, c("var", "stat"), sep="_") %>%
        pivot_wider(names_from = var, values_from = value) %>%
        arrange(survey) #%>%
        # Add between surveys p-value
      type_pvalue = dft[1,]
      type_pvalue$survey = "p-value"
      type_pvalue$stat = "Under-Over"
      for (t in types) {
        type_pvalue[[t]] = t.test(
          unlist(dfsp %>% ungroup(survey) %>% filter(survey=="overreporting") %>% select(types) %>% pull(t)),
          unlist(dfsp %>% ungroup(survey) %>% filter(survey=="underreporting") %>% select(types) %>% pull(t))
        )$p.value
      }
      
      dft %>%
        add_row(type_pvalue) %>%
        mutate(across('stat', \(x) str_replace(x, 'obs', 'Obs'))) %>%
        mutate(across('stat', \(x) str_replace(x, 'mean', 'Mean'))) %>%
        mutate(across('stat', \(x) str_replace(x, 'se', ''))) %>%
        mutate(across('survey', \(x) str_replace(x, 'overreporting', 'Over-Reporting'))) %>%
        mutate(across('survey', \(x) str_replace(x, 'underreporting', 'Under-Reporting'))) %>%
        rename(` `=survey, `  `=stat) %>%
        tt() %>%
        format_tt(i=c(1,4), digits=1, num_fmt="decimal") %>%
        format_tt(i=c(2,5), j=3:(length(types)+2), digits=2, num_fmt="decimal", num_zero=T) %>%
        format_tt(j=(length(types)+3):ncol(.), digits=3, num_fmt="decimal", num_zero=T) %>%
        format_tt(i=c(3,6), j=3:(length(types)+2), sprintf="(%.2f)", num_fmt="decimal", num_zero=T) %>%
        format_tt(i=7, digits=3, num_fmt="decimal", num_zero=T) %>%
        style_tt(align="c") %>%
        style_tt(j=1, i=c(1,4), rowspan = 3) %>%
        group_tt(j=list("p-value"=(length(types)+3):ncol(.))) %>%
        theme_tt("tabular", style="tabularray") %>%
        save_tt(filename, overwrite=T)
    }
        
  }
  
  # Summary Table
  vs = c("recommend_new_policy_numeric", "board_vote_numeric")
  dfst <- df %>%
    select(type, all_of(vs)) %>%
    rename(`Original Recommendation` = recommend_new_policy_numeric,
           `Revised Recommendation` = board_vote_numeric) %>%
    pivot_longer(!type, names_to = "recommendation") %>%
    pivot_wider(names_from = type, values_from = value, values_fn = list) %>%
    group_by(recommendation) %>%
    mutate(across(everything(), list(
      obs = ~ length(unlist(.x)),
      mean = ~ mean(unlist(.x)),
      se = ~ sd(unlist(.x)) / sqrt(length(unlist(.x)))
    )))
  # p-values
  types = as.character(unique(df$type))
  types_names = list(
    "Humans"="H", "LLMs"="L",
    "Online LLMs"="OL", "Local LLMs"="LL",
    "Small"="S", "Medium"="M", "Big"="B"
    )
  for (i in 1:(length(types)-1)) {
    for (j in (i+1):length(types)) {
      ti = types[i]
      tj = types[j]
      ti_name = types_names[ti]
      tj_name = types_names[tj]
      varname = paste0(ti_name,"-",tj_name,"_mean")
      dfst <- dfst %>%
        mutate(
          !! varname := max(t.test(unlist(get(ti)), unlist(get(tj)))$p.value, 1e-6)
        )
    }
  }
  
  dft <- dfst %>%
    ungroup(recommendation) %>%
    select(-types) %>%
    gather(var_name, value, -recommendation) %>%
    mutate(across('var_name', \(x) str_replace(x, '_se', ':se_mean'))) %>%
    separate(var_name, c("var", "stat"), sep="_") %>%
    pivot_wider(names_from = var, values_from = value) %>%
    arrange(recommendation) #%>%
  # Add between policies p-value
  type_pvalue = dft[1,]
  type_pvalue$recommendation= "p-value Revised-Original"
  type_pvalue$stat = "New-Current"
  for (t in types) {
    type_pvalue[[t]] = max(t.test(
      unlist(dfst %>% ungroup(recommendation) %>% filter(recommendation=="Original Recommendation") %>% select(types) %>% pull(t)),
      unlist(dfst %>% ungroup(recommendation) %>% filter(recommendation=="Revised Recommendation") %>% select(types) %>% pull(t))
    )$p.value, 1e-6)
  }
  # Observations
  robs = dft %>% filter(stat=="obs")
  robs = robs[1,]
  robs$recommendation = "Observations"
  
  ns = c("recommendation", surveys, type, models)
  filename = paste0(folder, "/tables/stats_", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
  
  dft %>%
    filter(stat!="obs") %>%
    add_row(robs, .before=1) %>%
    add_row(type_pvalue) %>%
    mutate(recommendation = ifelse(stat=="se", "", recommendation)) %>%
    select(-stat) %>%
    rename(` `=recommendation) %>%
    tt() %>%
    format_tt(i=1, digits=0) %>%
    format_tt(i=c(2,3), j=2*(1:length(types)), digits=2, num_fmt="decimal", num_zero=T) %>%
    format_tt(i=c(2,3), j=2*(1:length(types))+1, sprintf="(%.2f)", num_fmt="decimal", num_zero=T) %>%
    format_tt(j=(2*length(types)+2):ncol(.), digits=3, num_fmt="decimal", num_zero=T) %>%
    format_tt(i=4, digits=3, num_fmt="decimal", num_zero=T) %>%
    style_tt(j=2*(1:length(types)), i=c(0,1,4), colspan=2, align="c") %>%
    style_tt(align="c") %>%
    group_tt(j=list("p-value"=(2*length(types)+2):ncol(.))) %>%
    theme_tt("tabular", style="tabularray") %>%
    save_tt(filename, overwrite=T)
  
  
}

regression_policy_change <- function(folder="Results", models="all", surveys="all", type="type", plot_scale=scale_fill_discrete) {
  df <- data_survey(models)
  
  dfsp <- df %>%
    mutate(type=get(!!type))
  if (surveys != "all") { dfsp <- dfsp %>% filter(survey==!!surveys) }
  
  dfr <- dfsp %>%
    mutate(
      current_policy_ethic = as.numeric(current_policy_ethic),
      new_policy_ethic = as.numeric(new_policy_ethic),
      Ethical = new_policy_ethic - current_policy_ethic,
      Lying = new_policy_lying - current_policy_lying,
      Deceptive = new_policy_deceptive - current_policy_deceptive,
      Dishonest = new_policy_dishonest - current_policy_dishonest,
      Untruthful = new_policy_untruthful - current_policy_untruthful,
    ) %>%
    mutate(recommend_new_policy=recode(recommend_new_policy, "Yes"=1, "No opinion"=0, "No"=-1)) %>%
    mutate(board_vote=recode(board_vote, "In Favor"=1, "Follow Consensus"=0, "Abstain"=0, "Against"=-1)) #%>%
  
  
  # Define groups
  type1 <- "Humans"
  type2 <- "LLMs"
  if (type=="access") {
    type1 = "Online LLMs"
    type2 = "Local LLMs"
  }
  
  # For text value
  if (F) {
    # Standard deviation of keyword ratings (Lying, Deceptive, Dishonest, Untruthful)
    dfr %>%
      group_by(type) %>%
      summarise(across(c("Ethical", "Lying", "Deceptive", "Dishonest", "Untruthful"), sd))
      
    
  }
  
  ######################
  # Correlation Matrix #
  ######################
  for (t in c(type1, type2)) {
    dfco <- dfr %>%
      filter(type==!!t) %>%
      select(recommend_new_policy, board_vote,
             Ethical, Lying, Deceptive, Dishonest, Untruthful) %>%
      rename(`Original Rec.` = recommend_new_policy,
             `Revised Rec.` = board_vote)
    
    
    fun <- function(x) {
      m <- correlation(x, method="pearson", redundant=T)
      m$r <- round(m$r, digits=3)
      m <- m %>%
        summary() %>%
        format(3) %>%
        as.matrix()
      m <- m[, names(x)]
      ms <- correlation(x, method="spearman", redundant=T)
      ms$rho <- round(ms$rho, digits=3)
      ms <- ms %>%
        summary() %>%
        format(3) %>%
        as.matrix()
      ms <- ms[, names(x)] %>% t()
      m[lower.tri(m)] <- ms[lower.tri(ms)]
      diag(m) <- NA
      
      row.names(m) <- names(x)
      return(m)
    }
    
    ns = c("correlation", surveys, type, models, t)
    filename = paste0(folder, "/tables/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
    
    datasummary_correlation(dfco, method = fun) %>%
      theme_tt("tabular", style="tabularray") %>%
      save_tt(filename, overwrite=T)
  }
  
  for (noethic in c("ethic", "noethic")) {
    ###################
    # Characteristics #
    ###################
    f_rec = formula(recommend_new_policy ~ Ethical + Lying + Deceptive + Dishonest + Untruthful)
    f_rev = formula(board_vote ~ Ethical + Lying + Deceptive + Dishonest + Untruthful)
    if (noethic=="noethic") {
      f_rec = formula(recommend_new_policy ~ Lying + Deceptive + Dishonest + Untruthful)
      f_rev = formula(board_vote ~ Lying + Deceptive + Dishonest + Untruthful)
    }
    if (noethic=="ethic") { noethic <- NULL }
    
    m = list()
    m <- append(m, list(lm(f_rec, data=dfr %>% filter(type==type1))))
    m <- append(m, list(lm(f_rec, data=dfr %>% filter(type==type2))))
    m <- append(m, list(lm(f_rev, data=dfr %>% filter(type==type1))))
    m <- append(m, list(lm(f_rev, data=dfr %>% filter(type==type2))))
    names(m) <- c(type1, type2, type1, type2)
    
    ns = c("regression", surveys, type, models, noethic)
    filename = paste0(folder, "/tables/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
    
    gm <- tribble(
      ~raw, ~clean, ~fmt,
      "nobs", "N", 0,
      "r.squared", "$R^2$", 2)
    
    modelsummary(
      m,
      stars = c("*"=.1, "**"=.05, "***"=.01),
      estimate="{estimate}{stars}",
      gof_map = gm,
      coef_rename = c("(Intercept)" = "Constant")
    ) %>%
      group_tt(j = list(
        `Original Recommendation` = 2:3,
        `Revised Recommendation` = 4:5)) %>%
      theme_tt("tabular", style="tabularray") %>%
      save_tt(filename, overwrite=T)
    
    #############
    # t-test R2 #
    #############
    # For text values
    if (F) {
      dfrt1 <- dfr %>% filter(type==type1)
      dfrt2 <- dfr %>% filter(type==type2)
      
      t1 <- list()
      t2 <- list()
      
      t1$rs_rec <- c()
      t1$rs_rev <- c()
      t2$rs_rec <- c()
      t2$rs_rev <- c()
      
      for (i in 0:500) {
        dfrt1s <- dfrt1 %>% sample_n(nrow(dfrt1), replace = T)
        dfrt2s <- dfrt2 %>% sample_n(nrow(dfrt2), replace = T)
        t1$rs_rec <- c(t1$rs_rec, summary(lm(f_rec, data=dfrt1s))$r.square)
        t1$rs_rev <- c(t1$rs_rev, summary(lm(f_rev, data=dfrt1s))$r.square)
        t2$rs_rec <- c(t2$rs_rec, summary(lm(f_rec, data=dfrt2s))$r.square)
        t2$rs_rev <- c(t2$rs_rev, summary(lm(f_rev, data=dfrt2s))$r.square)
      }
      
      t.test(t1$rs_rec, t2$rs_rec)
      t.test(t1$rs_rev, t2$rs_rev)
    }
    
    ##################################
    # Welch's t-test Characteristics #
    ##################################
    welch_test <- function(m1, m2) {
      c1 <- coef(summary(m1))[,"Estimate"]
      s1 <- coef(summary(m1))[,"Std. Error"]
      n1 <- nrow(m1$model)
      df1 <- m1$df.residual
      c2 <- coef(summary(m2))[,"Estimate"]
      s2 <- coef(summary(m2))[,"Std. Error"]
      n2 <- nrow(m2$model)
      df2 <- m2$df.residual
      
      t <- (c1-c2) / sqrt(s1^2 + s2^2)
      df <- (s1^2 / n1 + s2^2 / n2)^2 / (s1^4 / (n1^2*df1) + s2^4 / (n2^2*df2))
      p_value <- pt(-abs(t), df)
      
      return(list(t=t, p_value=p_value))
    }
    # type2-type1 for original recommendation
    p_rec_21 <- welch_test(m[[2]], m[[1]])
    # type2-type1 for revised recommendation
    p_rev_21 <- welch_test(m[[4]], m[[3]])
    # revised-original recommendation for type 1
    p_1_revrec <- welch_test(m[[3]], m[[1]])
    # revised-original recommendation for type 2
    p_2_revrec <- welch_test(m[[4]], m[[2]])
    
    # Table
    g_tt <- list()
    g_tt[[paste(type2, "-", type1)]] = 2:3
    g_tt[["Revised - Original Rec."]] = 4:5
    
    dat <- list()
    dat[[" "]] = replace(names(p_rec_21$p_value), 1, "Constant")
    dat[["Orig. Rec."]] = p_rec_21$p_value
    dat[["Rev. Rec"]] = p_rev_21$p_value
    dat[[type1]] = p_1_revrec$p_value
    dat[[type2]] = p_2_revrec$p_value
    
    ns = c("regression", surveys, type, models, "p_values_diff", noethic)
    filename = paste0(folder, "/tables/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
    
    as_tibble(dat) %>%
      tt() %>%
      format_tt(digits=3, num_fmt = "decimal", num_zero = TRUE) %>%
      group_tt(j=g_tt) %>%
      theme_tt("tabular", style="tabularray") %>%
      save_tt(filename, overwrite=T)
  }
  
  ################
  # Demographics #
  ################
  if (models=="pilot") {
    #####################
    # Pure demographics #
    #####################
    dfrd <- dfr %>%
      filter(type=="Humans") %>%
      rename(Male = male, `Under-Represented`=under_represented,
             `Business or Econ`=business_econ, Age=age,
             `Right-Leaning`=political_affiliation, GPA=gpa,
             Misreporting=misreporting, Cheating=cheating, Trust=trust) %>%
      # Create missing indicators
      mutate(
        `Missing GPA` = ifelse(is.na(GPA), 1, 0),
        `Missing Misreporting` = ifelse(is.na(Misreporting), 1, 0),
        `Missing Trust` = ifelse(is.na(Trust), 1, 0),
      ) %>%
      mutate(
        GPA = ifelse(is.na(GPA), 0, GPA),
        Misreporting = ifelse(is.na(Misreporting), 0, Misreporting),
        Trust = ifelse(is.na(Trust), 0, Trust),
      )
    
    f_rec = formula(recommend_new_policy ~ Male + `Under-Represented` +
                      `Business or Econ` + Age + `Right-Leaning` + GPA + `Missing GPA`)
    f_rev = formula(board_vote ~ Male + `Under-Represented` +
                      `Business or Econ` + Age + `Right-Leaning` + GPA + `Missing GPA`)
    
    m = list()
    m <- append(m, list(lm(f_rec, data=dfrd)))
    m <- append(m, list(lm(f_rev, data=dfrd)))
    names(m) <- c("Orig. Rec.", "Rev. Rec.")
    
    ns = c("regression", surveys, type, models, "demographics")
    filename = paste0(folder, "/tables/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
    
    gm <- tribble(
      ~raw, ~clean, ~fmt,
      "nobs", "N", 0,
      "r.squared", "$R^2$", 2)
    
    modelsummary(
      m,
      stars = c("*"=.1, "**"=.05, "***"=.01),
      estimate="{estimate}{stars}",
      gof_map = gm,
      coef_rename = c("(Intercept)" = "Constant")
    ) %>%
      theme_tt("tabular", style="tabularray") %>%
      save_tt(filename, overwrite=T)
    
    ############################
    # Revealed Characteristics #
    ############################
    # Cheating
    dfp <- dfrd %>%
      select(recommend_new_policy, board_vote, Cheating) %>%
      mutate(Cheating = factor(Cheating, c(0,1), c("Not Cheating", "Cheating"))) %>%
      rename(`Original`=recommend_new_policy, `Revised`=board_vote) %>%
      pivot_longer(-Cheating, names_to = "Recommendation") %>%
      group_by(Cheating, Recommendation) %>%
      summarise(v = mean(value),
                se = sd(value) / sqrt(n()),
                n = n(),
                .groups = "keep")
   
    pl <- ggplot(data=dfp, aes(x=Cheating, y=v, fill=Recommendation)) +
      geom_bar(stat="identity",
               position=position_dodge2(preserve = "single"),
               width = .8) + 
      geom_errorbar(aes(ymin=v-se, ymax=v+se),
                    #position=position_dodge2(preserve = "single"),
                    position=position_dodge(.8),
                    width=0.2) +
      xlab(NULL) + ylab(NULL) + labs(fill="") +
      #ylim(0, 1) +
      plot_scale() +
      scale_x_discrete(drop=F) +
      theme_classic() +
      theme(text=element_text(size=12,  family="serif"),
            panel.grid.major.y = element_line(),
            legend.position="bottom",
            plot.margin = margin(0,0,0,0))
    
    ns = c("cheating", surveys, type, models)
    filename = paste0(folder, "/plots/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".pdf")
    ggsave(filename, width=4, height=3)
    
    # Misreporting
    dfp <- dfrd %>%
      select(recommend_new_policy, board_vote, Misreporting) %>%
      filter(Misreporting >= 0) %>%
      mutate(Misreporting = ifelse(Misreporting==0, 0, 1)) %>%
      mutate(Misreporting = factor(Misreporting, c(0,1), c("Not Misreporting", "Misreporting"))) %>%
      rename(`Original`=recommend_new_policy, `Revised`=board_vote) %>%
      pivot_longer(-Misreporting, names_to = "Recommendation") %>%
      group_by(Misreporting, Recommendation) %>%
      summarise(v = mean(value),
                se = sd(value) / sqrt(n()),
                n = n(),
                .groups = "keep")
   
    pl <- ggplot(data=dfp, aes(x=Misreporting, y=v, fill=Recommendation)) +
      geom_bar(stat="identity",
               position=position_dodge2(preserve = "single"),
               width = .8) + 
      geom_errorbar(aes(ymin=v-se, ymax=v+se),
                    #position=position_dodge2(preserve = "single"),
                    position=position_dodge(.8),
                    width=0.2) +
      xlab(NULL) + ylab(NULL) + labs(fill="") +
      #ylim(0, 1) +
      plot_scale() +
      scale_x_discrete(drop=F) +
      theme_classic() +
      theme(text=element_text(size=12,  family="serif"),
            panel.grid.major.y = element_line(),
            legend.position="bottom",
            plot.margin = margin(0,0,0,0))
    
    ns = c("misreporting", surveys, type, models)
    filename = paste0(folder, "/plots/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".pdf")
    ggsave(filename, width=4, height=3)
    
  }
  
  
    
  
  if (F) {
    # Interaction (to get the difference)
    f_rec_int = formula(recommend_new_policy ~ I(type==type2)*(Ethical + Lying + Deceptive + Dishonest + Untruthful))
    f_rev_int = formula(board_vote ~ I(type==type2)*(Ethical + Lying + Deceptive + Dishonest + Untruthful))
    
    f_rev_rec_int = formula(board_vote ~ recommend_new_policy * (Ethical + Lying + Deceptive + Dishonest + Untruthful))
    
    m <- list()
    m = append(m, list(lm(f_rec_int, data=dfr)))
    m = append(m, list(lm(f_rev_int, data=dfr)))
    m = append(m, list(lm(f_rev_rec_int, data=dfr %>% filter(type==type1))))
    m = append(m, list(lm(f_rev_rec_int, data=dfr %>% filter(type==type2))))
    
    
    names(m) <- c("Original", "Revised", type1, type2)
    
    
    ns = c("regression", surveys, type, models, "inter")
    filename = paste0(folder, "/tables/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
    
    cm <- c('I(type == type2)TRUE' = 'LLMs',
            'I(type == type2)TRUE:Ethical' = 'LLMs X Ethical',
            'I(type == type2)TRUE:Lying' = 'LLMs X Lying',
            'I(type == type2)TRUE:Deceptive' = 'LLMs X Deceptive',
            'I(type == type2)TRUE:Dishonest' = 'LLMs X Dishonest',
            'I(type == type2)TRUE:Untruthful' = 'LLMs X Untruthful'
    )
    
    modelsummary(
      m,
      stars = c("*"=.1, "**"=.05, "***"=.01),
      estimate="{estimate}{stars}",
      statistic="statistic",
      gof_map = gm,
      #coef_map = cm
    ) %>%
      group_tt(j = list(
        `LLMs - Humans` = 2:3
      )) %>%
      group_tt(i=list("LLMs X ..."=3))
      theme_tt("tabular", style="tabularray") %>%
      save_tt(filename, overwrite=T)
  }
    
  
  
  #ns = c("regression", surveys, type, models)
  #filename = paste0(folder, "/tables/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".tex")
  #stargazer(list(mh_rec, ml_rec, mh_rev, ml_rev),
  #          dep.var.caption = "",
  #          dep.var.labels = c("Original Recommendation", "Revised Recommendation"),
  #          column.labels = c("Humans", type_filter, "Humans", type_filter),
  #          keep.stat = c("n","rsq", "adj.rsq"),
  #          omit.table.layout = "n",
  #          float = FALSE,
  #          out=filename,
  #          type='latex')
}

results_textual_analysis <- function(folder="Results", models="all", surveys="all", type="type", plot_scale=scale_fill_discrete) {
  
  df <- data_survey(models)
  
  dfsp <- df %>%
    mutate(type=get(!!type))
  if (surveys != "all") { dfsp <- dfsp %>% filter(survey==!!surveys) }
  
  
  #######################
  # Plot Recommendation #
  #######################
  
  topics <- c("explanation_reporting", "explanation_legal",
              "explanation_stlt", "explanation_norm",
              "explanation_harm", "explanation_kant")  
  
  # For text values
  if (F) {
    # Percentage of topics per type
    dfsp %>% 
      group_by(type, recommend_new_policy) %>%
      summarise(across(topics, ~ mean(., na.rm=T))) #%>%
      left_join(dfs %>% filter(explanation=="Reporting Motive") %>% group_by(type) %>% summarise(N=n())) %>%
      mutate(perc = n / N)
      
  }
  
  
  dfs <- dfsp %>%
    select(all_of(c("type", topics, "weight"))) %>%
    mutate(ntopics = rowSums(dfsp %>% select(all_of(topics)))) %>%
    mutate(across(topics, ~ ifelse(ntopics>0, .x / ntopics, 0))) %>%
    select(-ntopics) %>%
    pivot_longer(!c(type, weight), names_to = "question", values_to = "value") %>%
    mutate(across('question', \(x) str_replace(x, 'explanation_', 'explanation:'))) %>%
    separate(question, c("tmp", "explanation"), sep=":") %>%
    select(-tmp) %>%
    drop_na(value) %>%
    mutate(explanation = factor(explanation,
                                c("reporting", "legal", "stlt", "norm",
                                  "harm", "kant"),
                                c("Reporting Motive", "Legal Liability", "Long-term focus",
                                  "Social Norms", "Investor Harm", "Kant"))) %>%
    arrange(explanation)
  
  
  
  
  dfp <- dfs %>%
    group_by(type, explanation) %>%
    summarise(
      sum=sum(value * weight), # Number of times this explanation has been provided
      .groups = "keep") %>%
    left_join(dfs %>% group_by(type) %>% summarise(all=sum(value * weight))) %>%
    mutate(mean = sum / all) 
  if (models=="pilot") {
    # Order by Humans means
    dfp <- dfp %>%
      mutate(explanation = factor(
        explanation,
        dfp %>% filter(type=="Humans") %>% arrange(mean) %>%pull(explanation)))
    
  }
  
  pl <- ggplot(data=dfp, aes(x=mean, y=explanation, fill=type)) +
    geom_bar(stat="identity",
             position=position_dodge(.8),
             width = .6) + 
    xlab(NULL) + ylab(NULL) + labs(fill="") +
    plot_scale() +
    theme_classic() +
    guides(fill=guide_legend(nrow=ceiling(length(unique(dfp$type))/4))) +
    theme(text=element_text(size=12,  family="serif"),
          #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position="bottom",
          plot.margin = margin(0,0,0,0))
  
  ns = c("explanations", surveys, type, models)
  filename = paste0(folder, "/plots/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".pdf")
  ggsave(filename, width=4, height=3)
  
  #############################
  # Plot for number of topics #
  #############################
  dfp <- dfsp %>%
    select(c(type,
             explanation_reporting, explanation_legal,
             explanation_stlt, explanation_norm,
             explanation_harm, explanation_kant,
             weight
             )) %>%
    mutate(ntopics = 
      explanation_reporting + explanation_legal + explanation_stlt +
      explanation_norm + explanation_harm + explanation_kant) %>%
    group_by(type, ntopics) %>%
    summarise(
      sum=sum(weight),
      .groups = "keep") %>%
    left_join(dfsp %>% group_by(type) %>% summarise(n=n())) %>%
    drop_na() %>%
    mutate(mean = sum / n) %>%
    mutate(ntopics = factor(ntopics))
  
  pl <- ggplot(data=dfp, aes(x=ntopics, y=mean, fill=type)) +
    geom_bar(stat="identity",
             position=position_dodge2(preserve = "single"),
             width = .6) + 
    xlab("Number of Topics") + ylab(NULL) + labs(fill="") +
    plot_scale() +
    scale_x_discrete(drop=F) +
    theme_classic() +
    guides(fill=guide_legend(nrow=ceiling(length(unique(dfp$type))/4))) +
    theme(text=element_text(size=12,  family="serif"),
          #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position="bottom",
          plot.margin = margin(0,0,0,0))
  
  ns = c("explanations_topics", surveys, type, models)
  filename = paste0(folder, "/plots/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".pdf")
  ggsave(filename, width=4, height=3)
  
  ################
  # Plot Ethical #
  ################
  topics <- c("ethical_reporting", "ethical_legal",
              "ethical_stlt", "ethical_norm",
              "ethical_harm", "ethical_kant")  
  
  dfs <- dfsp %>%
    select(all_of(c("type", topics, "weight"))) %>%
    mutate(ntopics = rowSums(dfsp %>% select(all_of(topics)))) %>%
    mutate(across(topics, ~ ifelse(ntopics>0, .x / ntopics, 0))) %>%
    select(-ntopics) %>%
    pivot_longer(!c(type, weight), names_to = "question", values_to = "value") %>%
    mutate(across('question', \(x) str_replace(x, 'ethical_', 'ethical:'))) %>%
    separate(question, c("tmp", "ethical"), sep=":") %>%
    select(-tmp) %>%
    drop_na(value) %>%
    mutate(ethical = factor(ethical,
                                c("reporting", "legal", "stlt", "norm",
                                  "harm", "kant"),
                                c("Reporting Motive", "Legal Liability", "Long-term focus",
                                  "Social Norms", "Investor Harm", "Kant"))) %>%
    arrange(ethical)
  
  dfp <- dfs %>%
    group_by(type, ethical) %>%
    summarise(
      sum=sum(value * weight), # Number of times this explanation has been provided
      .groups = "keep") %>%
    left_join(dfs %>% group_by(type) %>% summarise(all=sum(value * weight))) %>%
    mutate(mean = sum / all) 
  if (models=="pilot") {
    # Order by Humans means
    dfp <- dfp %>%
      mutate(ethical = factor(
        ethical,
        dfp %>% filter(type=="Humans") %>% arrange(mean) %>% pull(ethical)))
    
  }
  
  pl <- ggplot(data=dfp, aes(x=mean, y=ethical, fill=type)) +
    geom_bar(stat="identity",
             position=position_dodge(.8),
             width = .6) + 
    xlab(NULL) + ylab(NULL) + labs(fill="") +
    plot_scale() +
    theme_classic() +
    guides(fill=guide_legend(nrow=ceiling(length(unique(dfp$type))/4))) +
    theme(text=element_text(size=12,  family="serif"),
          #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position="bottom",
          plot.margin = margin(0,0,0,0))
  
  ns = c("ethical", surveys, type, models)
  filename = paste0(folder, "/plots/", paste(ns[ns!="all" & ns!="type"], collapse="_"), ".pdf")
  ggsave(filename, width=4, height=3)
  
    
}

regressions_llms <- function(folder="Results") {
  df <- data_survey(pilot)
  
  surveys = c("all", "overreporting", "underreporting")
  
  for (s in surveys) {
    
      dfsp <- df %>%
        filter(type == "LLMs")
      
      if (s != "all") { dfsp <- dfsp %>% filter(survey==!!s) }
      
      dfr <- dfsp %>%
        select(c(access,
                 recommend_new_policy, board_vote,
                 current_policy_ethic, new_policy_ethic,
                 current_policy_lying, new_policy_lying,
                 current_policy_deceptive, new_policy_deceptive,
                 current_policy_dishonest, new_policy_dishonest,
                 current_policy_untruthful, new_policy_untruthful
                 )) %>%
        mutate(
          Ethical = new_policy_ethic - current_policy_ethic,
          Lying = new_policy_lying - current_policy_lying,
          Deceptive = new_policy_deceptive - current_policy_deceptive,
          Dishonest = new_policy_dishonest - current_policy_dishonest,
          Untruthful = new_policy_untruthful - current_policy_untruthful,
        ) %>%
        mutate(recommend_new_policy=recode(recommend_new_policy, "Yes"=1, "No opinion"=0, "No"=-1)) %>%
        mutate(board_vote=recode(board_vote, "In Favor"=1, "Follow Consensus"=0, "Abstain"=0, "Against"=-1)) %>%
        select(access, recommend_new_policy, board_vote,
               Ethical, Lying, Deceptive, Dishonest, Untruthful)
      
      
      ##########################################
      # Recommendations All LLMs vs Local LLMs #
      ##########################################
      
      f_rec = formula(recommend_new_policy ~ Ethical + Lying + Deceptive + Dishonest + Untruthful)
      f_rev = formula(board_vote ~ Ethical + Lying + Deceptive + Dishonest + Untruthful)
      
      m = list()
      
      # Original Recommendation
      m <- append(m, list(lm(f_rec, data=dfr)))
      m <- append(m, list(lm(f_rec, data=dfr %>% filter(access=='Online LLMs'))))
      m <- append(m, list(lm(f_rec, data=dfr %>% filter(access=='Local LLMs'))))
      
      # Revised Recommendation
      m <- append(m, list(lm(f_rev, data=dfr)))
      m <- append(m, list(lm(f_rev, data=dfr %>% filter(access=='Online LLMs'))))
      m <- append(m, list(lm(f_rev, data=dfr %>% filter(access=='Local LLMs'))))
      
      
      ns = c("regression_llm", s)
      filename = paste0(folder, "/tables/", paste(ns[ns!="all"], collapse="_"), ".tex")
      
      stargazer(m,
                dep.var.caption = "",
                dep.var.labels = c("Original Recommendation", "Revised Recommendation"),
                column.labels = c("All", "Online", "Local", "All", "Online", "Local"),
                keep.stat = c("n","rsq", "adj.rsq"),
                omit.table.layout = "n",
                float = FALSE,
                out=filename,
                type='latex')
      
      #######################################
      # Online VS Local using Google models #
      #######################################
      # Use Gemini-1.5-flash-8b vs Gemma-9b
      
      dfr <- dfsp %>%
        filter(model %in% c("gemini-1.5-flash-8b", "gemma2:9b"))
      
      ###############
      # Size Effect #
      ###############
      # Use Llama and Gemma at all sizes
      # with size indicator and LLM fixed effects
      
      ###################################
      # RLHF Effect (see Mistral maybe) #
      ###################################
      
      
  }
      
}
