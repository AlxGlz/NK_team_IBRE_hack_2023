
# Функция для красивых значений р

pretty_p <- function(x, digits = 3) {
    ifelse(x < 10 ^ (-1 * digits), 
           paste0("<", (10 ^ (-1 * digits)) %>% as.character()),
           as.character(round(x, digits)))
}


# Создание красиво оформленного графика для однофакторной 
# регрессии Кокса

# ### #
# 
# dataset - набор данных
# variables - текстовый вектор с названиями предикторов
# outcome - название переменной исхода
# 
## ### #

simple_cox_uvregression <- function(
        dataset,
        variables,
        outcome,
        time,
        filter_fails = F
){
    
    temp_data <- dataset %>% 
        dplyr::select(
            dplyr::all_of(
                c(variables, outcome, time)
            )
        )
    
    lapply(variables, function(variable){
        
        temp_data <- dataset %>% 
            dplyr::select(outcome = all_of(outcome),
                          predictor = all_of(variable),
                          time = all_of(time))
        
        temp_model <- coxph(Surv(time, outcome) ~ predictor,
                            data = temp_data)
        
        res_table <- temp_model %>% 
            broom::tidy(exponentiate = T,
                        conf.int = T) %>% 
            mutate(varname = variable,
                   levels = str_remove(term, "predictor") %>% na_if(""),
                   .before = everything())
        
        if (is.factor(temp_data$predictor) |
            is.character(temp_data$predictor)){
            
            tibble_with_lelevs <- tibble(varname = variable,
                                         levels = levels(as.factor(temp_data$predictor)))
            
            res_table <- tibble_with_lelevs %>% 
                full_join(res_table)
            
        }
        
        res_table
        
    }) %>% 
        bind_rows %>% 
        dplyr::select(
            varname,
            levels,
            estimate,
            conf.low,
            conf.high,
            p.value
        ) -> res_table_full
    
    res_table_full %>%
        filter(!(is.na(conf.low) | is.na(conf.high))) %>%
        mutate(y = ifelse(is.na(levels), varname,
                          str_c(varname, ": ", levels)) %>%
                   factor(., levels = unique(.), ordered = T),
               .after = levels) %>%
        ggplot(aes(y = reorder(y, desc(y)), x = estimate)) +
        geom_point(shape = 15, size = 3) +
        geom_vline(xintercept = 1,
                   linewidth = 1,
                   linetype = "dashed",
                   color = "red") +
        geom_linerange(aes(xmin = conf.low,
                           xmax = conf.high)) +
        scale_x_log10() +
        theme_bw() +
        labs(x = "Отношение рисков\nHazard Ratio, HR)",
             y = "Фактор") 

}

# Функция для красивого violin plot'a

simple_plot_violin_2gr <- function(data, variable_name, groups_name,
                               hide_ns = T,
                               step_increase = 0.05,
                               y_shift_perc = 2,
                               jitter = F,
                               trim = F){
    
    temp_data <- data %>% 
        dplyr::select(x = all_of(groups_name),
                      y = all_of(variable_name)) %>% 
        drop_na() %>% 
        droplevels()
    
    p_values <- temp_data %>% 
        wilcox_test(y ~ x) %>% 
        add_xy_position(x = "x",
                        step.increase = step_increase) %>% 
        # Отступ от максимума автоматически смещается на y_shift_perc % от масштаба графика
        mutate(y.position = y.position + 
                   (max(temp_data$y, na.rm = T) - min(temp_data$y, na.rm = T)) / 
                   (100/y_shift_perc),
               p.lab = p %>% pretty_p())
    
    plot <- temp_data %>% 
        ggplot(aes(x = x, y = y)) + 
        geom_violin(trim = trim) +
        geom_boxplot(outlier.size = 0.5,
                     width=0.2) + 
        stat_pvalue_manual(p_values,
                           label = "p.lab",
                           label.size = 2.5,
                           hide.ns = hide_ns,
                           tip.length = 0.02) +
 
        labs(x = groups_name,
             y = variable_name) +
        theme_bw()
    
    if (jitter){
        plot <- plot +
            geom_jitter()
    }
    
    plot
    
}

# Создание красивого графика для качественных данных

# ### #
# 
# data - набор данных
# x_name - категории по оси х
# fill_name - переменная для категорий
# palette - название палитры из пакета RColorBrewer
#
# ### #

simple_plot_col <- function(data, x_name, fill_name, 
                            palette = "Pastel1",
                            direction = -1){
    
    data %>% 
        dplyr::select(x = all_of(x_name),
                      y = all_of(fill_name)) %>% 
        drop_na() %>% 
        droplevels() %>% 
        group_by_all() %>% 
        summarise(N = n()) %>% 
        group_by(x) %>% 
        mutate(`Доля пациентов, %` = N / sum(N),
               Label = str_c(N, " (", (100 * `Доля пациентов, %` %>% round(3)), "%)")) %>% 
        ggplot(aes(x = x, y = `Доля пациентов, %`)) + 
        geom_col(aes(fill = y),
                 position = "fill", 
                 width = 0.8, 
                 color = "black") +
        geom_label(aes(label = Label, group = y), 
                   fill = "white",
                   position = position_fill(vjust = 0.5),
                   show.legend = F,
                   alpha = 0.8,
                   label.r = unit(0.00, "lines"),
                   size = 3) +
        labs(x = x_name,
             fill = fill_name) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_brewer(palette = palette,
                          direction = direction)
    
}


