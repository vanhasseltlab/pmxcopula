barboxplot_comparison <- function(variables, data_os, data_orig, data_naive = NULL) {
  all_combs <- crossing(unique(data_os[[variables[1]]]), unique(data_os[[variables[2]]]))
  colnames(all_combs) <- variables
  my_label <- function(x) {
    names(x)[[1]] <- stringr::str_wrap(gsub("_", " ", variables[[2]]), 10)
    x[[1]] <- stringr::str_wrap(gsub("_", " ", x[[1]]), 10)
    label_both(x, sep = "\n")
  }

  frequencies_os <- data_os %>%
    select(all_of(variables), sim_nr) %>%
    group_by(across(all_of(variables)), sim_nr) %>%
    summarize(count = n()) %>% ungroup() %>%
    group_by(sim_nr) %>% full_join(all_combs) %>%
    mutate(count = ifelse(is.na(count), 0, count)) %>%
    mutate(freq = count/sum(count)) %>% ungroup()

  frequencies_orig <- data_orig %>%
    select(all_of(variables)) %>%
    group_by(across(all_of(variables))) %>%
    summarize(count = n()) %>% ungroup() %>%
    mutate(freq = count/sum(count)) %>% ungroup()
  if (is.null(data_naive)) {
    bar_box_plots <- frequencies_orig %>%
      ggplot(aes(x = .data[[variables[1]]], y = freq)) +
      geom_bar(stat = "identity", position  = "dodge", aes(fill = "#969696")) +
      geom_vline(xintercept = seq(0.5, 100, by = 1), color = "gray93", linewidth = 0.5) +
      geom_boxplot(data = frequencies_os, alpha = 0.7, aes(fill = "#3ABAC1")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      scale_fill_identity(name = "data", guide = 'legend', labels = c("simulated", "observed")) +
      facet_grid(~ .data[[variables[2]]], labeller = my_label) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), strip.background = element_rect(fill = "white"))
  } else {
    frequencies_naive <- data_naive %>%
      select(all_of(variables), sim_nr) %>%
      group_by(across(all_of(variables)), sim_nr) %>%
      summarize(count = n()) %>% ungroup() %>%
      group_by(sim_nr) %>% full_join(all_combs) %>%
      mutate(count = ifelse(is.na(count), 0, count)) %>%
      mutate(freq = count/sum(count)) %>% ungroup() %>%
      mutate(data = "simulated original")

    frequencies_both <- frequencies_os %>%
      mutate(data = "simulated os") %>%
      bind_rows(frequencies_naive) %>%
      mutate(fill_col = ifelse(data == "simulated os", "#3ABAC1", "#E7C734"))

    bar_box_plots <- frequencies_orig %>%
      ggplot(aes(x = .data[[variables[1]]], y = freq)) +
      geom_bar(stat = "identity", position  = "dodge", aes(fill = "#969696")) +
      geom_vline(xintercept = seq(0.5, 100, by = 1), color = "gray93", linewidth = 0.5) +
      geom_boxplot(data = frequencies_both, alpha = 0.7, aes(fill = fill_col)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      scale_fill_identity(name = "data", guide = 'legend', labels = c("os simulated", "observed", "original simulated")) +
      facet_grid(~ .data[[variables[2]]], labeller = my_label) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), strip.background = element_rect(fill = "white"))
  }

  return(bar_box_plots)
}
