visualise_re <- function(epis) {
  dat <- collect_statistics(epis)
  
  g_re <-
    dat %>% filter(Loc != "Overall") %>% select(Loc, R0, Re) %>%
    melt(id.vars = "Loc", measure.vars = c("R0", "Re")) %>% group_by(Loc, variable) %>%
    summarise_all(funs(mean = mean(value), lower = quantile(value, 0.025), upper = quantile(value, 0.975))) %>%
    group_by(Loc) %>% mutate(key = max(mean)) %>% ungroup() %>%
    mutate(hubei = ifelse(Loc == "Hubei", "red", "black")) %>% 
    ggplot(aes(x = reorder(Loc, key))) +
    geom_pointrange(aes(y = mean, ymin = lower, ymax = upper, colour = variable), 
                    position = position_dodge(width = 0.3)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_y_continuous("Reproduction number", breaks = c(0, 1, 2, 4, 6, 8)) +
    scale_x_discrete("Province") +
    coord_flip() +
    scale_color_discrete("", labels = c("R0", "R(t)")) +
    expand_limits(y = c(0, 8)) +
    guides(colour = guide_legend(reverse = T)) +
    theme(legend.position = c(0.8, 0.5))
  
  return(g_re)
} 

visualise_peak <- function(epis, bind = T) {
  dat <- collect_statistics(epis)
  
  peaks <- dat %>% filter(Loc != "Overall") %>% select(Loc, PeakSize, PeakTime) %>% group_by(Loc) %>% 
    summarise_all(funs("mean", lower = quantile(., 0.025), upper = quantile(., 0.975)))
  
  
  g_peak_size <- 
    peaks %>%
    mutate(hubei = ifelse(Loc == "Hubei", "red", "black")) %>% 
    ggplot(aes(x = reorder(Loc, PeakSize_mean))) +
    geom_pointrange(aes(y = PeakSize_mean, ymin = PeakSize_lower, ymax = PeakSize_upper, colour = hubei)) +
    scale_y_log10("Size of outbreak peak", breaks = c(1, 10, 100, 1000, 10000)) +
    scale_x_discrete("Province") +
    scale_color_manual(values = c("black", "red")) +
    coord_flip() +
    theme(legend.position = "none")

  date0 <- epis[[1]]$Forecasts$Date[1] - 1
  
  g_peak_time <- 
    peaks %>%
    mutate(hubei = ifelse(Loc == "Hubei", "red", "black"),
           order = PeakTime_mean,
           PeakTime_mean = date0 + round(PeakTime_mean),
           PeakTime_lower = date0 + round(PeakTime_lower),
           PeakTime_upper = date0 + round(PeakTime_upper)) %>% 
    ggplot(aes(x = reorder(Loc, order))) +
    geom_pointrange(aes(y = PeakTime_mean, ymin = PeakTime_lower, ymax = PeakTime_upper, colour = hubei)) +
    scale_y_date("Date of outbreak peak") +
    scale_x_discrete("Province") +
    scale_color_manual(values = c("black", "red")) +
    coord_flip() +
    theme(legend.position = "none")
  
  
  return(list(
    g_peak_size = g_peak_size, 
    g_peak_time = g_peak_time
  ))
}

visualise_lockdown <- function(epis, bind = T) {
  dat <- collect_statistics(epis)
  
  lockdown <- dat %>% filter(Loc != "Overall") %>% 
    select(Loc, PrExFOI, PAF_2, PAF_4) %>% group_by(Loc) %>% 
    summarise_all(funs("mean", lower = quantile(., 0.025), upper = quantile(., 0.975))) %>%
    mutate(hubei = ifelse(Loc == "Hubei", "red", "black"))
  
  
  g_exo <- 
    lockdown %>% 
    ggplot(aes(x = reorder(Loc, PrExFOI_mean))) +
    geom_pointrange(aes(y = PrExFOI_mean, ymin = PrExFOI_lower, 
                        ymax = PrExFOI_upper, colour = hubei)) +
    scale_color_manual(values = c("black", "red")) +
    coord_flip() +
    expand_limits(y=c(0, 100))
  
  #ggsave(plot = g_exo, filename = "Output/Figure/ExoFOI.pdf")
  
  g_paf <- 
    lockdown %>%
    ggplot(aes(x = reorder(Loc, PAF_2_mean))) +
    geom_pointrange(aes(y = PAF_2_mean, ymin = PAF_2_lower, ymax = PAF_2_upper, colour = hubei)) +
    scale_x_discrete("Province", position = "right") +
    scale_color_manual(values = c("black", "red")) +
    expand_limits(y=c(0, 100)) +
    coord_flip()
  
  if (bind) {
    g_exo <- g_exo +
      scale_y_reverse("Percentage (%)\n12th February") +
      scale_x_discrete("Province", position = "left") +
      labs(title = "A. Exogenous force of infection") +
      theme(legend.position = "none", axis.title.y = element_blank(),
            axis.text.y = element_blank())
    
    g_paf <- g_paf +
      scale_y_continuous("Percentage (%)\ntwo weeks after overwhelming lockdown") +
      labs(title = "B. Population Attributable Fraction") +
      theme(legend.position = "none", axis.title.y = element_blank(), 
            axis.text.y = element_text(hjust=0))
    
    links <- with(lockdown, {
      x0 <- reorder(Loc, PrExFOI_mean)
      x0 <- sort(x0)
      anc <- 1:length(x0)
      names(anc) <- levels(x0)
      x1 <- reorder(Loc, PAF_2_mean)
      lx1 <- levels(x1)
      xo <- rep(0, length(x0))
      for (i in 1:length(x0)) {
        xo[i] <- which(lx1 == x0[i])
      }
      x1 <- factor(levels(x0)[xo], levels(x0))
      data.table(x0 = x0, x1 = x1, up = 1:length(x0) > xo)
    })
    
    
    g_link <- 
      links %>%
      ggplot() +
      geom_segment(aes(x = x0, y = 0, xend = as.character(x1), yend = 1, colour = up), size = 1) +
      scale_color_discrete() +
      scale_y_continuous(" \n", breaks = 0:1, labels = c("", "")) +
      coord_flip() +
      labs(title = " ") +
      theme(legend.position = "none", axis.title.y = element_blank())
    
    g_lock <- grid.arrange(g_exo, g_link, g_paf, 
                           layout_matrix = matrix(1:3, 1, 3), widths=c(2.3, 2, 3))
    return(g_lock)
  } else {
    
    g_exo <- g_exo +
      scale_y_continuous("Exogenous force of infection (%)\n12/02/2020") +
      scale_x_discrete("Province") +
      theme(legend.position = "none")
    
    g_paf <- g_paf +
      scale_y_continuous("Population Attributable Fraction (%)\ntwo weeks after overwhelming lockdown") +
      theme(legend.position = "none")
      
    
    return(list(
      g_exo = g_exo,
      g_paf = g_paf
    ))
  }
}

visualise_ts_prov <- function(epi, loc) {
  cases <- epi$Data
  
  cases$Index = "Comfirmed cases among effective sample size"
  
  back <- rbindlist(list(
    extract_fitted(epi, key = "Cases_hat", name = "Comfirmed cases among effective sample size"),
    extract_fitted(epi, key = "Re_hat", name = "Effective reproduction number")
  ))
  
  fore <- rbind(
    extract_tab(epi, key = "Prv", name = "Comfirmed cases among effective sample size"),
    extract_tab(epi, key = "Re", name = "Effective reproduction number")
  )
  
  g_prov <- ggplot(fore, aes(x = Date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) +
    geom_point(data = cases, aes(y = Value)) +
    geom_ribbon(data = back, aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(data = back, aes(y = mean)) +
    scale_y_continuous("Number of cases") + 
    facet_wrap(Index~., scales = "free_y", nrow = 2) +
    labs(title = loc) +
    theme(legend.position = "none")
  
  return(g_prov)
}


visualise_ts_r0 <- function(epis) {
  for(i in names(epis)) {
    epis[[i]]$Location <- i
  }
  
  re_prov <- rbindlist(lapply(epis, function(epi) {
    tab <- rbind(
      extract_fitted(epi, key = "Re_hat", name = "Effective reproduction number"),
      extract_tab(epi, key = "Re", name = "Effective reproduction number")
    )
    tab <- cbind(tab, Loc = epi$Location)
    tab
  }))
  
  r0_prov <- collect_statistics(epis)[, .(Loc, R0)] %>% group_by(Loc) %>% summarise_all("mean")
  
  g_re <- ggplot(re_prov, aes(x = Date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) +
    geom_hline(data = r0_prov, aes(yintercept = R0, colour = "R0")) +
    geom_vline(aes(xintercept = as.Date("20200212", "%Y%m%d")), linetype = "dashed") +
    geom_hline(aes(yintercept = 1, colour = "R(t) = 1")) +
    scale_y_continuous("Effective reproduction number", breaks = c(0, 1, 3, 5, 7, 8)) + 
    scale_x_date("Date", date_labels = "%e %b %Y",
                 breaks = as.Date(c("20200124", "20200212", "20200226", "20200311"), "%Y%m%d"),
                 limits = as.Date(c("20200124", "20200311"), "%Y%m%d")) +
    geom_text(data = r0_prov, 
              aes(x = as.Date("20200302", "%Y%m%d"), 
                  y = R0, 
                  label = paste0("R0=", round(R0, 1)), 
                  vjust = -0.4)) +
    scale_color_discrete("Lines") +
    facet_wrap(Loc~., ncol = 5) +
    labs(title = "") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1))
  
  g_re
}


visualise_ts_r0_sel <- function(epis, sel, nc = 3) {
  epis_sel <- list()
  
  for(i in names(epis)) {
    if (i %in% sel) {
      epis[[i]]$Location <- i
      epis_sel[[i]] <- epis[[i]] 
    }
  }
  
  epis <- epis_sel
  nc <- min(length(sel), nc)
  
  re_prov <- rbindlist(lapply(epis, function(epi) {
    tab <- rbind(
      extract_fitted(epi, key = "Re_hat", name = "Effective reproduction number"),
      extract_tab(epi, key = "Re", name = "Effective reproduction number")
    )
    tab <- cbind(tab, Loc = epi$Location)
    tab
  }))
  
  #re_prov[, upper := min(upper, 5)]
  
  r0_prov <- collect_statistics(epis)[, .(Loc, R0)] %>% group_by(Loc) %>% summarise_all("mean")
  
  g_re <- ggplot(re_prov, aes(x = Date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) +
    geom_hline(data = r0_prov, aes(yintercept = R0, colour = "R0")) +
    geom_vline(aes(xintercept = as.Date("20200212", "%Y%m%d")), linetype = "dashed") +
    geom_hline(aes(yintercept = 1, colour = "R(t) = 1")) +
    # geom_text(data = r0_prov, 
    #           aes(x = as.Date("20200302", "%Y%m%d"), 
    #               y = R0, 
    #               label = paste0("R0=", round(R0, 1)), 
    #               vjust = -1)) +
    scale_y_continuous("Effective reproduction number", breaks = c(0, 1, 3, 5, 7, 8)) + 
    scale_x_date("Date", date_labels = "%e %b %Y",
                 breaks = as.Date(c("20200124", "20200212", "20200226", "20200311"), "%Y%m%d"),
                 limits = as.Date(c("20200124", "20200311"), "%Y%m%d")) +
    scale_color_discrete("Lines") +
    facet_wrap(Loc~., ncol = nc) +
    labs(title = "") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1))
  
  g_re
}


visualise_ts_pex <- function(epis) {
  for(i in names(epis)) {
    epis[[i]]$Location <- i
  }
  
  pex_prov <- rbindlist(lapply(epis, function(epi) {
    tab <- rbind(
      extract_fitted(epi, key = "PrEx_hat", name = "Exogenous FOI")
    )
    tab <- cbind(tab, Loc = epi$Location)
    tab
  }))
  
  g_pex <- ggplot(pex_prov, aes(x = Date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) +
    scale_y_continuous("Fraction of exogenous force of infection (%)") + 
    scale_x_date("Date", date_labels = "%e %b %Y",
                 breaks = as.Date(c("20200124", "20200212", "20200226", "20200311"), "%Y%m%d"),
                 limits = as.Date(c("20200124", "20200212"), "%Y%m%d")) +
    scale_color_discrete("Lines") +
    facet_wrap(Loc~., ncol = 5) +
    labs(title = "") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1))
  
  g_pex
}


visualise_ts_prv <- function(epis, log = T) {
  for(i in names(epis)) {
    epis[[i]]$Location <- i
  }
  
  cases <- rbindlist(lapply(epis, function(epi) {
    d <- epi$Data
    d <- cbind(d, Loc = epi$Location)
  }))
  
  prv_prov <- rbindlist(lapply(epis, function(epi) {
    tab <- rbind(
      extract_fitted(epi, key = "Cases_hat", name = "Comfirmed cases"),
      extract_tab(epi, key = "Prv", name = "Comfirmed cases")
    )
    tab <- cbind(tab, Loc = epi$Location)
    tab
  }))
  
  m_prov <- collect_statistics(epis)[, .(Loc, EffN)] %>% group_by(Loc) %>% summarise_all("mean")
  
  g_prv <- ggplot(prv_prov, aes(x = Date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) +
    geom_hline(data = m_prov, aes(yintercept = EffN, colour = "Effective population size")) +
    geom_vline(aes(xintercept = as.Date("20200212", "%Y%m%d")), linetype = "dashed") +
    geom_point(data = cases, aes(y = Value, colour = "Data"), size = 0.5)
    
  if (log) {
    g_prv <- g_prv + 
      scale_y_log10("Number of active cases, logarithm scale with base 10", breaks = c(5, 50, 500, 5E3, 5E4)) + 
      facet_wrap(Loc~., ncol = 5)
  } else {
    g_prv <- g_prv +
      scale_y_continuous("Active cases") + 
      facet_wrap(Loc~., ncol = 5, scales = "free_y") + 
      geom_text(data = m_prov, 
                aes(x = as.Date("20200302", "%Y%m%d"), 
                    y = EffN, 
                    label = paste0("", round(EffN/1000, 1), "E3"), 
                    vjust = 1.3))
  }
    
  g_prv <- g_prv +
    scale_x_date("Date", date_labels = "%e %b %Y",
                 breaks = as.Date(c("20200124", "20200212", "20200226", "20200311"), "%Y%m%d"),
                 limits = as.Date(c("20200124", "20200311"), "%Y%m%d")) +
    scale_color_discrete("") +
    labs(title = "") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1))
    
  return(g_prv)
}


visualise_scenarios <- function(res_sc) {
  g_scs_ts <- 
    rbindlist(res_sc$SummaryPrv) %>% 
    ggplot(aes(x = Date)) +
    geom_ribbon(aes(ymin = lower/1E3, ymax = upper/1E3, fill = Scenario), alpha = 0.4) +
    geom_line(aes(y = mean/1E3, colour = Scenario)) +
    scale_y_continuous("Number of active cases, thousands") +
    scale_x_date("", breaks = as.Date(c("2020-02-17", "2020-02-24", "2020-03-02", "2020-03-11")),
                 date_labels = "%e %b %Y") +
    labs(tag = "A", title = "Time-series by date") +
    expand_limits(y=c(1, NA)) +
    theme(legend.position = c(0, 0), legend.justification = c(-0.1, -0.1))
  
  g_scs_range <- 
    rbindlist(res_sc$SummaryPrv) %>% 
    filter(Date %in% as.Date(c("17 Feb 2020", "24 Feb 2020", "02 Mar 2020", "11 Mar 2020"), "%d %b %Y")) %>%
    mutate(dl = reorder(format(Date, "%e %b %Y"), Date)) %>%
    ggplot(aes(x = Scenario, y = mean/1E3)) +
    geom_point(aes(colour = Scenario)) +
    geom_errorbar(aes(ymin = lower/1E3, ymax = upper/1E3, colour = Scenario), width = 0.4) +
    scale_y_continuous("Number of active cases, thousands") +
    scale_x_discrete("") +
    facet_grid(.~dl) +
    expand_limits(y=c(1, NA)) +
    labs(tag = "B", title = "Cross-sectional comparison") +
    theme(legend.position = "none", axis.text.x = element_blank())
  
  g_scs <- gridExtra::grid.arrange(g_scs_ts, g_scs_range, heights = c(2.5, 2))
  
  return(g_scs)
}
