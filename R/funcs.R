#' Plot showing number of times a zone was identified across transects, dates
zonecnt_plo <- function(vegdat, thm){
  
  toplo <- vegdat %>%
    select(site, sample, zone_name) %>% 
    unique %>% 
    filter(!is.na(zone_name)) %>% 
    group_by(zone_name) %>% 
    summarize(
      cnt = n(), 
      .groups = 'drop'
    ) %>% 
    arrange(cnt) %>% 
    mutate(
      zone_name = factor(zone_name, levels = zone_name)
    )
  
  p <- ggplot(toplo, aes(y = zone_name, x = cnt)) + 
    geom_bar(stat = 'identity', alpha = 0.7) + 
    thm +
    theme(
      axis.text.y = element_text(size = 8)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, max(toplo$cnt))) +
    labs(
      y = NULL, 
      x = 'Unique counts across sites, dates'
    )
  
  return(p)

}

# Plot showing total distance zone was recorded across transect, dates
zonedst_plo <- function(vegdat, thm){
  
  toplo <- vegdat %>%
    select(site, sample, zone_name, meter) %>% 
    unique %>% 
    filter(!is.na(zone_name)) %>% 
    group_by(site, sample, zone_name) %>% 
    summarize(
      dist = max(meter) - min(meter),
      .groups = 'drop'
    ) %>% 
    group_by(zone_name) %>% 
    summarise(
      dist = sum(dist, na.rm = T)
    ) %>% 
    arrange(dist) %>% 
    mutate(
      zone_name = factor(zone_name, levels = zone_name)
    )
  
  p <- ggplot(toplo, aes(y = zone_name, x = dist)) + 
    geom_bar(stat = 'identity', alpha = 0.7) + 
    thm +
    theme(
      axis.text.y = element_text(size = 8)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(
      y = NULL, 
      x = 'Total distance recorded, all dates'
    )
  
  return(p)

}

#' site zone distance by date, continuous
sitezonedst_plo1 <- function(vegdat, site, zonefct = NULL, thm){

  dat <- vegdat %>% 
    filter(site == !!site) %>% 
    select(zone_name, zone, sample, meter, date) %>% 
    unique %>% 
    mutate(date = year(date)) %>% 
    unite('zonefct', zone, zone_name, sep = ': ') %>%
    unite('sample', sample, date, sep = ': ') %>% 
    mutate(
      sample = paste('Year:', sample),
      sample = factor(sample, levels = rev(unique(sample))),
      zonefct = factor(zonefct, levels = sort(unique(zonefct)))
    ) 

  if(!is.null(zonefct))
    dat <- dat %>% 
      filter(zonefct %in% !!zonefct)
  
  toplo <- dat
  
  cols <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  levs <- levels(toplo$zonefct)
  colin <- cols(length(levs))
  names(colin) <- levs
  
  p <- ggplot(toplo, aes(x = meter, y = sample)) + 
    geom_line(aes(color = zonefct), stat = 'identity', lwd = 20, alpha = 0.7) + 
    scale_x_continuous(breaks = seq(0, max(toplo$meter), by = 10)) +
    guides(color = guide_legend(override.aes = list(lwd = 7))) +
    scale_colour_manual(values = colin, limits = force) +
    thm +
    theme(
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = 'Meters', 
      y = NULL, 
      subtitle = paste0('Site: ', site),
      color = 'Zone name'
    )
  
  return(p)

}

#' site zone distance by date, by zone
sitezonedst_plo2 <- function(vegdat, site, zonefct = NULL, thm){
  
  dat <- vegdat %>% 
    filter(site == !!site) %>% 
    select(zone_name, zone, sample, meter, date) %>% 
    unique %>% 
    mutate(date = year(date)) %>% 
    unite('zonefct', zone, zone_name, sep = ': ') %>%
    unite('sample', sample, date, sep = ': ') %>% 
    mutate(
      sample = paste('Year:', sample),
      sample = factor(sample, levels = unique(sample)),
      zonefct = factor(zonefct, levels = sort(unique(zonefct)))
    ) %>% 
    group_by(zonefct, sample) %>% 
    summarize(
      dist = max(meter) - min(meter), 
      .groups = 'drop'
    ) 
  
  if(!is.null(zonefct))
    dat <- dat %>% 
      filter(zonefct %in% !!zonefct)
  
  toplo <- dat
  
  cols <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  levs <- levels(toplo$zonefct)
  colin <- cols(length(levs))
  names(colin) <- levs
  
  p <- ggplot(toplo, aes(x = sample, y = dist, color = zonefct, group = zonefct)) + 
    geom_line(alpha = 0.7) +
    geom_point(size = 6) +
    scale_colour_manual(values = colin, limits = force) + 
    thm + 
    labs(
      x = NULL, 
      color = 'Zone', 
      fill = 'Zone', 
      y = 'Meters',
      subtitle = paste0('Site: ', site)
    )
  
  return(p)

}

#' site zone distance by date, species
sitezonedst_plo3 <- function(vegdat, site, zonefct = NULL, thm){
  
  # tofilt <- c('Unknown', 'Open Water', 'none/detritus', 'Woody Debris')
  tofilt <- NULL
  
  dat <- vegdat %>% 
    filter(site == !!site) %>% 
    filter(!species %in% tofilt) %>% 
    unique %>% 
    unite('zonefct', zone, zone_name, sep = ': ') %>% 
    mutate(
      sample = paste('Year:', sample),
      sample = factor(sample, levels = unique(sample)),
      zonefct = factor(zonefct, levels = sort(unique(zonefct)))
    )
  
  if(!is.null(zonefct))
    dat <- dat %>% 
    filter(zonefct %in% !!zonefct)
  
  toplo <- dat
  
  cols <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  levs <- levels(toplo$zonefct)
  colin <- cols(length(levs))
  names(colin) <- levs

  p <- ggplot(toplo, aes(x = meter, y = species, height = pcent_basal_cover / 100, fill = zonefct, color = zonefct)) + 
    geom_ridgeline(stat = 'identity') + 
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, max(toplo$meter), by = 20)) + 
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = colin, limits = force) +
    scale_color_manual(values = colin, limits = force) +
    thm + 
    theme(axis.text.y = element_text(vjust = 0)) +
    labs(
      x = 'Meters', 
      color = 'Zone', 
      fill = 'Zone', 
      y = 'Species relative basal cover', 
      subtitle = paste0('Site: ', site)
    ) + 
    facet_wrap(~sample, ncol = 2)
  
  return(p)

}

#' summarize species at a site, zone optional, used for tabular or graphical summary
sitezonesum_fun <- function(vegdat, site, zonefct = NULL, var = c('fo', 'cover')){
  
  var <- match.arg(var)
 
  dat <- vegdat %>% 
    filter(site %in% !!site) %>% 
    unite('zonefct', zone, zone_name, sep = ': ') %>% 
    mutate(zonfect = factor(zonefct, levels = sort(unique(zonefct)))) 

  if(!is.null(zonefct))
    dat <- dat %>% 
      filter(zonefct %in% !!zonefct)
  
  # get complete data by filling species as zero
  dat <- dat %>% 
    select(site, sample, meter, zonefct, species, pcent_basal_cover) %>%
    tidyr::complete(species, tidyr::nesting(site, sample, zonefct, meter), fill = list(pcent_basal_cover = 0))
  
  # freq occ estimates
  if(var == 'fo')

    out <- dat %>%
      mutate(
        pa = ifelse(pcent_basal_cover > 0, 1, 0)
      ) %>%
      select(-pcent_basal_cover) %>%
      unique %>%
      group_by(site, sample, zonefct, species) %>%
      summarise(
        yval = sum(pa) / n(),
        .groups = 'drop'
      )

  # % basal cover estimates    
  if(var == 'cover')
    
    out <- dat %>% 
      unique %>% 
      group_by(site, sample, zonefct, species) %>% 
      summarise(
        yval = mean(pcent_basal_cover) / 100, 
        .groups = 'drop'
      )
  
  out <- out %>% 
    filter(yval > 0)
     
  return(out)
  
}

# tabular output from sitezonesum_fun
sitezonesum_tab <- function(vegdat, site, zonefct = NULL, var = c('fo', 'cover')){

  var <- match.arg(var)
  
  totab <- sitezonesum_fun(vegdat, site, zonefct, var)
  
  ylab <- 'Mean basal % cover'
  if(var == 'fo')
    ylab <- '% Frequency Occurrence'

  totab <- totab %>% 
    mutate(
      sample = paste('Year', sample)
    ) %>% 
    pivot_wider(names_from = 'sample', values_from = 'yval', values_fill = NA) %>%
    arrange(zonefct, species) 
  
  tab <- reactable(
    totab,
    groupBy = c('zonefct'),
    columns = list(
      site = colDef(show = F),
      zonefct = colDef(name = 'Zone'),
      species = colDef(name = 'Species')
    ), 
    defaultColDef = colDef(format = colFormat(digits = 1, percent = T), align = 'left'), 
    resizable = T, 
    defaultExpanded = T
    )
  
  return(tab)
  
}

#' summarize species at a site, across zones, used for tabular or graphical summary
sitesum_fun <- function(vegdat, site, delim, delimtyp = c('Number', 'Distance'), vegsel, var = c('fo', 'cover'), zonefct = NULL, torm = NULL){
  
  delimtyp <- match.arg(delimtyp)
  var <- match.arg(var)
  
  dat <- vegdat %>% 
    filter(site %in% !!site) %>% 
    unite('zonefct', zone, zone_name, sep = ': ') %>% 
    mutate(zonfect = factor(zonefct, levels = sort(unique(zonefct)))) %>% 
    select(site, sample, meter, zonefct, species, pcent_basal_cover) %>%
    tidyr::complete(species, tidyr::nesting(site, sample, zonefct, meter), fill = list(pcent_basal_cover = 0)) %>% 
    filter(!species %in% torm) 
  
  # make uniform levels for open water, unknown, woody debris, none/detritus
  notspp <- c('Open Water', 'Unknown', 'Woody Debris, none/detritus')
  spp <- unique(dat$species) %>% 
    sort %>% 
    .[!. %in% notspp]
  spp <- c(spp, notspp)
  dat <- dat %>% 
    mutate(species = factor(species, levels = spp))

  # get breaks and labels for meter cuts
  delims <- unique(dat$meter) %>% 
    as.numeric %>% 
    range
  
  maxdelim <- max(dat$meter)
  if(delimtyp == 'Number'){ 
    delims <- seq(delims[1], delims[2], length.out = delim + 1)
    lbs <- round(delims, 0)[-length(delims)]
    lbs <- paste(lbs, c(lbs[-1], round(maxdelim, 0)), sep = '-')
  }
  
  # this is junk because there's usually a remainder
  if(delimtyp == 'Distance'){
    delims <- seq(delims[1], delims[2], by = delim) %>% 
      c(maxdelim) %>% 
      unique
    
    lbs <- round(delims, 0)
    lbs <- lbs[-length(lbs)]
    lbs <- paste(lbs, c(lbs[-1], round(maxdelim, 0)), sep = '-')
  }

  # filter by zones
  if(!is.null(zonefct))
    dat <- dat %>%
      filter(zonefct %in% !!zonefct)

  # add delimiter grouping
  dat <- dat %>% 
    mutate(
      meter_grp = cut(meter, breaks = delims, labels = lbs, include.lowest = T, right = F)
    ) 
  
  # cover
  if(var == 'cover')
    sums <- dat %>% 
      group_by(sample, species, meter_grp) %>% 
      summarize(yval = sum(pcent_basal_cover), .groups = 'drop') %>% 
      filter(yval > 0)
    
  # frequency occurrence
  if(var == 'fo')
    sums <- dat %>%
      mutate(
        pa = ifelse(pcent_basal_cover > 0, 1, 0)
      ) %>%
      unique %>%
      group_by(sample, meter_grp, species) %>%
      summarise(
        yval = sum(pa) / n(),
        .groups = 'drop'
      ) %>% 
      filter(yval > 0)

  # get selection to filter summaries by actual species list or count
  sppflt <- vegsel
  if(is.numeric(vegsel))
    sppflt <- sums %>% 
      group_by(species) %>% 
      summarise(yval = sum(yval)) %>% 
      arrange(-yval) %>% 
      pull(species) %>% 
      .[1:vegsel]
    
  out <- sums %>% 
    filter(species %in% sppflt)
  
  return(out)

}

# plot results for sitesum_fun
sitesum_plo <- function(vegdat, site, delim, delimtyp, vegsel, var = c('fo', 'cover'), zonefct = NULL, thm){
  
  var <- match.arg(var)
  
  toplo <- sitesum_fun(vegdat, site, delim, delimtyp, vegsel, var, zonefct) %>% 
    mutate(
      sample = paste0('Year ', sample)
    )
  
  cols <- RColorBrewer::brewer.pal(9, 'Set1') %>% 
    colorRampPalette(.)

  leglab <- 'Selected species'
  if(is.numeric(vegsel)){
    vegsel <- min(c(vegsel, length(unique(toplo$species))))
    leglab <- paste('Top', vegsel, 'species')
  }
  
  levs <- levels(toplo$species)
  colin <- cols(length(levs))
  names(colin) <- levs
  
  if(var == 'cover')
    ylab <- 'Sum of basal % cover'
  if(var == 'fo')
    ylab <- 'Freq. Occ. (%)'
  
  p <- ggplot(toplo, aes(x = meter_grp, y = yval, fill = species)) + 
    geom_bar(stat = 'identity', color = 'black') + 
    scale_x_discrete(drop = F) +
    scale_fill_manual(values = colin, limits = force) +
    facet_wrap(~sample, ncol = 1, drop = F) + 
    thm +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      y = ylab, 
      x = 'Meter distance', 
      fill = leglab
    )
  
  return(p)
  
}

#' single species summary across sites, zones
sppsum_plo <- function(vegdat, sp, var = c('fo', 'cover'), sitefct = NULL, thm){
  
  var <- match.arg(var)
  
  dat <- vegdat %>% 
    mutate(site = factor(site))
  
  if(var == 'fo'){
  
    dgval <- 0
    ylab <- 'Freq. Occ.'
    
    toplo <- dat %>% 
      group_by(site, sample, meter) %>% 
      summarise(
        pres = sp %in% species,
        .groups = 'drop'
      ) %>% 
      group_by(site, sample) %>% 
      summarise(
        yval = sum(pres) / n(), 
        .groups = 'drop'
      )
   
  }
    
  if(var == 'cover'){
    
    dgval <- 0.1
    ylab <- 'Mean % basal cover (+/- 95% CI)'
    
    toplo <- dat %>%
      select(site, sample, meter, species, pcent_basal_cover) %>% 
      tidyr::complete(species, tidyr::nesting(site, sample, meter), fill = list(pcent_basal_cover = 0)) %>%
      filter(species %in% !!sp) %>% 
      group_by(site, sample) %>% 
      summarise(
        yval = mean(pcent_basal_cover), 
        lov = t.test(pcent_basal_cover)$conf.int[1], 
        hiv = t.test(pcent_basal_cover)$conf.int[2], 
        .groups = 'drop'
      ) 
  
  }
  
  toplo <- toplo %>% 
    mutate(
      sample = paste('Year', sample),
      sample = factor(sample)
    )
    
  if(!is.null(sitefct))
    toplo <- toplo %>% 
      filter(site %in% !!sitefct)

  cols <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  levs <- levels(toplo$site)
  colin <- cols(length(levs))
  names(colin) <- levs
  
  dodge <- position_dodge(width = dgval) 
  
  p <- ggplot(toplo, aes(x = sample, y = yval, color = site, group = site)) + 
    geom_line(alpha = 0.7, position = dodge) +
    geom_point(size = 6, position = dodge) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(values = colin, limits = force) + 
    thm + 
    labs(
      x = NULL, 
      color = 'Site', 
      fill = 'Site', 
      y = ylab
    ) 

  if(var == 'cover')
    p <- p + 
      geom_errorbar(aes(ymin = lov, ymax = hiv), position = dodge, width = 0)

  return(p)
  
}

#' summarise tree plot data into species by zone or just by zone
treesum_fun <- function(treedat, site, byspecies = T, zonefct = NULL,
                        var = c("cm2_m2", "m2_ha", "relcov_per", "trees_ha", "trees_m2", "rich", "tree_height")){
  
  var <- match.arg(var)

  if(byspecies & var == 'rich')
    stop('Cannot use var = "rich" with byspecies = "TRUE"')
  
  dat <- treedat %>% 
    filter(site %in% !!site) %>% 
    unite('zonefct', zone, zone_name, sep = ': ') %>% 
    mutate(
      zonefct = factor(zonefct, levels = sort(unique(zonefct))), 
      species = factor(species)
    )
  
  if(!is.null(zonefct))
    dat <- dat %>% 
      filter(zonefct %in% !!zonefct)
  
  # handle tree height different since it's already present
  if(var == 'tree_height'){
    
    if(byspecies)
      dat <- dat %>% 
        group_by(site, sample, zonefct, species) %>%
        summarize(
          val = mean(tree_height, na.rm = T),
          .groups = 'drop'
        )
        
    if(!byspecies)
      dat <- dat %>% 
        group_by(site, sample, zonefct) %>%
        summarize(
          val = mean(tree_height, na.rm = T),
          .groups = 'drop'
        ) 
        
     out <- dat %>% 
       mutate(
         var = 'tree_height',
         varlab = 'Tree height (m)'
       )
     
     return(out)
     
  }
  
  # summarize by plot in each zone first, then density of trees in the zone
  # this is used to get species densities in each zone
  zonedens <- dat %>% 
    group_by(site, sample, zonefct, plot) %>%
    summarize(
      trees_m2 = 12 / pi / sum(dist_to_tree_m ^ 2, na.rm = T),
      cnt = n(),  
      .groups = 'drop'
    ) %>% 
    mutate(
      trees_m2 = case_when( # correction factor if four points were not sampled
        cnt == 4 ~ trees_m2, 
        cnt == 3 ~ trees_m2 * 0.58159, 
        cnt == 2 ~ trees_m2 * 0.3393,
        cnt == 1 ~ trees_m2 * 0.15351
      )
    ) %>% 
    group_by(site, sample, zonefct) %>% 
    summarise(
      trees_m2 = mean(trees_m2, na.rm = T), 
      .groups = 'drop'
    )
  
  # get species density summaries by zone
  # uses results from above
  zonesppsum <- dat %>%  
    group_by(site, sample, zonefct) %>%
    mutate(
      dbh_cm_gr0 = sum(dbh_cm > 0, na.rm = T), 
      ba_cm2 = pi * (dbh_cm / 2) ^ 2, 
      ba_cm2sum = sum(ba_cm2, na.rm = T)
    ) %>% 
    group_by(site, sample, zonefct, species) %>%
    inner_join(zonedens, by = c('site', 'sample', 'zonefct')) %>% 
    summarise(
      trees_m2 = unique(trees_m2) * n() / unique(dbh_cm_gr0), 
      cm2_m2 = mean(ba_cm2, na.rm = T), 
      relcov_per = 100 * sum(ba_cm2, na.rm = T) / unique(ba_cm2sum),
      .groups = 'drop'
    ) %>% 
    mutate(
      trees_ha = trees_m2 * 1e4, 
      m2_ha = trees_ha * cm2_m2 / 1e4
    ) %>% 
    pivot_longer(names_to = 'var', values_to = 'val', -matches(c('site', 'sample', 'zone_name', 'zone', 'species'))) %>% 
    mutate(
      varlab = case_when(
        var == 'trees_m2' ~ 'Absolute species density (trees/m2)', 
        var == 'cm2_m2' ~ 'Species average basal area (cm2/m2)', 
        var == 'relcov_per' ~ 'Relative cover (%)', 
        var == 'trees_ha' ~ 'Absolute species density (trees/ha)', 
        var == 'm2_ha' ~ 'Species absolute cover (m2/ha)'  
      )
    )
  
  out <- zonesppsum
  
  # summarise the above across zone
  if(!byspecies){

    richdat <- out %>%  
      group_by(site, sample, zonefct) %>%
      summarise(
        val = length(unique(species)), 
        .groups = 'drop'
      ) %>% 
      mutate(
        var = 'rich', 
        varlab = 'Species richness'
      )
    
    out <- out %>% 
      group_by(site, sample, zonefct, var, varlab) %>% 
      summarise(
        val = sum(val, na.rm = T),
        .groups = 'drop'
      ) %>% 
      bind_rows(richdat) %>% 
      arrange(site, sample, zonefct)
    
  }
  
  out <- out %>% 
    filter(var %in% !!var)

  return(out)
  
}

#' tree site summary table
treesum_tab <- function(treedat, site, byspecies = T, zonefct = NULL,
                        var = c("cm2_m2", "m2_ha", "relcov_per", "trees_ha", "trees_m2", "rich", "tree_height")){
  
  totab <- treesum_fun(treedat, site = site, byspecies = byspecies, zonefct = zonefct, var = var) %>% 
    mutate(
      sample = paste('Year', sample)
    ) %>% 
    pivot_wider(names_from = 'sample', values_from = 'val', values_fill = NA) %>%
    arrange(zonefct) 
  
  if(byspecies)
    tab <- reactable(
      totab,
      groupBy = c('zonefct'),
      columns = list(
        var = colDef(show = F),
        varlab = colDef(show = F),
        site = colDef(show = F),
        zonefct = colDef(name = 'Zone'),
        species = colDef(name = 'Species')
      ), 
      defaultColDef = colDef(format = colFormat(digits = 1), align = 'left'), 
      resizable = T, 
      defaultExpanded = T
    )
  
  if(!byspecies)
    tab <- reactable(
      totab,
      columns = list(
        var = colDef(show = F),
        varlab = colDef(show = F),
        site = colDef(show = F),
        zonefct = colDef(name = 'Zone', minWidth = 200)
      ), 
      defaultColDef = colDef(format = colFormat(digits = 1)), 
      resizable = T, 
      defaultExpanded = T
    )

  
  ttl <- paste(site, unique(totab$varlab), sep = ', ')
  out <-  prependContent(tab, h5(class = "title", ttl))
  
  return(out)
  
}

#' tree site summary plot
treesum_plo <- function(treedat, site, byspecies, zonefct = NULL, var, thm){
  
  toplo <- treesum_fun(treedat, site, byspecies, zonefct, var) %>% 
    mutate(
      sample = paste('Year', sample)
    )
  
  cols <- RColorBrewer::brewer.pal(8, 'Accent') %>% 
    colorRampPalette(.)
  
  levs <- levels(toplo$species)
  colin <- cols(length(levs))
  names(colin) <- levs
  
  leglab <- unique(toplo$varlab)
  
  if(byspecies)
    p <- ggplot(toplo, aes(x = zonefct, y = val, fill = species)) + 
      geom_bar(stat = 'identity', color = 'black') + 
      scale_x_discrete(drop = F, labels = function(x) str_wrap(x, width = 10)) +
      scale_fill_manual(values = colin, limits = force) +
      facet_wrap(~sample, ncol = 1, drop = F) + 
      thm + 
      labs(
        y = leglab,
        x = NULL, 
        fill = 'Species'
      )
  
  if(!byspecies)
    p <- ggplot(toplo, aes(x = zonefct, y = val)) + 
      geom_bar(stat = 'identity', color = 'black') + 
      scale_x_discrete(drop = F, labels = function(x) str_wrap(x, width = 10)) +
      facet_wrap(~sample, ncol = 1, drop = F) + 
      thm + 
      labs(
        y = leglab,
        x = NULL, 
        fill = 'Species'
      )
  
  return(p)
  
}