#' # Set of analysis functions
#'
#'
#' @section `wt_ord` details:
#'
#' @description Uses a series of multivariate analyses to determine differences in multiobserver data

#' @param input Character; A wt_download_report tibble
#' @param min_obs Numeric; The minimum number of replicates you want to use. wt_ord will omit
#' @param confidence Numeric; The confidence of the ellipses in the RDA
#'
#' @import
#' @importFrom
#' @export
#'
#' @examples
#' \dontrun{
#' res <- wt_prd(input = data, min_obs = 10, confidence = 0.67)
#' }
#'
#' @return A list containing the following:

wt_ord <- function(input = x, min_obs, confidence) {

  # Filter by the minimum amount of observers. Less takes longer to run
  multi <- input %>%
    group_by(location, recording_date) %>%
    mutate(unique_times = n_distinct(observer)) %>%
    ungroup() %>%
    filter(unique_times >= min_obs) %>%
    select(-unique_times)

  # Set up the data
  multi <- multi %>%
    mutate(abundance = case_when(abundance == "TMTT" ~ "4", abundance == "CI 1" ~ "1", abundance == "CI 2" ~ "2", abundance == "CI 3" ~ "3", TRUE ~ abundance),
           abundance = as.numeric(abundance)) %>%
    group_by(project_name, location, recording_date, species_code, observer) %>%
    mutate(indvs = max(individual_appearance_order)) %>%
    ungroup() %>%
    dplyr::select(project_name, location, recording_date, species_code, observer, indvs, abundance) %>%
    distinct() %>%
    group_by(project_name, location, recording_date, species_code, observer) %>%
    summarise(abundance = case_when(max(abundance) > max(indvs) ~ max(abundance), TRUE ~ max(indvs))) %>%
    ungroup()

  # Create the species matrix
  multi2 <- multi %>%
    dplyr::select(project_name, location, recording_date, observer, species_code, abundance) %>%
    distinct() %>%
    group_by(project_name, location, recording_date, observer, species_code) %>%
    mutate(abundance = as.numeric(max(abundance))) %>%
    ungroup() %>%
    arrange(location, observer, species_code) %>%
    group_by(species_code, project_name, location, recording_date, observer) %>%
    distinct() %>%
    ungroup() %>%
    filter(!species_code %in% abiotic_codes) %>%
    arrange(species_code, project_name, location, recording_date, observer) %>%
    pivot_wider(names_from = species_code, values_from = abundance, values_fill = 0) %>%
    mutate_if(is.integer, as.numeric) %>%
    replace(is.na(.), 0) %>%
    rowwise() %>%
    mutate(total = sum(c_across(`ALFL`:`YRWA`))) %>%
    ungroup() %>%
    filter(!total < 1) %>%
    select(-total)

  # Create the groups
  multi_type <- multi2 %>%
    dplyr::select(location, recording_date, observer) %>%
    distinct()

  if (length(unique(multi2$location)) == 1) {
    stop(print("You need at least two recordings duplicated in order to generate the analysis"))
  }

  # Run the RDA
  ordination <- rda(multi2[,-c(1:4)] ~ observer + location + recording_date, data = multi_type)
  ordination_obs <- rda(multi2[,-c(1:4)] ~ observer, data = multi_type)
  ordination_loc <- rda(multi2[,-c(1:4)] ~ location, data = multi_type)
  ordination_date <- rda(multi2[,-c(1:4)] ~ recording_date, data = multi_type)
  ordination_obs_loc <- rda(multi2[,-c(1:4)] ~ observer + location, data = multi_type)
  ordination_recording <- rda(multi2[,-c(1:4)] ~ location + recording_date, data = multi_type)
  ordination_obs_date <- rda(multi2[,-c(1:4)] ~ observer + recording_date, data = multi_type)

  # Getting R2 for models
  firstmodel <- RsquareAdj(ordination)$adj.r.squared
  obsmodel <- RsquareAdj(ordination_obs)$adj.r.squared
  locmodel <- RsquareAdj(ordination_loc)$adj.r.squared
  datemodel <- RsquareAdj(ordination_date)$adj.r.squared
  obs_plus_locmodel <- RsquareAdj(ordination_obs_loc)$adj.r.squared
  recordingmodel <- RsquareAdj(ordination_recording)$adj.r.squared
  obs_plus_datemodel <- RsquareAdj(ordination_obs_date)$adj.r.squared

  # Print the results of a permutation test for the constrained ordination
  step <- ordistep(ordination, direction = "both")
  u <- ordination$CCA$u %>% as.data.frame()

  showvarparts(2, bg = c("hotpink","skyblue"))
  # Partioning the variance of the RDA
  mod <- varpart(multi2[,-c(1:4)] %>% as.data.frame(), as.factor(multi2$location), as.factor(multi2$observer), transfo="hel")
  ## Use fill colours
  plot(mod, bg = c("hotpink","skyblue"))
  # Alternative way of to conduct this partitioning

  # Set up the output - scores first
  ordination_scores <- scores(ordination_obs, display = "sites") %>%
    as.data.frame() %>%
    rownames_to_column("site") %>%
    bind_cols(., multi_type)

  # Then eigenvectors
  ordination_vect <- scores(ordination_obs, display = "species") %>%
    as.data.frame()

  # Now plot everything for the ordination
  plot_RDA <- ggplot(data = ordination_scores, aes(x = RDA1, y = RDA2)) +
    geom_point(data = ordination_scores, aes(x = RDA1, y = RDA2, colour = observer), alpha = 0.6) +
    stat_ellipse(data = ordination_scores, aes(colour = observer), linetype = 4, type = 'norm', level = 0.67) +
    geom_vline(xintercept = c(0), color = "#A19E99", linetype = 2) +
    geom_hline(yintercept = c(0), color = "#A19E99", linetype = 2) +
    geom_segment(data = ordination_vect, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text(data = ordination_vect, aes(x = RDA1, y = RDA2, label = rownames(ordination_vect))) +
    # Apply ABMI themes
    labs(x = paste0("RDA1 ", round(ordination$CA$eig[[1]],2), '%'),
         y = paste0("RDA2 ", round(ordination$CA$eig[[2]],2), '%'),
         title = paste0("RDA of observer detections constrained by recording")) +
    theme_bw() +
    guides(fill="none") +
    scale_colour_viridis_d()


  # Time to first detection data setup
  dd <- data %>%
    select(location, recording_date,
           observer, species_code, individual_appearance_order,
           tag_start_s, tag_duration_s,  min_tag_freq, max_tag_freq, vocalization, abundance) %>%
    distinct() %>%
    group_by(location, recording_date) %>%
    mutate(unique_times = n_distinct(observer)) %>%
    ungroup() %>%
    filter(unique_times >= 11) %>%
    select(-unique_times) %>%
    relocate(abundance, .after=individual_appearance_order) %>%
    mutate_at(vars(min_tag_freq, max_tag_freq), ~as.numeric(str_replace(.,"kHz",""))) %>%
    group_by(location, recording_date, observer) %>%
    mutate(detection_order = row_number()) %>%
    ungroup() %>%
    mutate(freq_diff = max_tag_freq - min_tag_freq) %>%
    mutate(Unknown = case_when(grepl('^U',species_code) ~ "Unknown", TRUE ~ "Species"))

  # Plot the results
  d <- ggplot(dd, aes(x=tag_start_s,y=detection_order,colour=Unknown,shape=Unknown)) +
    geom_point(alpha = 0.2) +
    geom_smooth(alpha = 0.6) +
    ylim(0,50) +
    facet_wrap(~observer, scales="free_x") +
    ggtitle("Time to first detection of species and unknown tags") +
    xlab("Tag start time (seconds)") + ylab("Detections per recording") +
    scale_colour_viridis_d() +
    theme_bw()

  #Return all objects
  return(list(plot_RDA, # Ordination
              d, # Time to first detection plot
              ordination, # Ordination
              ordination_scores %>% as_tibble(), # Ordination scores
              vegan::anova.cca(ordination, step = 1000, by = "term"), # Permutation Test for RDA (ANOVA-like)
              vegan::adonis(multi2[,-c(1:4)] ~ observer + location + recording_date, data = multi_type, distance = "jaccard"), # PERMANOVA
              vegan::adonis2(multi2[,-c(1:4)] ~ observer + location + recording_date, data = multi_type, distance = "jaccard")))
}

