# CFAR Social Network Code - Color Schemes

# Author: Max Schultz
# Description: Coloring schemes for specific SN plots... to be sourced in the main code so as to not take up too much space.




color_schemes <- function(graph, data, global_code, 
                          scheme = c("basic", "contact_age", "igra_results", "hiv_exposure", "hh_id", 
                                     "household_info", "household_info_shading", "hh_act")){
    scheme = match.arg(scheme)

    # relevant data for cleanliness
    contact_age = data$snml_vage_mrg
    contact_id = data$contact_id
    index_id = data$record_id
    index_age = data$enrol_age_yrs_mrg
    lived = data$lived_mrg
    stayed = data$stayed_mrg
    visited = data$visited_mrg
    occas = data$occas_mrg
    elswe = data$elswe_mrg
    igra = data$final_igra_results
    hiv = data$cherish_sn_study_group_mrg
    hh = data$hh_id_unified_

    # default attributes 
    basic_color = global_code$palettes$basic_network
    V(graph)$plot_color = unname(basic_color[ifelse(V(graph)$type == "index", "index", "contact")]) 
    legend_labels = c("Index", "Contact")
    legend_colors = unname(basic_color[c("index", "contact")])

    # basic plot
    if (scheme == "basic"){
        return(list(graph = graph, legend = list(labels = legend_labels, colors = legend_colors)))
    }


    # contact age plot 
    if (scheme == "contact_age"){
        label_by_level = setNames(global_code$contact_age_labels, global_code$contact_age_levels)
        palette = global_code$palettes$contact_age_network
        level = contact_age
        names(level) = contact_id
        contacts = which(V(graph)$type == "contact")

        if (length(contacts) > 0){
        level_here = level[V(graph)$name[contacts]]
        not_null = !is.na(level_here)
        V(graph)$plot_color[contacts[not_null]] = palette[label_by_level[as.character(level_here[not_null])]]
        V(graph)$plot_color[contacts[!not_null]] = "white"
        }

        legend_labels = c("Index", paste("Contact:", names(palette)), "Contact: NA")
        legend_colors = c(basic_color["index"], unname(palette), "white")
        return(list(graph = graph, legend = list(labels = legend_labels, colors = legend_colors)))
    }
    

    # igra results (index-level)
    if (scheme == "igra_results"){
        palette = global_code$palettes$igra_network
        names(igra) = index_id
        index = which(V(graph)$type == "index")

        if (length(index) > 0){
        vals <- igra[V(graph)$name[index]]
        non_null <- !is.na(vals) & vals %in% names(palette)
        V(graph)$plot_color[index[non_null]] <- palette[vals[non_null]]
        }
        legend_labels <- c(paste0("Index IGRA=", names(palette)), "Contact")
        legend_colors <- c(unname(palette), basic_color["contact"])
        return(list(graph=graph, legend=list(labels=legend_labels, colors=legend_colors)))
    }


    # hiv exposure (index-level)
    if (scheme == "hiv_exposure"){
        palette = global_code$palettes$hiv_network
        names(hiv) = index_id
        index = which(V(graph)$type == "index")

        if (length(index) > 0){
        vals <- hiv[V(graph)$name[index]]
        non_null <- !is.na(vals) & vals %in% names(palette)
        V(graph)$plot_color[index[non_null]] <- palette[vals[non_null]]
        }
        legend_labels <- c(paste0("Index HIV=", names(palette)), "Contact")
        legend_colors <- c(unname(palette), basic_color["contact"])
        return(list(graph=graph, legend=list(labels=legend_labels, colors=legend_colors)))
    }



    # household information 
    if (scheme == "household_info"){
        palette = global_code$palettes$hh_info_network
        names(lived) = contact_id
        names(stayed) = contact_id
        names(visited) = contact_id
        names(occas) = contact_id
        names(elswe) = contact_id
        contacts = which(V(graph)$type == "contact")

        if (length(contacts) > 0){
        vals = V(graph)$name[contacts]
        cat = rep(NA_character_, length(vals))
        cat[lived[vals] == 1] = "lived"
        cat[stayed[vals] == 1] = "stayed"
        cat[occas[vals] == 1] = "occas"
        cat[elswe[vals] == 1] = "elswe"
        cat[visited[vals] == 1] = "visited"

        not_null = !is.na(cat)
        V(graph)$plot_color[contacts[not_null]] = unname(palette[cat[not_null]])
        V(graph)$plot_color[contacts[!not_null]] = "white"
        }
        legend_labels <- c("Index", paste0("Contact: ", names(palette)), "Contact: NA")
        legend_colors <- c(basic_color["index"], unname(palette), "white")
        return(list(graph = graph, legend = list(labels = legend_labels, colors = legend_colors)))
    }


    # household area shading 
    if (scheme == "hh_id"){
        alpha = global_code$alpha
        pal0 <- global_code$palettes$hh_id_network
        pal_keys <- setdiff(names(pal0), "NA")
        hh_map <- c(
        setNames(adjustcolor(unname(pal0[pal_keys]), alpha.f = alpha), pal_keys),
        "NA" = rgb(0.5, 0.5, 0.5, alpha)
        )

        three_col <- data %>%
        dplyr::distinct(record_id, contact_id, hh_id_unified_) %>%
        dplyr::filter(!is.na(contact_id))

        groups <- list()
        cols <- character()

        for (ego in unique(three_col$record_id)) {
            rows <- three_col[three_col$record_id == ego, , drop = FALSE]
            hh_key <- ifelse(is.na(rows$hh_id_unified_), "NA", as.character(rows$hh_id_unified_))
            spl <- split(rows$contact_id, hh_key)

            for (k in names(spl)) {
                ids <- unique(spl[[k]])
                vtx <- which(igraph::V(graph)$name %in% ids)
                if (!length(vtx)) next
                groups[[if (k == "NA") paste0("Ego", ego, "_NoHH") else paste0("Ego", ego, "_HH", k)]] <- vtx
                cols <- c(cols, if (k %in% names(hh_map)) unname(hh_map[k]) else unname(hh_map["NA"]))
            }
        }

        legend_labels <- c("Index", "Contact", paste0("Household ", pal_keys), "Household NA")
        legend_colors <- c(basic_color["index"], basic_color["contact"], unname(hh_map[pal_keys]), unname(hh_map["NA"]))

        return(list(groups = groups, cols = cols, legend = list(labels = legend_labels, colors = legend_colors)))
    }

    if (scheme == "household_info_shading"){
        alpha = global_code$alpha
        pal0 = global_code$palettes$hh_info_network
        pal_keys = names(pal0)

        info_map = c(
            setNames(adjustcolor(unname(pal0[pal_keys]), alpha.f = alpha), pal_keys),
            "NA" = rgb(0.5, 0.5, 0.5, alpha)
        )
        three_col = data %>%
            dplyr::distinct(record_id, contact_id, lived_mrg, stayed_mrg, occas_mrg, elswe_mrg, visited_mrg) %>%
            dplyr::filter(!is.na(contact_id))
        
        groups = list()
        cols = character()

        for (ego in unique(three_col$record_id)){
            rows = three_col[three_col$record_id == ego, , drop = FALSE]
            ids = rows$contact_id
            key <- rep("NA", length(ids))
            key[rows$lived_mrg == 1] <- "lived"
            key[rows$stayed_mrg == 1] <- "stayed"
            key[rows$occas_mrg == 1] <- "occas"
            key[rows$elswe_mrg == 1] <- "elswe"
            key[rows$visited_mrg == 1] <- "visited"
            spl <- split(ids, key)
            for (k in names(spl)) {
                vtx = which(V(graph)$name %in% unique(spl[[k]]))
                if (!length(vtx)) next
                groups[[if (k == "NA") paste0("Ego", ego, "_InfoNA") else paste0("Ego", ego, "_Info_", k)]] <- vtx
                cols <- c(cols, if (k %in% names(info_map)) unname(info_map[k]) else unname(info_map["NA"]))
            }
        }
        legend_labels <- c("Index", "Contact", paste0("Contact: ", pal_keys), "Contact: NA")
        legend_colors <- c(basic_color["index"], basic_color["contact"], unname(info_map[pal_keys]), unname(info_map["NA"]))

        return(list(groups = groups, cols = cols, legend = list(labels = legend_labels, colors = legend_colors)))

    }
    
    
    if (scheme == "hh_act"){
      alpha = global_code$alpha
      pal0 <- global_code$palettes$hh_act_network
      pal_keys <- setdiff(names(pal0), "NA")
      
      act_map <- c(
        setNames(adjustcolor(unname(pal0[pal_keys]), alpha.f = alpha), pal_keys),
        "NA" = rgb(0.5, 0.5, 0.5, alpha)
      )
      
      three_col <- data %>%
        dplyr::distinct(record_id, contact_id, h_act_mrg) %>%
        dplyr::filter(!is.na(contact_id))
      
      groups <- list()
      cols <- character()
      
      for (ego in unique(three_col$record_id)) {
        rows <- three_col[three_col$record_id == ego, , drop = FALSE]
        act_key <- ifelse(is.na(rows$h_act_mrg), "NA", as.character(rows$h_act_mrg))
        spl <- split(rows$contact_id, act_key)
        
        for (k in names(spl)) {
          ids <- unique(spl[[k]])
          vtx <- which(igraph::V(graph)$name %in% ids)
          if (!length(vtx)) next
          
          groups[[if (k == "NA") paste0("Ego", ego, "_ActNA") else paste0("Ego", ego, "_Act", k)]] <- vtx
          cols <- c(cols, if (k %in% names(act_map)) unname(act_map[k]) else unname(act_map["NA"]))
        }
      }
      
      legend_labels <- c("Index", "Contact", paste0("In Household: ", pal_keys), "In Household NA")
      legend_colors <- c(
        basic_color["index"],
        basic_color["contact"],
        unname(act_map[pal_keys]),
        unname(act_map["NA"])
      )
      
      return(list(groups = groups, cols = cols, legend = list(labels = legend_labels, colors = legend_colors)))
    }

}
