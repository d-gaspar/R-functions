
########################################################################
#                                                                      #
# AUTHOR: Daniel Gaspar Goncalves                                      #
# git: https://github.com/d-gaspar/                                    #
#                                                                      #
########################################################################

# library(survival)
# library(survminer)

########################################################################

dgg_survival = function(df, lab_x="Time (months)", lab_y="Survival probability", title=NULL, style=list(), style_number=c(1), show_n=TRUE, prop_test=FALSE, prop_test_cut=36){
    
    cat(paste(
        "dgg_boxplot",
        "   AUTHOR: Daniel Gaspar Goncalves",
        "   git: https://github.com/d-gaspar/",
        "\n",
        sep="\n"))
    
    ####################################################################
    
    # Pre-made styles
    
    pre_made_styles = data.frame(
        n = 1,
        line_size = 2,
        censor_size = 4,
        main_size = 30,
        font_x = 30,
        font_y = 30,
        tickslab_size = 15,
        palette = "Set1",
        break_time = 12,
        linetype = 1,
        median_line = "none",
        legend_size = 15
    )
    
    ####################################################################
    
    # ERRORS
    
    if(ncol(df) != 3){
        return("Error: 'df' must have 3 columns.")
    }
    
    if(!(style_number %in% pre_made_styles$n)){
        return("Error: Style number doesn't exist")
    }
    
    ####################################################################
    
    # Rename colnames
    colnames(df) = c("time", "censor", "group")
    
    # set column 3 class to factor
    df$group = factor(as.character(df$group))
    
    # show N
    if(show_n){
        df$group = as.factor(sapply(df$group, function(x) paste0(x, " [",as.numeric(table(df$group)[x]), "]")))
    }
    
    # groups
    groups = levels(df$group)
    
    ####################################################################
    
    # Customizable styles
    
    if(!("line_size" %in% names(style))) style$line_size = pre_made_styles[style_number,"line_size"]
    if(!("censor_size" %in% names(style))) style$censor_size = pre_made_styles[style_number,"censor_size"]
    if(!("main_size" %in% names(style))) style$main_size = pre_made_styles[style_number,"main_size"]
    if(!("font_x" %in% names(style))) style$font_x = pre_made_styles[style_number,"font_x"]
    if(!("font_y" %in% names(style))) style$font_y = pre_made_styles[style_number,"font_y"]
    if(!("tickslab_size" %in% names(style))) style$tickslab_size = pre_made_styles[style_number,"tickslab_size"]
    if(!("palette" %in% names(style))) style$palette = pre_made_styles[style_number,"palette"]
    if(!("break_time" %in% names(style))) style$break_time = pre_made_styles[style_number,"break_time"]
    if(!("linetype" %in% names(style))) style$linetype = pre_made_styles[style_number,"linetype"]
    if(!("median_line" %in% names(style))) style$median_line = pre_made_styles[style_number,"median_line"]
    if(!("legend_size" %in% names(style))) style$legend_size = pre_made_styles[style_number,"legend_size"]

    ####################################################################
    
    # SURV
    
    FitAll = survfit(Surv(time, censor) ~ group, data=df)
    sdf = survdiff(Surv(time, censor) ~ as.factor(group), data=df)
    
    # Log-rank
    pval_log_rank = round(1 - pchisq(sdf$chisq, length(sdf$n) - 1), 5)
    if(pval_log_rank==0) pval_log_rank = "<0,00001"
    pval = paste0(
        "Log-rank: ", pval_log_rank
    )
    
    # PROPORTION TEST
    if(prop_test){
        
        prop_test_df = matrix(0, 2, length(groups))
        for(i in 1:length(groups)){
            prop_test_df[1,i] = sum(df$group==groups[i] & df$time>prop_test_cut)
            prop_test_df[2,i] = sum(df$group==groups[i] & df$time<=prop_test_cut & df$censor==1)
        }
        pval_prop_test = round(fisher.test(prop_test_df)$p.value, 5)
        if(pval_prop_test==0) pval_prop_test = "<0,00001"
        
        pval = paste0(
            pval,
            "\n",
            "prop.test: ", pval_prop_test
        )
    }
    
    ####################################################################
    
    # PLOT
    
    p = ggsurvplot(
        FitAll,
        data = df,
        legend.labs = groups,
        size = style$line_size,
        censor.size = style$censor_size,
        censor.shape = "|",
        pval = pval,
        conf.int = FALSE,
        risk.table.y.text = FALSE,
        legend.title = "",
        font.main = c(style$main_size, "bold"),
        font.x = style$font_x,
        font.y = style$font_y,
        font.tickslab = style$tickslab_size,
        palette = style$palette,
        alpha = 0.7,
        break.x.by = style$break_time,
        surv.median.line = as.character(style$median_line),
        linetype = style$linetype
    )
    
    suppressMessages({
        p = p$plot +
            labs(
                x = lab_x,
                y = lab_y,
                title = title
            ) +
            coord_cartesian(
                xlim = c(0, max(df$time, na.rm=T)*1.1),
                expand = FALSE
            ) +
            scale_y_continuous(labels=scales::percent) +
        theme(
            legend.text = element_text(
                size = style$legend_size,
                color = "black",
                face = "bold"),
            axis.text.x = element_text(
                angle = 45,
                hjust = 1)
        )
    })
    
    return(p)
}
