
########################################################################
#                                                                      #
# AUTHOR: Daniel Gaspar Goncalves                                      #
# git: https://github.com/d-gaspar/                                    #
#                                                                      #
########################################################################

# library(reshape2)
# library(ggplot2)

########################################################################

dgg_linegraph = function(df, features_X, features_Y, samples_A, samples_B, lab_x=NULL, lab_y=NULL, title=NULL, legend=TRUE, legend.title="variable", legend.labels=c("samples_A", "samples_B"), style=list(), style_number=c(1)){
    
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
        tickslab_size = 15,
        lab_size = 20
    )
    
    ####################################################################
    
    # ERRORS
    
    if(!(style_number %in% pre_made_styles$n)){
        return("Error: Style number doesn't exist")
    }
    
    ####################################################################
    
    df = df[c(features_X, features_Y), c(samples_A, samples_B)]
    
    ####################################################################
    
    # Customizable styles
    
    if(!("tickslab_size" %in% names(style))) style$tickslab_size = pre_made_styles[style_number,"tickslab_size"]
    if(!("lab_size" %in% names(style))) style$lab_size = pre_made_styles[style_number,"lab_size"]
    
    ####################################################################
    
    # create data.frame
    
    df_mean = cbind(
        data.frame(row.names=rownames(df), samples_A=apply(df[, samples_A], 1, mean)),
        data.frame(row.names=rownames(df), samples_B=apply(df[, samples_B], 1, mean))
    )
    df_mean$features = rownames(df_mean)
    df_mean$median_samples_A = c(
        rep(
            median(df_mean[features_X,]$samples_A),
            length(features_X)
        ),
        rep(
            median(df_mean[features_Y,]$samples_A),
            length(features_Y)
        )
    )
    df_mean$median_samples_B = c(
        rep(
            median(df_mean[features_X,]$samples_B),
            length(features_X)
        ),
        rep(
            median(df_mean[features_Y,]$samples_B),
            length(features_Y)
        )
    )
    
    df_mean_point = melt(df_mean[,c("features", "samples_A", "samples_B")], id.vars=c("features"), measure.vars=c("samples_A", "samples_B"))
    df_mean_point$features = factor(as.character(df_mean_point$features), levels=c(features_X, features_Y))
    
    df_mean_line = melt(df_mean[,c("features", "median_samples_A", "median_samples_B")], id.vars=c("features"), measure.vars=c("median_samples_A", "median_samples_B"))
    df_mean_line$features = factor(as.character(df_mean_line$features), levels=c(features_X, features_Y))
    df_mean_line$variable = factor(gsub("median_", "", as.character(df_mean_line$variable)), levels=c("samples_A", "samples_B"))
    
    ####################################################################
    
    # PLOT
    
    p = ggplot() +
        geom_point(data=df_mean_point, mapping=aes(x=as.integer(df_mean_point$features), y=value, color=variable)) +
        geom_line(data=df_mean_line, mapping=aes(x=as.integer(df_mean_point$features), y=value, color=variable), size=1.5) +
        theme(
            text = element_text(face="bold", size=style$lab_size),
            panel.background = NULL,
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size=style$tickslab_size)
        ) +
        labs(
            title = title,
            x = lab_x,
            y = lab_y
        )
    
    if(legend){
        p = p +
            labs(
                color = legend.title
            ) +
            scale_color_manual(
                values = c("#E41A1C", "#377EB8"),
                labels = legend.labels
            )
    } else {
        p = p +
            theme(
                legend.position = "none"
            ) +
            scale_color_manual(
                values = c("#E41A1C", "#377EB8")
            )
    }
    
    return(p)
}