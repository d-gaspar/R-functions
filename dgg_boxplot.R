
########################################################################
#                                                                      #
# AUTHOR: Daniel Gaspar Goncalves                                      #
# git: https://github.com/d-gaspar/                                    #
#                                                                      #
########################################################################

# library(ggplot2)
# library(grid)
# library(gridExtra)

########################################################################

dgg_boxplot = function(df, lab_x=NULL, lab_y=NULL, title=NULL, stat_test=c(FALSE, "wilcox"), stat_test_paired=FALSE, style=list(), style_number=c(1), help=FALSE){
    
    cat(paste(
        "dgg_boxplot",
        "   AUTHOR: Daniel Gaspar Goncalves",
        "   git: https://github.com/d-gaspar/",
        "\n",
        sep="\n"))
    
    ####################################################################
        
    # Error checking
    
    if(!(ncol(df) %in% c(2,3))){
        return("Error: 'df' must have 2 or 3 columns.")
    }
    
    if(length(stat_test)>1) stat_test = stat_test[1]
    
    ####################################################################
    
    # Pre-made styles
    
    pre_made_styles = data.frame(
        n = 1,
        panel.background = as.character("element_blank"),
        axis.text.x.angle = 45,
        axis.text.x.hjust = 1,
        scales = "fixed"
    )
    pre_made_styles$panel.background = as.character(pre_made_styles$panel.background)
    
    ####################################################################
    
    # ERRORS
    
    if(!(style_number %in% pre_made_styles$n)){
        return("Error: Style number doesn't exist")
    }
    
    ####################################################################
    
    # Rename colnames
    colnames(df)[1:(ncol(df)-1)] = paste0("V", 1:(ncol(df)-1))
    colnames(df)[ncol(df)] = "value"
    
    # set column 1 class to factor
    df$V1 = factor(as.character(df$V1))
    
    # set column 1 class to factor
    if(ncol(df)==3){
        df$V2 = factor(as.character(df$V2))
    }
    
    ####################################################################
    
    # Customizable styles
    
    if(!("panel.background" %in% names(style))) style$panel.background = pre_made_styles[style_number,"panel.background"]
    if(!("axis.text.x.angle" %in% names(style))) style$axis.text.x.angle = pre_made_styles[style_number,"axis.text.x.angle"]
    if(!("axis.text.x.hjust" %in% names(style))) style$axis.text.x.hjust = pre_made_styles[style_number,"axis.text.x.hjust"]
    if(!("scales" %in% names(style))) scales = pre_made_styles[style_number,"scales"]
    
    # Correct hjust when angle is zero
    if(style$axis.text.x.angle==0) style$axis.text.x.hjust = 0.5
    
    ####################################################################
    
    # Statistical test
    
    if(stat_test=="wilcox"){
        
        # generate all possibilities of comparison
        test = as.data.frame(t(combn(as.character(levels(df[,1])),2)))
        colnames(test) = c("A", "B")
        
        if(ncol(df)==3){
            test = test[rep(1:nrow(test), length(levels(df$V2))),]
            n_aux = length(levels(df$V1))
            test$V2 = rep(levels(df$V2), each=n_aux*(n_aux-1)/2)
        }
        
        # wilcox test
        suppressWarnings({
            test$p.value_greater = apply(test, 1, function(x) wilcox.test(x=df[df$V1==x[1],]$value, y=df[df$V1==x[2],]$value, paired=stat_test_paired,alternative="greater")$p.value)
            test$p.value_less = apply(test, 1, function(x) wilcox.test(x=df[df$V1==x[1],]$value, y=df[df$V1==x[2],]$value, paired=stat_test_paired, alternative="less")$p.value)
        })
        test$p.value = apply(test[,c("p.value_greater","p.value_less")], 1, min)
        test$p.value = ifelse(
            test$p.value < 0.001, "***", ifelse(
                test$p.value < 0.01, "**", ifelse(
                    test$p.value < 0.05, "*", "ns"
                )
            )
        )
        test$p.value = apply(test[,c("p.value_greater","p.value_less", "p.value")], 1, function(x){
            ifelse(as.numeric(x[1]) > as.numeric(x[2]), paste0("< (",x[3],")"), paste0("> (", x[3], ")"))
        })
        test$x_pos = apply(t(combn(length(levels(df$V1)), 2)), 1, mean)
        test$y_pos = max(df$value)
        test$y_pos = apply(test[,c("A", "B", "y_pos")], 1, function(x){
            as.numeric(x[3]) + (abs(which(levels(df$V1) %in% x[1]) - which(levels(df$V1) %in% x[2])) * 1.05)
        })
    }
    
    ####################################################################
    
    # plot
    
    # print(style)
    
    p = ggplot(df, aes(x=V1, y=value)) +
        geom_boxplot(color="black") +
        theme(
            axis.text.x = element_text(
                angle = style$axis.text.x.angle,
                hjust = style$axis.text.x.hjust,
                color = "black"
            ),
            axis.text.y = element_text(
                color = "black"
            ),
            strip.text = element_text(
                color = "black"
            ),
            strip.background = element_rect(
                fill = "lightgray"
            )
        ) +
        labs(
            title = title,
            x = lab_x,
            y = lab_y
        )
    
    # background
    if(style$panel.background=="element_blank"){
        p = p +
            theme(
                panel.background = element_blank()
            )
    }
    
    # wilcox test
    if(stat_test=="wilcox"){
        p = p + geom_text(
            data = test,
            aes(x=x_pos, y=y_pos, label=p.value)
        )
    }
    
    # 3 columns
    if(ncol(df)==3){
        p = p +
            facet_wrap(~ V2, scales="fixed")
    }
    
    ####################################################################
    
    return(p)
}
