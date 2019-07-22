

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
    
    if(!(help)){
        
        # Error checking
        
        if(!(ncol(df) %in% c(2,3))){
            return("Error: 'df' must have 2 or 3 columns.")
        }
        
        ################################################################
        
        # Pre-made styles
        
        pre_made_styles = data.frame(
            n = 1,
            panel.background = as.character("element_blank"),
            axis.text.x.angle = 45,
            axis.text.x.hjust = 1,
            scales = "fixed"
        )
        pre_made_styles$panel.background = as.character(pre_made_styles$panel.background)
        
        ################################################################
        
        # ERRORS
        
        if(!(style_number %in% pre_made_styles$n)){
            return("Error: Style number doesn't exist")
        }
        
        ################################################################
        
        # Rename colnames
        colnames(df)[1:(ncol(df)-1)] = paste0("V", 1:(ncol(df)-1))
        colnames(df)[ncol(df)] = "value"
        
        # set column 1 class to factor
        df$V1 = factor(as.character(df$V1))
        
        # set column 1 class to factor
        if(ncol(df)==3){
            df$V2 = factor(as.character(df$V2))
        }
        
        ################################################################
        
        # Customizable styles
        
        if(!("panel.background" %in% names(style))) style$panel.background = pre_made_styles[style_number,"panel.background"]
        if(!("axis.text.x.angle" %in% names(style))) style$axis.text.x.angle = pre_made_styles[style_number,"axis.text.x.angle"]
        if(!("axis.text.x.hjust" %in% names(style))) style$axis.text.x.hjust = pre_made_styles[style_number,"axis.text.x.hjust"]
        if(!("scales" %in% names(style))) scales = pre_made_styles[style_number,"scales"]
        
        # Correct hjust when angle is zero
        if(style$axis.text.x.angle==0) style$axis.text.x.hjust = 0.5
        
        ################################################################
        
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
        
        ################################################################
        
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
        
        ################################################################
        
        return(p)
        
    } else {
        
        # HELP
        
        cat(paste(
            "dgg_boxplot - Help",
            "   AUTHOR: Daniel Gaspar Goncalves",
            "   git: https://github.com/d-gaspar/",
            "\n",
            "libraries required:",
            "   ggplot2",
            "   grid",
            "   gridExtra",
            "\n",
            "The input file must be a 'data.frame' with 2 or 3 columns, where the last column",
            "must have the values (numeric class).",
            "\n",
            "Help PDF file with examples saved as 'dgg_boxplot.pdf'",
            "\n",
            sep="\n"))
        
        # generate data.frame example
        df = data.frame(
            treatment = paste0("treatment ", rep(1:3,20)),
            time = paste0("time ", rep(1:2, 10, each=3)),
            value = 0
        )
        for(i in 0:2){
            set.seed(42)
            df[1:nrow(df) %% 3 == i,]$value = sample(seq(0.1+i*2,10+i*2,0.1), size=20, replace=TRUE)
        }
        
        p1 = tableGrob(
            head(df[,c(1,3)], n=5),
            rows = NULL
        )
        
        p2 = ggplot(df, aes(x=treatment, y=value)) +
            geom_boxplot() +
            theme(
                panel.background = element_blank(),
                axis.text.x = element_text(angle=45, hjust=1)
            )
        
        p3 = tableGrob(
            head(df, n=5),
            rows = NULL
        )
        
        p4 = ggplot(df, aes(x=treatment, y=value)) +
            geom_boxplot() +
            theme(
                panel.background = element_blank(),
                axis.text.x = element_text(angle=45, hjust=1)
            ) +
            facet_wrap(~ time, scales="fixed")
        
        # plot help pdf
        graphics.off()
        cairo_pdf(filename="dgg_boxplot.pdf", width=8.3, height=11.7)
        grid.arrange(
            layout_matrix = rbind(
                matrix(rep(c(1,1,1,1), 2), nrow=2, byrow=TRUE),
                matrix(rep(c(2,2,3,3), 4), nrow=4, byrow=TRUE),
                matrix(rep(c(4,4,5,5), 4), nrow=4, byrow=TRUE)
            ),
            top=textGrob("dgg_boxplot - Help", gp=gpar(fontsize=20, fontface="bold")),
            textGrob(paste(
                "AUTHOR: Daniel Gaspar Goncalves",
                "git: https://github.com/d-gaspar/",
                sep="\n"), gp=gpar(fontsize=10), vjust=0.5),
            arrangeGrob(
                p1,
                top=textGrob("Input example", gp=gpar(fontsize=15, fontface="bold")),
                left=textGrob("2 columns\nStyle 1", rot=90, gp=gpar(fontsize=15, fontface="bold"))
            ),
            arrangeGrob(
                p2,
                top=textGrob("Output graphs", gp=gpar(fontsize=15, fontface="bold"))
            ),
            arrangeGrob(
                p3,
                left=textGrob("3 columns\nStyle 1", rot=90, gp=gpar(fontsize=15, fontface="bold"))
            ),
            arrangeGrob(
                p4
            )#,
            # arrangeGrob(
            #     p3
            # ),
            # arrangeGrob(
            #     p4
            # ),
            # arrangeGrob(
            #     p3
            # ),
            # arrangeGrob(
            #     p4
            # )
        )
        invisible(dev.off())
    }
}