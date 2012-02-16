library(ggplot2)
library(reshape)

setwd("~/Documents/stackexchange/")

df <- read.csv("./data/user_ratio.csv",
               header=TRUE
               )

df <- df[,c("country_code",
            "user_to_total_labor",
            "user_to_broad_labor",
            "user_to_narrow_labor",
            "user_to_patent")
         ]

names(df) <- c("country_code",
               "Users /\n workforce (\'000)",
               "Users /\n IT workforce (broad)",
               "Users /\n IT workforce (narrow)",
               "Users /\n IT patents"
               )
df <- melt(df, id.vars="country_code")


plot.ratios <- ggplot(df,
                      aes(x=toupper(country_code),
                          y=value)
                      ) +

  geom_point() +
  facet_grid(variable ~ ., scales="free") +
  scale_x_discrete("Country") +
  scale_y_continuous("User ratios") +
  opts(strip.text.y=theme_text(angle=0, size=6))
  
pdf("./figures/plot_user_ratios.pdf")
print(plot.ratios)
dev.off()
