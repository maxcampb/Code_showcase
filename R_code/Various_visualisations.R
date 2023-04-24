
# =======================================================================
# Author: Max D Campbell
#
# This script contains some examples of copy pasted code from my previous 
# projects (written by myself). This showcase script is not designed to run. 
# Some of the major skills demonstrated here are: 
# ggplot, statistical summary plots, plotly
# 
# Feel free to contact me with any queries or for more information.
# ========================================================================


# Spatial Bubble plot from Campbell et al. (2021) -----------------------------------

# Check for spatial auto-correllation
autocorData <- data.frame(Lon = CopeData$Lon, 
                          Lat = CopeData$Lat, 
                          resids = resid(hr_mod)) %>%  # Extract residuals of GLMM
  within({
    signres <- sign(resids)
  })


## Plot the residuals in space
world <- map_data("world")

# Change longitude so it matches up with the world map
autocorData$Lon[autocorData$Lon < (-170)] <- autocorData$Lon[autocorData$Lon < (-170)] + 360

# Bubble plot WITH random effects
ggplot(data = autocorData, aes(x = Lon, y = Lat)) + 
  geom_map(data=world, map=world, aes(x = long, y = lat, map_id=region),
           color="white", fill="gray94", size=0.08) + 
  geom_point(aes(size = abs(resids), color = sign(resids)), shape = 1,
             alpha = 0.4) + 
  scale_size_continuous(range=c(.1,4)) + 
  scale_colour_gradient(low = "springgreen3", high = "magenta3") + 
  ylab(NULL) + xlab(NULL) + 
  annotate("text", x = -190, y = 90, label = "(b)", size = 9) +
  guides(colour = "none", size = guide_legend(title = "Magnitude"))



# Model effect plot from Campbell et al. (2021)  ----------------------------

# Make data that spans the SST range
newdat <- with(CopeData, data.frame( SST = seq(min(SST), max(SST), length.out = 100),
                                     Sqrt_chl  = mean(Sqrt_chl),
                                     Asin_omni = mean(Asin_omni)))

# Make predictions
y_pred <- predict(hr_mod,re.form=NA,newdata=newdat,type="response")
# Estimate the confidence intervals using Ben Bolker's CI 
# function (based on uncertainty in fixed effects only)
conf_int <- easyPredCI(hr_mod,newdata = newdat)

# Bind data and predictions
newdat <- cbind(newdat, y_pred, conf_int)

# Specify parameters of plotting
annotate_sz <- 14
point_sz <- 2.4
line_sz <- 2
error_sz <- 1.3

# Make effect plot
ggplot(data = CopeData, mapping = aes(x = SST, y = fitted(hr_mod))) + 
  geom_ribbon(data = newdat, mapping = aes( y = y_pred,  ymin = conf.low, ymax =conf.high,
                                            x = SST),  alpha = .9, fill = "grey80") +
  geom_point(alpha = 0.2, size = point_sz) +
  geom_line(data = newdat, aes(x = SST, y = y_pred), col = "dodgerblue", size = line_sz) +
  geom_line(data = newdat, aes(x = SST, y = conf.low), col = "dodgerblue", 
            size = error_sz, lty = "dashed") +
  geom_line(data = newdat, aes(x = SST, y = conf.high), col = "dodgerblue", 
            size = error_sz, lty = "dashed") +
  theme_bw() + annotate("text", x = 0, y = 6, label = "(d)", size = annotate_sz) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=13), 
        panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank()) + xlab("SST (°C)") + 
  ylab("Mean length of copepod (mm)") +
  scale_x_continuous(breaks = seq(-0, 30, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) 



# Plotly used to visualise a timeline of website and linkedin data --------


# This function is used in the ploty to create horizontal lines
hline <- function(y = 0, color = "black", line_type = 'dot') {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash = line_type)
  )
}

# Reformat LinkedIn visers and website vistor data so they are in longer format
timeseries_subdat <- timeseries_dat %>% 
  select(Date, LinkedIn = `Overview unique visitors (total)`, 
         Website = `Website_Total users`) %>%
  pivot_longer(-Date, names_to = 'Platform', values_to = 'Users')

# Create interactive plotly of website users and linkedin visitors over time
fig <- plot_ly(timeseries_subdat, x = ~ Date, y = ~ Users, type = "scatter",
               color = ~ Platform, mode = 'lines', colors = pallet[c(1,3)]) %>% 
  # Add point markers which show details of post and actions data made in the given month 
  add_markers(color = NA,
              x = c(actions_dat$Date), 
              # Spread out post points so they don't overlap
              y = c(seq(from = max(timeserisubdat$Users)/2, 
                        to = max(timeseries_subdat$Users),
                        length.out = nrow(actions_dat))),
              # Generate strings to view when hovering over markers
              text = c(
                paste(substr(actions_dat$Action, start = 1, stop = 35), '... \n',
                      actions_dat$Date,
                      ifelse(is.na(actions_dat$Impressions), '',  
                             paste('\n', actions_dat$Impressions, 'impressions \n',
                                   actions_dat$Clicks, 'clicks \n',
                                   actions_dat$Likes, 'likes')))),
              hoverinfo = 'text',
              marker = list(color=pallet[[5]]),
              showlegend = FALSE
  ) %>% 
  # Add markers to describe hlines for the website 
  add_trace(mode = 'markers', color = 'Website',
            # Locations of website baselines
            y = quantile(web_users_by_date$`Total users`,
                         probs = c(0.05, 0.5, 0.95)),
            x = rep(max(timeseries_subdat$Date), 3),
            marker = list(size = 6, symbol = 2, 
                          color = es_pallet[[4]]),
            showlegend = FALSE,
            textposition = "auto",
            hoverinfo = "text",
            # Generate strings to view when hovering over markers
            hovertext = paste(c('5th percentile', 'median', '95th percentile'),
                              '(Website) -', 
                              round(quantile(web_users_by_date$`Total users`,
                                             probs = c(0.05, 0.5, 0.95)), 
                                    digits = 1))
  ) %>% 
  # Add markers to describe hlines for the Linkedin 
  add_trace(mode = 'markers', color = 'LinkedIn',
            # Locations of linkedin baselines
            y = quantile(linkedin_users_by_date$Users,
                         probs = c(0.05, 0.5, 0.95)),
            x = rep(max(timeseries_subdat$Date), 3),
            marker = list(size = 6, symbol = 2,
                          color = pallet[[2]]),
            showlegend = FALSE,
            textposition = "auto",
            hoverinfo = "text",
            # Generate strings to view when hovering over markers
            hovertext = paste(c('5th percentile', 'median', '95th percentile'),
                              '(LinkedIn) -', 
                              round(quantile(linkedin_users_by_date$Users,
                                             probs = c(0.05, 0.5, 0.95)), 
                                    digits = 1))
  ) %>% 
  # Add hlines which give the baseline data of the website and linkedin
  layout(shapes = list(
    hline(quantile(web_users_by_date$`Total users`, probs = 0.05), color = pallet[[4]]),
    hline(quantile(web_users_by_date$`Total users`, probs = 0.50), color = pallet[[4]],
          line_type = 'solid'),
    hline(quantile(web_users_by_date$`Total users`, probs = 0.95), color = pallet[[4]]),
    hline(quantile(linkedin_users_by_date$Users, probs = 0.05), color = pallet[[2]]),
    hline(quantile(linkedin_users_by_date$Users, probs = 0.50), color = pallet[[2]],
          line_type = 'solid'),
    hline(quantile(linkedin_users_by_date$Users, probs = 0.95), color = pallet[[2]])))

# Plot the figure
fig
