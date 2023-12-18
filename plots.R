library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)

## highest alpha correlation is 0.3
df <- list.files(path='./data/aself/', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/aself/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- uself[0:50]


df_self <- df %>% 
  mutate(x=4*x-2) %>% 
  pivot_longer(cols=-12, names_to='alpha', values_to = 'y')

p1 <- ggplot(df_self, aes(x=x, y=y, col = as.numeric(alpha), group=alpha)) + 
  theme_minimal() +
  geom_point(alpha=0.3) + 
  geom_line(stat="smooth", method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
  labs(title = "",
       x = expression(U[self]),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col = expression(A[x])) +
  scale_color_viridis(discrete=FALSE, option='plasma', guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p1

## asocial 1.0 (tentatively)
df <- list.files(path='./data/asocial', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/asocial/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- wrongness[0:50]


df_social <- df %>% 
  mutate(x=x) %>% 
  pivot_longer(cols=-12, names_to='alpha', values_to = 'y')

p2 <- ggplot(df_social, aes(x=x, y=y, col = as.numeric(alpha), group=alpha)) + 
  geom_point(alpha=0.3) + 
  geom_line(stat="smooth", method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
  labs(title = "",
       x = expression(Wrongness),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col = expression(A[social])) + 
  theme_minimal() + 
  scale_color_viridis(discrete=FALSE, option='plasma', guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p2

df <- list.files(path='./data/asocial_wrongness_utarget/', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/asocial_wrongness_utarget/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- wrongness[0:50]


df_social <- df %>% 
  pivot_longer(cols=-51, names_to='alpha', values_to = 'y') %>% 
  mutate(alpha=as.numeric(alpha)/49)

p3 <- ggplot(df_social, aes(x=x, y=y, col=as.numeric(alpha), group=alpha)) + 
  geom_point(alpha=0.1) + 
  geom_line(stat='smooth', method='loess', alpha=0.3) + 
  labs(
    title = "",
    x = expression(Wrongness),
    y = expression(paste("Policy (", P[punish], ")", sep = "")),
    col = expression(U[target])) + 
  theme_minimal() + 
  scale_color_viridis(discrete=FALSE, option='plasma', guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p3

df <- list.files(path='./data/asocial_utarget_wrongness/', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/asocial_utarget_wrongness/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- utarget[0:50]


df_social <- df %>% 
  pivot_longer(cols=-51, names_to='alpha', values_to = 'y') %>%
  mutate(alpha = as.numeric(alpha)/49)

p4 <- ggplot(df_social, aes(x=x, y=y, col = as.numeric(alpha), group=alpha)) + 
  geom_point(alpha=0.1) + 
  geom_line(stat='smooth', method='loess', alpha=0.3) + 
  labs(
    title = "",
    x = expression(U[target]),
    y = expression(paste("Policy (", P[punish], ")", sep = "")),
    col = expression(Wrongness)) + 
  theme_minimal() + 
  scale_color_viridis(discrete=FALSE, option='plasma', guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p4

#########################
df <- list.files(path='./data/atarget/', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/atarget/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- utarget[0:50]


df_target <- df %>% 
  mutate(x=4*x-2) %>% 
  pivot_longer(cols=-12, names_to='alpha', values_to = 'y')

p5 <- ggplot(df_target, aes(x=x, y=y, col = as.numeric(alpha), group=alpha)) + 
  geom_point(alpha=0.3) + 
  geom_line(stat="smooth", method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
  labs(title = "",
       x = expression(U[target]),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col = expression(A[target])) + 
  theme_minimal() + 
  scale_color_viridis(discrete=FALSE, option='plasma', guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p5

#########################
df <- list.files(path='./data/arep/', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/arep/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- utarget[0:50]


df_rep <- df %>% 
  mutate(x=4*x-2) %>% 
  pivot_longer(cols=-12, names_to='alpha', values_to = 'y')

p6 <- ggplot(df_rep, aes(x=x, y=y,col = as.numeric(alpha), group=alpha)) + 
  geom_point(alpha=0.3) + 
  geom_line(stat="smooth", method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
  labs(title = "",
       x = expression(U[self]),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col = expression(A[rep])) + 
  theme_minimal() + 
  scale_color_viridis(discrete=FALSE, option='plasma', guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p6

#########################
df <- list.files(path='./data/self_rep/', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/self_rep/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- uself[0:50]


df_rep <- df %>% 
  mutate(x=4*x-2) %>% 
  pivot_longer(cols=-12, names_to='alpha', values_to = 'y')

p7 <- ggplot(df_rep, aes(x=x, y=y, col=alpha)) + 
  geom_point(alpha=0.3) + 
  geom_line(stat="smooth", method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
  labs(title = "",
       x = expression(U[self]),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col = expression(A[self]-A[rep])) + 
  theme_minimal() + 
  scale_color_viridis(discrete=TRUE, option='plasma')#, guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p7

#########################
df <- list.files(path='./data/rep_rest/', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/rep_rest/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- uself[0:50]


df_rep <- df %>% 
  mutate(x=4*x-2) %>% 
  pivot_longer(cols=-12, names_to='alpha', values_to = 'y')

p8 <- ggplot(df_rep, aes(x=x, y=y, col=as.numeric(alpha), group=alpha)) + 
  geom_point(alpha=0.3) + 
  geom_line(stat="smooth", method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
  labs(title = "",
       x = expression(U[self]),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col = expression(A[rep])) + 
  theme_minimal() + 
  scale_color_viridis(discrete=FALSE, option='plasma', guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p8

#########################
df <- list.files(path='./data/rep_rest_wrongness/', full.names=TRUE) %>% 
  map_df(~read_csv(., show_col_types=FALSE, col_names=FALSE)) %>% t() %>% as.data.frame()
file_names <- tools::file_path_sans_ext(basename(list.files(path='./data/rep_rest_wrongness/', full.names=TRUE)))
colnames(df) <- file_names

df['x'] <- wrongness[0:50]


df_rep <- df %>% 
  mutate(x=x) %>% 
  pivot_longer(cols=-12, names_to='alpha', values_to = 'y')

p9 <- ggplot(df_rep, aes(x=x, y=y, col=as.numeric(alpha), group=alpha)) + 
  geom_point(alpha=0.3) + 
  geom_line(stat="smooth", method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
  labs(title = "",
       x = expression(Wrongness),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col = expression(A[rep])) + 
  theme_minimal() + 
  scale_color_viridis(discrete=FALSE, option='plasma', guide=guide_colorbar(ticks=FALSE), limits=c(0, 1), breaks=c(0, 0.5, 1))
p9

df2 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/uself', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>% 
  {
    # Add column names (s1, s2, s3, etc.)
    column_names <- paste0("s", 1:ncol(.))
    names(.) <- column_names
    .
  } %>%
  cbind(uself[0:50]) %>% 
  rename_with(~'x', 7) %>%
  pivot_longer(cols=-7, names_to='subj', values_to='y') %>% 
  mutate(y=(y-1)/6, x=4*x-2)


dat1 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/uself', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>%
  rowMeans() %>% 
  as.data.frame() %>% 
  rename_with(~'y', 1) %>%
  transmute((y-1)/6) %>%
  rename_with(~'avg', 1)
dat1['x'] <- uself[0:50]
dat1 <- dat1 %>%
  mutate(x=4*x-2)

#cor(dat1[1], df)



colnames(df2) <- c('x', 'subj', 'y')
p10 <- ggplot(df2, aes(x=x, y=y)) + 
  geom_point(aes(col=subj), alpha=0.3) + 
  geom_line(aes(col=subj), stat='smooth', alpha=0.3) +
  geom_smooth(dat=dat1, aes(x=x, y=avg), col='black') +
  scale_y_continuous(limits=c(-0.1, 1.1)) +
  labs(title = "",
       x = expression(U[self]),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col='Subject') + 
  theme_minimal() + 
  scale_color_viridis(discrete=TRUE, option='plasma')
p10

df2 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/wrongness', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>% 
  {
    # Add column names (s1, s2, s3, etc.)
    column_names <- paste0("s", 1:ncol(.))
    names(.) <- column_names
    .
  } %>%
  cbind(wrongness[0:50]) %>% 
  rename_with(~'x', ncol(.)) %>%
  pivot_longer(cols=-ncol(.), names_to='subj', values_to='y') %>% 
  mutate(y=1-(y-1)/6, x=4*x-2)


dat1 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/wrongness', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>%
  rowMeans() %>% 
  as.data.frame() %>% 
  rename_with(~'y', 1) %>%
  transmute(1 - (y-1)/6) %>%
  rename_with(~'avg', 1)
dat1['x'] <- wrongness[0:50]
dat1 <- dat1 %>%
  mutate(x=4*x-2)

#cor(dat1[1], df)



colnames(df2) <- c('x', 'subj', 'y')
p11 <- ggplot(df2, aes(x=x, y=y)) + 
  geom_point(aes(col=subj), alpha=0.3) + 
  geom_line(aes(col=subj), stat='smooth', alpha=0.3) +
  geom_smooth(dat=dat1, aes(x=x, y=avg), col='black') +
  scale_y_continuous(limits=c(-0.1, 1.1)) +
  labs(title = "",
       x = expression(Wrongness),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col='Subject') + 
  theme_minimal() + 
  scale_color_viridis(discrete=TRUE, option='plasma')
p11

df2 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/utarget', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>% 
  {
    # Add column names (s1, s2, s3, etc.)
    column_names <- paste0("s", 1:ncol(.))
    names(.) <- column_names
    .
  } %>%
  cbind(wrongness[0:50]) %>% 
  rename_with(~'x', ncol(.)) %>%
  pivot_longer(cols=-ncol(.), names_to='subj', values_to='y') %>% 
  mutate(y=(y-1)/6, x=4*x-2)


dat1 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/utarget', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>%
  rowMeans() %>% 
  as.data.frame() %>% 
  rename_with(~'y', 1) %>%
  transmute((y-1)/6) %>%
  rename_with(~'avg', 1)
dat1['x'] <- utarget[0:50]
dat1 <- dat1 %>%
  mutate(x=4*x-2)

#cor(dat1[1], df)



colnames(df2) <- c('x', 'subj', 'y')
p12 <- ggplot(df2, aes(x=x, y=y)) + 
  geom_point(aes(col=subj), alpha=0.3) + 
  geom_line(aes(col=subj), stat='smooth', alpha=0.3) +
  geom_smooth(dat=dat1, aes(x=x, y=avg), col='black') +
  scale_y_continuous(limits=c(-0.1, 1.1)) +
  labs(title = "",
       x = expression(U[target]),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col='Subject') + 
  theme_minimal() + 
  scale_color_viridis(discrete=TRUE, option='plasma')
p12

df2 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/full_uself', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>% 
  {
    # Add column names (s1, s2, s3, etc.)
    column_names <- paste0("s", 1:ncol(.))
    names(.) <- column_names
    .
  } %>%
  cbind(uself[0:50]) %>% 
  rename_with(~'x', ncol(.)) %>%
  pivot_longer(cols=-ncol(.), names_to='subj', values_to='y') %>% 
  mutate(y=(y-1)/6, x=4*x-2)

dat1 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/full_uself', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>%
  rowMeans() %>% 
  as.data.frame() %>% 
  rename_with(~'y', 1) %>%
  transmute((y-1)/6) %>%
  rename_with(~'avg', 1)
dat1['x'] <- uself[0:50]
dat1 <- dat1 %>%
  mutate(x=4*x-2)

cor(dat1[1], df)

p13 <- ggplot(df2, aes(x=x, y=y)) + 
  geom_point(aes(col=subj), alpha=0.3) + 
  geom_line(aes(col=subj), stat='smooth', alpha=0.3) +
  geom_smooth(dat=dat1, aes(x=x, y=avg), col='black') +
  scale_y_continuous(limits=c(-0.1, 1.1)) +
  labs(title = "",
       x = expression(U[target]),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col='Subject') + 
  theme_minimal() + 
  scale_color_viridis(discrete=TRUE, option='plasma')
p13

df3 <- bind_cols(dat1[1], df['1.0'], df['0.0'])
colnames(df3) <- c('Human', 'communicative', 'base')
df3 <- df3 %>% 
  pivot_longer(cols=2:3, names_to = 'punisher', values_to = 'Model')

p15 <- ggplot(data=df3, aes(x=Model, y=Human, col=punisher)) + 
  geom_point(alpha=0.5) + 
  geom_line(stat='smooth', method='lm') + 
  theme_classic() + 
  scale_color_manual(values=c('red', 'black'), guide=FALSE)
p15

df2 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/full_wrongness', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>% 
  {
    # Add column names (s1, s2, s3, etc.)
    column_names <- paste0("s", 1:ncol(.))
    names(.) <- column_names
    .
  } %>%
  cbind(wrongness[0:50]) %>% 
  rename_with(~'x', ncol(.)) %>%
  pivot_longer(cols=-ncol(.), names_to='subj', values_to='y') %>% 
  mutate(y=1 - (y-1)/6,x=x)

dat1 <- list.files('/Users/amarvi_/Desktop/MIT/RCSA_LW-main/subj_data/full_wrongness', full.names=TRUE) %>% 
  lapply(read_csv, show_col_types=FALSE, col_names=FALSE) %>% 
  lapply(subset, select=2) %>%
  bind_cols() %>%
  rowMeans() %>% 
  as.data.frame() %>% 
  rename_with(~'y', 1) %>%
  transmute(1 - (y-1)/6) %>%
  rename_with(~'avg', 1)
dat1['x'] <- wrongness[0:50]
dat1 <- dat1 %>%
  mutate(x=x)

cor(dat1[1], df)

ggplot(df2, aes(x=x, y=y)) + 
  geom_point(aes(col=subj), alpha=0.3) + 
  geom_line(aes(col=subj), stat='smooth', alpha=0.3) +
  geom_smooth(dat=dat1, aes(x=x, y=avg), col='black') +
  scale_y_continuous(limits=c(-0.1, 1.1)) +
  labs(title = "",
       x = expression(Wrongness),
       y = expression(paste("Policy (" , P[punish], ")",sep="")),
       col='Subject')

df3 <- bind_cols(dat1[1], df['1.0'], df['0.0'])
colnames(df3) <- c('Human', 'communicative', 'base')
df3 <- df3 %>% 
  pivot_longer(cols=2:3, names_to = 'punisher', values_to = 'Model')

p14 <- ggplot(data=df3, aes(x=Model, y=Human, col=punisher)) + 
  geom_point(alpha=0.5) + 
  geom_line(stat='smooth', method='lm') + 
  theme_classic() + 
  scale_color_manual(values=c('red', 'black'), guide=FALSE) 
p14

plot_grid(p14, p15, align='hv', ncol=2, labels=c('a', 'b')) 
ggsave('test.png', device='png', dpi=1000, width=9000, height=3000, units='px', bg='white')

prow <- plot_grid(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"), p5 + theme(legend.position="none"), p6 + theme(legend.position="none"), align='hv', ncol=2, labels=c('a', 'b', 'c', 'd')) 

legend_b <- get_legend(p1+theme(legend.position = "right"))
plot_grid(prow, legend_b, ncol = 2, rel_widths = c(1, .1))

ggsave('test.png', device='png', dpi=1000, width=11000, height=6000, units='px', bg='white')

prow <- plot_grid(p9 + theme(legend.position="none"), p8 + theme(legend.position="none"), align='hv', ncol=2, labels=c('a', 'b')) 
legend_b <- get_legend(p9+theme(legend.position = "right"))
plot_grid(prow, legend_b, ncol = 2, rel_widths = c(1, .1))

ggsave('test.png', device='png', dpi=1000, width=10000, height=3000, units='px', bg='white')

plot_grid(p10 + theme(legend.position="none"), p11 + theme(legend.position="none"), p12 + theme(legend.position="none"), align='v', ncol=1, labels=c('a', 'b', 'c'))
ggsave('test.png', device='png', dpi=1000, width=4500, height=9000, units='px', bg='white')

ggplot(data=dist, aes(x=x)) + 
  geom_density()

ggplot(data=dist, aes(x=x, y=p, col=dist)) + 
  geom_point(alpha=0.1)

p_dist <- ggplot() + 
  geom_density(data=sself, aes(x=V1, col='Self', fill='Self'), alpha=0.3) +
  geom_density(data=ssocial, aes(x=V1, col='Social', fill='Social'), alpha=0.3) +
  geom_density(data=starget, aes(x=V1, col='Target', fill='Target'), alpha=0.3) +
  scale_color_manual(name='',
                     breaks=c('Self', 'Social', 'Target'), values=c('Self'='coral', 'Social'='darkseagreen', 'Target'='deepskyblue'), labels=c(expression(A[self]), expression(A[social]), expression(A[target]))) +
  scale_fill_manual(name='',
                    breaks=c('Self', 'Social', 'Target'), values=c('Self'='coral', 'Social'='darkseagreen', 'Target'='deepskyblue'), labels=c(expression(A[self]), expression(A[social]), expression(A[target]))) + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'bottom')

ggsave('odist2.png', device='png', dpi=1000, width=5000, height=3300, units='px', bg='white')