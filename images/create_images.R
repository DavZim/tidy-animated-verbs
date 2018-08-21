library(tidyAnimatedVerbs)

# add the Fira fonts
if (!getOption("tidy_verb_anim.font_registered", FALSE)) {
  sysfonts::font_add_google("Fira Sans")
  sysfonts::font_add_google("Fira Mono")
  showtext::showtext_auto()
  options(tidy_verb_anim.font_registered = TRUE)
}

## check that all folders exist
check_and_create_statics <- function(folder) {
  ff <- here::here("images", "static", folder)
  if (!dir.exists(ff)) dir.create(ff, recursive = TRUE)
}

check_and_create_statics("gif")
check_and_create_statics("png")
check_and_create_statics("svg")

### Joins
x <- data_frame(
  id = 1:3,
  x = paste0("x", 1:3)
)
y <- data_frame(
  id = (1:4)[-3],
  y = paste0("y", (1:4)[-3])
)
y_extra <- y_extra <- bind_rows(y, data_frame(id = 2, y = "y5"))

# Initial Dataset
df_names <- data_frame(
  .x = c(1.5, 4.5), .y = 0.25,
  value = c("x", "y"),
  size = 12,
  color = "black"
)

initial_join_dfs <- tidyAnimatedVerbs:::proc_data(x, "x") %>%
  bind_rows(mutate(tidyAnimatedVerbs:::proc_data(y, "y"), .x = .x + 3)) %>%
  mutate(frame = 1)

g <- tidyAnimatedVerbs:::plot_data(initial_join_dfs) +
  geom_text(data = df_names, family = "Fira Mono", size = 24)

save_static_plot(g, "original-dfs")

## Joins

ij <- animate_inner_join(x, y)
lf <- animate_left_join(x, y)
lf_extra <- animate_left_join(x, y_extra)
rf <- animate_right_join(x, y)
fj <- animate_full_join(x, y)

ij_g <- animate_inner_join(x, y, "static")
lf_g <- animate_inner_join(x, y, "static")
lf_extra_g <- animate_inner_join(x, y_extra, "static")
rf_g <- animate_right_join(x, y, "static")
fj_g <- animate_full_join(x, y, "static")


anim_save(here::here("images", "inner-join.gif"), ij)
anim_save(here::here("images", "left-join.gif"), lf)
anim_save(here::here("images", "left-join-extra.gif"), lf_extra)
anim_save(here::here("images", "right-join.gif"), rf)
anim_save(here::here("images", "full-join.gif"), fj)

save_static_plot(ij_g, "inner-join")
save_static_plot(lf_g, "left-join")
save_static_plot(lf_extra_g, "left-join-extra")
save_static_plot(rf_g, "right-join")
save_static_plot(fj_g, "full-join")


## Filtering Joins

sj <- animate_semi_join(x, y)
aj <- animate_anti_join(x, y)

sj_g <- animate_semi_join(x, y, "static")
aj_g <- animate_anti_join(x, y, "static")

anim_save(here::here("images", "semi-join.gif"), sj)
anim_save(here::here("images", "anti-join.gif"), aj)

save_static_plot(sj_g, "semi-join")
save_static_plot(aj_g, "anti-join")


### Set Operations
xs <- data_frame(
  id = 1:3,
  x = c(1, 1, 2),
  y = c("a", "b", "a")
)
ys <- data_frame(
  id = c(1, 4),
  x = c(1, 2),
  y = c("a", "b")
)

# Initial Data
df_names <- data_frame(
  .x = c(2.5, 5.5), .y = 0.25,
  value = c("x", "y"),
  size = 12,
  color = "black"
)

initial_set_dfs <- bind_rows(
  tidyAnimatedVerbs:::proc_data_set(xs, "x"),
  tidyAnimatedVerbs:::proc_data_set(ys, "y") %>% mutate(.x = .x + 3)
) %>%
  mutate(frame = 1)

g <- tidyAnimatedVerbs:::plot_data_set(initial_set_dfs, "", NULL, NULL) +
  geom_text(data = df_names, family = "Fira Mono", size = 24)

save_static_plot(g, "original-dfs-set-ops")

## Set operations

un <- animate_union(xs, ys)
un_rev <- animate_union(ys, xs, title = "union(y, x)")
un_all <- animate_union_all(xs, ys)
ins <- animate_intersect(xs, ys)
setd <- animate_setdiff(xs, ys)
setd_rev <- animate_setdiff(ys, xs, title = "setdiff(y, x)")

un_g <- animate_union(xs, ys, "static")
un_rev_g <- animate_union(ys, xs, "static", "union(y, x)")
un_all_g <- animate_union_all(xs, ys, "static")
ins_g <- animate_intersect(xs, ys, "static")
setd_g <- animate_setdiff(xs, ys, "static")
setd_rev_g <- animate_setdiff(xs, ys, "static", "setdiff(y, x)")

anim_save(here::here("images", "union.gif"), un)
anim_save(here::here("images", "union-rev.gif"), un_rev)
anim_save(here::here("images", "union-all.gif"), un_all)
anim_save(here::here("images", "intersect.gif"), ins)
anim_save(here::here("images", "setdiff.gif"), setd)
anim_save(here::here("images", "setdiff-rev.gif"), setd_rev)

save_static_plot(un_g, "union")
save_static_plot(un_rev_g, "union-rev")
save_static_plot(un_all_g, "union-all")
save_static_plot(ins_g, "intersect")
save_static_plot(setd_g, "setdiff")
save_static_plot(setd_rev_g, "setdiff-rev")
