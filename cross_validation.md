Cross Validation
================

## Step One

Cross validation “by hand” on simulated data.

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-1-1.png" width="90%" />

Let’s get this by hand. \[80% data for training, 20% data for testing\]

``` r
train_df = sample_n(nonlin_df, 80)
test_df = anti_join(nonlin_df, train_df, by = "id")
```

``` r
train_df %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_point(data = test_df, color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Let’s try to fit three models.

``` r
linear_mod = lm(y ~ x, data = train_df)
smooth_mod = mgcv::gam(y ~ s(x), data = train_df)
wiggly_mod = mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

Let’s see the results.

``` r
train_df %>%
  add_predictions(smooth_mod) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

Let’s make predictions and compute RMSEs.

\[RMSEs = prediction errors (lower is better)\]

``` r
rmse(linear_mod, test_df)
```

    ## [1] 1.019904

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.382834

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.4885044

## Can we iterate ..?

`cross_mc` by default gives 80% train and 20% test.

``` r
cross_valid_df = 
  crossv_mc(nonlin_df, 100) %>%
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>%
  mutate(
    linear_mod = map(.x = train, ~lm(y ~ x, data = .x)),
    smooth_mod = map(.x = train, ~gam(y ~ s(x), data = .x)),
    wiggly_mod = map(.x = train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))
  ) %>%
  mutate(
    rmse_linear = map2_dbl(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_wiggly = map2_dbl(.x = wiggly_mod, .y = test, ~rmse(model = .x, data = .y))
  )
```

What do these results say about the model choices?

``` r
cross_valid_df %>%
  select(starts_with("rmse")) %>%
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>%
  ggplot(aes(x = model, y = rmse)) + 
  geom_violin()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

The smooth model is doing the best.

Compute averages …

``` r
cross_valid_df %>%
  select(starts_with("rmse")) %>%
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>%
  group_by(model) %>%
  summarize(avg_rmse = mean(rmse))
```

    ## # A tibble: 3 × 2
    ##   model  avg_rmse
    ##   <chr>     <dbl>
    ## 1 linear    0.862
    ## 2 smooth    0.333
    ## 3 wiggly    0.417
