# wordrates

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/SimonGreenhill/wordrates/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SimonGreenhill/wordrates/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->


## Usage


```{r}
phl <- read.phlorest("/path/to/cldf")
words <- get_words(phl)
states <- get_states(phl, word)

write.bayestraits(states, filename)


# or
generate('/path/to/cldf', '/path/to/staging')
```

