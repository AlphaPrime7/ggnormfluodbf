# Ggnormfluodbf

## Install Dev Version 

```{r}
devtools::install_github('https://github.com/AlphaPrime7/ggnormfluodbf')
library(ggnormfluodbf)
```

## Quick Test

```{r}
data(package = .packages(all.available = TRUE))
data('Affairs', package = 'AER')
ggnormfluodbf_box(Affairs, ggplot2::aes(x=children,y=age,fill=gender), include_labels = F)
ggnormfluodbf_box(Affairs, ggplot2::aes(x=gender,y=rating,fill=children), include_labels = T)
```

## Suggestions Accepted

Please provide suggestions and requests.
