```{r}


files <- list.files(path = 'img/stu2report', pattern = "png", full.names = TRUE)

for (f in files) {
  cat(paste0("![image_label](", f, ")\n"))
}


```