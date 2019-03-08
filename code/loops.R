afdata1_imputed = afdata1




for (i in 1:length(afdata1_imputed$FIXBI2))
{
  if (is.na(afdata1$FIXBI2[i])) {
    afdata1_imputed$FIXBI2[i] <-0
  }
}