remove_non_joint_dict_words = function(words_vec, joint_dict) {
  # name the words vector with the words vector elements
  names(words_vec) = words_vec
  # select elements that exit in joint dictory from the words vector, the result will be your new words vector.
  new_words_vec = as.vector(na.omit(words_vec[joint_dict]))
  new_words_vec
}


library(stringi)

data("NYTimes")
tweets_df = data.frame(sentence = NYTimes$Subject, stringsAsFactors = FALSE)
head(tweets_df)

sentence2words = stri_extract_all_words(stri_trans_tolower(tweets_df$sentence))

## code below is just used to create a joint dictionary, since i don't have your data.
## you can ignor this
set.seed(123)
joint_dict <- unlist(sample(lapply(sentence2words, function(x) x[sample(1:length(x), 1)]), 90))
##-------

lapply(sentence2words, remove_non_joint_dict_words, joint_dict)
