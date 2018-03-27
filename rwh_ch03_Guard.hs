fromMaybe :: a -> Maybe a -> a
fromMaybe defval wrapped = 
  case wrapped of 
    Nothing -> defval
    Just value -> value

