#' Extract serialized information for model.
#'
#' @title Transform Model
#' @param model Input model xml.
#' @return transformed_model
transform_model <- function(model)
{
  model_list <- as_list(read_xml(model))
  model_list <- model_list[[1]]
  model_list <- model_list[[1]]
  
  vec_state = list('matrix', 'column vector', 'row vector')
  
  transformed_model <- vector("list", length(names(model_list)))
  names(transformed_model) <- names(model_list)
  child <- xml_children(xml_children(read_xml(model))[[1]])

  for (i in 1 : length(names(model_list)))
  {
    if (any(names(model_list[[i]]) %in% matrix(c('n_cols', 'n_rows', 'n_elem'))))
    {
      if (any (names(model_list[[i]]) %in% matrix(c('item'))))
      {
        list_m <- vector("list", 5)
        names(list_m) <- matrix(c('n_cols', 'n_rows', 'n_elem', 'vec_state', 'item'))
        list_m[1] = c(as.numeric (xml_text (xml_find_all
           (child[[i]], ".//n_cols"))))
        list_m[2] =  c(as.numeric (xml_text (xml_find_all
           (child[[i]], ".//n_rows"))))
        list_m[3] = c(as.numeric (xml_text (xml_find_all
           (child[[i]], ".//n_elem"))))
        list_m[4] = vec_state [as.numeric (xml_text (xml_find_all
           (child[[i]], ".//vec_state"))) + 1]
        list_m[5] = list(matrix (c (as.numeric (xml_text (xml_find_all
           (child[[i]], ".//item")))), nrow = (as.numeric (xml_text (xml_find_all
           (child[[i]], ".//n_rows"))))))
        transformed_model[i] <- list(list_m)
      }
    }
    else if (any(names(model_list[[i]]) %in% matrix(c('count'))))
    {
      if (as.numeric (xml_text (xml_find_all (child[[i]], ".//count"))) > 0
            && length(names(model_list[[i]][[3]]) > 1))
      {
        count <- as.numeric (xml_text (xml_find_all (child[[i]], ".//count")))
        list_c <- vector("list", 2)
        names(list_c) <- matrix(c('count', 'item'))
        list_c[1] = count
        list_m <- vector("list", count)

        for (j in 3 : count + 2)
        {
          if (length(names(model_list[[i]][[j]]) > 0))
          {
            if (any(names(model_list[[i]][[j]]) %in% matrix(c('n_cols', 'n_rows',
                 'n_elem'))))
            {
              if (any (names(model_list[[i]][[j]]) %in% matrix(c('item'))))
              {
                list_m2 <- vector("list", 5)
                names(list_m2) <- matrix(c('n_cols', 'n_rows', 'n_elem', 'vec_state', 'item'))
                list_m2[1] = c(as.numeric (xml_text (xml_find_all
                              (xml_children(child[[i]])[j], ".//n_cols"))))
                list_m2[2] =  c(as.numeric (xml_text (xml_find_all
                              (xml_children(child[[i]])[j], ".//n_rows"))))
                list_m2[3] = c(as.numeric (xml_text (xml_find_all
                              (xml_children(child[[i]])[j], ".//n_elem"))))
                list_m2[4] = vec_state [as.numeric (xml_text (xml_find_all
                              (xml_children(child[[i]])[j], ".//vec_state"))) + 1]
                list_m2[5] = list(matrix (c (as.numeric (xml_text (xml_find_all
                              (xml_children(child[[i]])[j], ".//item")))),
                              nrow = (as.numeric (xml_text (xml_find_all
                              (xml_children(child[[i]])[j], ".//n_rows"))))))
                list_m[j - 2] = list(list_m2)
                }
            }
          }
        }
        list_c[2] = list(list_m)
        transformed_model[i] = list(list_c)
        
      }
      else if (any (names(model_list[[i]]) %in% matrix(c('item'))))
      {
        list_c <- vector("list", 2)
        names(list_c) <- matrix(c('count', 'item'))
        list_c[1] = (c (as.numeric (xml_text (xml_find_all
                        (child[[i]], ".//count")))))
        list_c[2] = list(matrix (c (as.numeric (xml_text (xml_find_all
                        (child[[i]], ".//item"))))))
        transformed_model[i] = list(list_c)
      }
    }
    else
    {
      transformed_model[i] = list(as.numeric (xml_text (child[[i]])))
    }
  }
  return(transformed_model)
}
