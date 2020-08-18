#' Serialize/Unserialize an mlpack model.
#'
#' @param model Input model pointer.
#' @param filename Input filename.
#' @export
#' @rdname mlpack-serialization
Serialize <- function(model, filename) {
  model_serialization_function <-
    switch(attributes(model)$type,
      "GaussianKernel" = SerializeGaussianKernelPtr,
      "AdaBoostModel" = SerializeAdaBoostModelPtr,
      "ApproxKFNModel" = SerializeApproxKFNModelPtr,
      "BayesianLinearRegression" = SerializeBayesianLinearRegressionPtr,
      "CFModel" = SerializeCFModelPtr,
      "DecisionTreeModel" = SerializeDecisionTreeModelPtr,
      "DTree" = SerializeDTreePtr,
      "FastMKSModel" = SerializeFastMKSModelPtr,
      "GMM" = SerializeGMMPtr,
      "HMMModel" = SerializeHMMModelPtr,
      "HoeffdingTreeModel" = SerializeHoeffdingTreeModelPtr,
      "KDEModel" = SerializeKDEModelPtr,
      "LARS" = SerializeLARSPtr,
      "LinearRegression" = SerializeLinearRegressionPtr,
      "LinearSVMModel" = SerializeLinearSVMModelPtr,
      "LocalCoordinateCoding" = SerializeLocalCoordinateCodingPtr,
      "LogisticRegression" = SerializeLogisticRegressionPtr,
      "LSHSearch" = SerializeLSHSearchPtr,
      "NBCModel" = SerializeNBCModelPtr,
      "KNNModel" = SerializeKNNModelPtr,
      "KFNModel" = SerializeKFNModelPtr,
      "PerceptronModel" = SerializePerceptronModelPtr,
      "ScalingModel" = SerializeScalingModelPtr,
      "RandomForestModel" = SerializeRandomForestModelPtr,
      "RANNModel" = SerializeRANNModelPtr,
      "SoftmaxRegression" = SerializeSoftmaxRegressionPtr,
      "SparseCoding" = SerializeSparseCodingPtr,
      stop("Requested model type is not currently supported.")
    )

  # Read in model
  con <- file(as.character(filename), "wb")
  serialize(model_serialization_function(model), con)
  close(con)
}


#' @return For Unserialize, Output model_ptr.
#' @export
#' @rdname mlpack-serialization
Unserialize <- function(filename) {
  con <- file(as.character(filename), "rb")
  model <- unserialize(con)

  model_unserialization_function <-
    switch(attributes(model)$type,
      "GaussianKernel" = DeserializeGaussianKernelPtr,
      "AdaBoostModel" = DeserializeAdaBoostModelPtr,
      "ApproxKFNModel" = DeserializeApproxKFNModelPtr,
      "BayesianLinearRegression" = DeserializeBayesianLinearRegressionPtr,
      "CFModel" = DeserializeCFModelPtr,
      "DecisionTreeModel" = DeserializeDecisionTreeModelPtr,
      "DTree" = DeserializeDTreePtr,
      "FastMKSModel" = DeserializeFastMKSModelPtr,
      "GMM" = DeserializeGMMPtr,
      "HMMModel" = DeserializeHMMModelPtr,
      "HoeffdingTreeModel" = DeserializeHoeffdingTreeModelPtr,
      "KDEModel" = DeserializeKDEModelPtr,
      "LARS" = DeserializeLARSPtr,
      "LinearRegression" = DeserializeLinearRegressionPtr,
      "LinearSVMModel" = DeserializeLinearSVMModelPtr,
      "LocalCoordinateCoding" = DeserializeLocalCoordinateCodingPtr,
      "LogisticRegression" = DeserializeLogisticRegressionPtr,
      "LSHSearch" = DeserializeLSHSearchPtr,
      "NBCModel" = DeserializeNBCModelPtr,
      "KNNModel" = DeserializeKNNModelPtr,
      "KFNModel" = DeserializeKFNModelPtr,
      "PerceptronModel" = DeserializePerceptronModelPtr,
      "ScalingModel" = DeserializeScalingModelPtr,
      "RandomForestModel" = DeserializeRandomForestModelPtr,
      "RANNModel" = DeserializeRANNModelPtr,
      "SoftmaxRegression" = DeserializeSoftmaxRegressionPtr,
      "SparseCoding" = DeserializeSparseCodingPtr,
      stop("Requested model type is not currently supported.")
    )

  model_ptr <- model_unserialization_function(model)
  close(con)
  return(model_ptr)
}
