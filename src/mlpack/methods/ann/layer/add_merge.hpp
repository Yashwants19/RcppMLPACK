/**
 * @file methods/ann/layer/add_merge.hpp
 * @author Marcus Edel
 *
 * Definition of the AddMerge module which accumulates the output of the given
 * modules.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_ANN_LAYER_ADD_MERGE_HPP
#define MLPACK_METHODS_ANN_LAYER_ADD_MERGE_HPP

#include <mlpack/prereqs.hpp>

#include "../visitor/delete_visitor.hpp"
#include "../visitor/delta_visitor.hpp"
#include "../visitor/output_parameter_visitor.hpp"

#include "layer_types.hpp"

namespace mlpack {
namespace ann /** Artificial Neural Network. */ {

/**
 * Implementation of the AddMerge module class. The AddMerge class accumulates
 * the output of various modules.
 *
 * @tparam InputDataType Type of the input data (arma::colvec, arma::mat,
 *         arma::sp_mat or arma::cube).
 * @tparam OutputDataType Type of the output data (arma::colvec, arma::mat,
 *         arma::sp_mat or arma::cube).
 * @tparam CustomLayers Additional custom layers that can be added.
 */
template<
    typename InputDataType = arma::mat,
    typename OutputDataType = arma::mat,
    typename... CustomLayers
>
class AddMerge
{
 public:
  /**
   * Create the AddMerge object using the specified parameters.
   *
   * @param model Expose all the network modules.
   * @param run Call the Forward/Backward method before the output is merged.
   */
  AddMerge(const bool model = false, const bool run = true);

  /**
   * Create the AddMerge object using the specified parameters.
   *
   * @param model Expose all the network modules.
   * @param run Call the Forward/Backward method before the output is merged.
   * @param ownsLayers Delete the layers when this is deallocated.
   */
  AddMerge(const bool model, const bool run, const bool ownsLayers);

  //! Destructor to release allocated memory.
  ~AddMerge();

  /**
   * Ordinary feed forward pass of a neural network, evaluating the function
   * f(x) by propagating the activity forward through f.
   *
   * @param * (input) Input data used for evaluating the specified function.
   * @param output Resulting output activation.
   */
  template<typename InputType, typename OutputType>
  void Forward(const InputType& /* input */, OutputType& output);

  /**
   * Ordinary feed backward pass of a neural network, calculating the function
   * f(x) by propagating x backwards trough f. Using the results from the feed
   * forward pass.
   *
   * @param * (input) The propagated input activation.
   * @param gy The backpropagated error.
   * @param g The calculated gradient.
   */
  template<typename eT>
  void Backward(const arma::Mat<eT>& /* input */,
                const arma::Mat<eT>& gy,
                arma::Mat<eT>& g);

  /**
   * This is the overload of Backward() that runs only a specific layer with
   * the given input.
   *
   * @param * (input) The propagated input activation.
   * @param gy The backpropagated error.
   * @param g The calculated gradient.
   * @param index The index of the layer to run.
   */
  template<typename eT>
  void Backward(const arma::Mat<eT>& /* input */,
                const arma::Mat<eT>& gy,
                arma::Mat<eT>& g,
                const size_t index);

  /*
   * Calculate the gradient using the output delta and the input activation.
   *
   * @param input The input parameter used for calculating the gradient.
   * @param error The calculated error.
   * @param gradient The calculated gradient.
   */
  template<typename eT>
  void Gradient(const arma::Mat<eT>& input,
                const arma::Mat<eT>& error,
                arma::Mat<eT>& gradient);

  /*
   * This is the overload of Gradient() that runs a specific layer with the
   * given input.
   *
   * @param input The input parameter used for calculating the gradient.
   * @param error The calculated error.
   * @param gradient The calculated gradient.
   * @param The index of the layer to run.
   */
  template<typename eT>
  void Gradient(const arma::Mat<eT>& input,
                const arma::Mat<eT>& error,
                arma::Mat<eT>& gradient,
                const size_t index);

  /*
   * Add a new module to the model.
   *
   * @param args The layer parameter.
   */
  template <class LayerType, class... Args>
  void Add(Args... args) { network.push_back(new LayerType(args...)); }

  /*
   * Add a new module to the model.
   *
   * @param layer The Layer to be added to the model.
   */
  void Add(LayerTypes<CustomLayers...> layer) { network.push_back(layer); }

  //! Get the input parameter.
  InputDataType const& InputParameter() const { return inputParameter; }
  //! Modify the input parameter.
  InputDataType& InputParameter() { return inputParameter; }

  //! Get the output parameter.
  OutputDataType const& OutputParameter() const { return outputParameter; }
  //! Modify the output parameter.
  OutputDataType& OutputParameter() { return outputParameter; }

  //! Get the delta.
  OutputDataType const& Delta() const { return delta; }
  //! Modify the delta.
  OutputDataType& Delta() { return delta; }

  //! Return the model modules.
  std::vector<LayerTypes<CustomLayers...> >& Model()
  {
    if (model)
    {
      return network;
    }

    return empty;
  }

  //! Get the parameters.
  OutputDataType const& Parameters() const { return weights; }
  //! Modify the parameters.
  OutputDataType& Parameters() { return weights; }

  //! Get the value of run parameter.
  bool Run() const { return run; }
  //! Modify the value of run parameter.
  bool& Run() { return run; }

  /**
   * Serialize the layer.
   */
  template<typename Archive>
  void serialize(Archive& ar, const unsigned int /* version */);

 private:
  //! Parameter which indicates if the modules should be exposed.
  bool model;

  //! Parameter which indicates if the Forward/Backward method should be called
  //! before merging the output.
  bool run;

  //! We need this to know whether we should delete the internally-held layers
  //! in the destructor.
  bool ownsLayers;

  //! Locally-stored network modules.
  std::vector<LayerTypes<CustomLayers...> > network;

  //! Locally-stored empty list of modules.
  std::vector<LayerTypes<CustomLayers...> > empty;

  //! Locally-stored delete visitor module object.
  DeleteVisitor deleteVisitor;

  //! Locally-stored output parameter visitor module object.
  OutputParameterVisitor outputParameterVisitor;

  //! Locally-stored delta visitor module object.
  DeltaVisitor deltaVisitor;

  //! Locally-stored delta object.
  OutputDataType delta;

  //! Locally-stored gradient object.
  OutputDataType gradient;

  //! Locally-stored input parameter object.
  InputDataType inputParameter;

  //! Locally-stored output parameter object.
  OutputDataType outputParameter;

  //! Locally-stored weight object.
  OutputDataType weights;
}; // class AddMerge

} // namespace ann
} // namespace mlpack

//! Set the serialization version of the AddMerge class.
namespace boost {
namespace serialization {

template<
    typename InputDataType,
    typename OutputDataType,
    typename... CustomLayers
>
struct version<mlpack::ann::AddMerge<
    InputDataType, OutputDataType, CustomLayers...>>
{
  BOOST_STATIC_CONSTANT(int, value = 1);
};

} // namespace serialization
} // namespace boost

// Include implementation.
#include "add_merge_impl.hpp"

#endif
