/**
 * @file methods/reinforcement_learning/environment/env_type.hpp
 * @author Nishant Kumar
 *
 * This file defines a dummy environment to be used with gym_tcp_api.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_RL_ENVIRONMENT_ENV_TYPE_HPP
#define MLPACK_METHODS_RL_ENVIRONMENT_ENV_TYPE_HPP

#include <mlpack/prereqs.hpp>

namespace mlpack {
namespace rl {

/**
 * To use the dummy environment, one may start by specifying the state and
 * action dimensions.
 * Eg:
 *  DiscreteActionEnv::State::dimension = 4;
 *  DiscreteActionEnv::Action::size = 2;
 * 
 * Now the DiscreteActionEnv class can be used as an EnvironmentType in RL
 * methods as usual. 
 */
class DiscreteActionEnv
{
 public:
  /**
   * Implementation of state of the dummy environment.
   */
  class State
  {
   public:
    /**
     * Construct a state instance.
     */
    State() : data(dimension)
    { /* Nothing to do here. */ }

    /**
     * Construct a state instance from given data.
     *
     * @param data Data for the state.
     */
    State(const arma::colvec& data) : data(data)
    { /* Nothing to do here */ }

    //! Modify the internal representation of the state.
    arma::colvec& Data() { return data; }

    //! Encode the state to a column vector.
    const arma::colvec& Encode() const { return data; }

    //! Dimension of the encoded state.
    static size_t dimension;

   private:
    //! Locally-stored state data.
    arma::colvec data;
  };

  /**
   * Implementation of discrete action.
   */
  class Action
  {
   public:
    // To store the action.
    size_t action = 0;
    // Track the size of the action space.
    static size_t size;
  };

  /**
   * Dummy function to mimic sampling in an environment.
   *
   * @param * (state) The current state.
   * @param * (action) The current action.
   * @param * (nextState) The next state.
   * @return It's of no use so lets keep it 0.
   */
  double Sample(const State& /* state */,
                const Action& /* action */,
                State& /* nextState*/)
  { return 0; }

  /**
   * Dummy function to mimic initial sampling in an environment.
   *
   * @return the dummy state.
   */
  State InitialSample() { return State(); }
  /**
   * Dummy function to find terminal state.
   *
   * @param * (state) The current state.
   * @return It's of no use but so lets keep it false.
   */
  bool IsTerminal(const State& /* state */) const { return false; }
};
size_t DiscreteActionEnv::State::dimension = 0;
size_t DiscreteActionEnv::Action::size = 0;

} // namespace rl
} // namespace mlpack

#endif
