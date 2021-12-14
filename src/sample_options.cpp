
#include <Rcpp.h>
#include <random>
#define pow2(n) ( 1 << (n) )

//' @title cpp_sample_options
//' @name cpp_sample_options
//' @description The C++ sample function that takes a vector of weights and
//'   randomly returns an integer of the choice. Sampling solution based on:
//' [link](https://stackoverflow.com/questions/57599509/c-random-non-repeated-integers-with-weights).
//' With the of definition of pow2 coming from: [link](https://stackoverflow.com/questions/101439/the-most-efficient-way-to-implement-an-integer-based-power-function-powint-int).
//' A solution for the setting the random seed from: [link](https://www.r-bloggers.com/2018/09/using-rs-set-seed-to-set-seeds-for-use-in-c-c-including-rcpp/).
//'
//' @param W A vector of weights.
//' @param SEED The seed to be used for the sampling.
//' @details Requires a new seed for each run as a separate seed is set
//'   internally every time the function runs.
//' @return An integer corresponding to the chosen weight in the initially
//'   provided vector. __NOTE: indexing begins at 0 matching C++ convention__.

// [[Rcpp::export]]
int cpp_sample_options(std::vector<double> W, int SEED){

  /* initialize random sampler */
  unsigned int seed = SEED;
  std::mt19937 rng(seed);

  int rnd_max = W.size();
  std::vector<double> weights(rnd_max);

  // if the weighting is sub-zero make zero. ie not possible
  for(int i = 0; i < rnd_max; i++){

    if(W[i] < 0){
      weights[i] = 0;
    } else {
      weights[i] = W[i];
    }

  }

  /* determine smallest power of two that is larger than N */
  int tree_levels = ceil(log2((double) rnd_max));

  /* initialize vector with place-holders for perfectly-balanced tree */
  std::vector<double> tree_weights(pow2(tree_levels + 1));

  /* compute sums for the tree leaves at each node */
  int offset = pow2(tree_levels) - 1;
  for (int ix = 0; ix < rnd_max; ix++) {
    tree_weights[ix + offset] = weights[ix];
  }
  for (int ix = pow2(tree_levels+1) - 1; ix > 0; ix--) {
    tree_weights[(ix - 1) / 2] += tree_weights[ix];
  }

  /* sample according to uniform distribution */
  double rnd_subrange, w_left;
  double curr_subrange;
  int curr_ix;
  std::vector<int> sampled(1);
  for (int el = 0; el < 1; el++) {

    /* go down the tree by drawing a random number and
     checking if it falls in the left or right sub-ranges */
    curr_ix = 0;
    curr_subrange = tree_weights[0];
    for (int lev = 0; lev < tree_levels; lev++) {
      rnd_subrange = std::uniform_real_distribution<double>(0, curr_subrange)(rng);
      w_left = tree_weights[2 * curr_ix + 1];
      curr_ix = 2 * curr_ix + 1 + (rnd_subrange >= w_left);
      curr_subrange = tree_weights[curr_ix];
    }

    /* finally, add element from this iteration */
    sampled[el] = curr_ix - offset;

    /* now remove the weight of the chosen element */
    tree_weights[curr_ix] = 0;
    for (int lev = 0; lev < tree_levels; lev++) {
      curr_ix = (curr_ix - 1) / 2;
      tree_weights[curr_ix] =   tree_weights[2 * curr_ix + 1]
      + tree_weights[2 * curr_ix + 2];
    }
  }

  // std::cout << "sampled integers: [ ";
  // for (int a : sampled) std::cout << a << " ";
  // std::cout << "]" << std::endl;
  return sampled[0];
}
