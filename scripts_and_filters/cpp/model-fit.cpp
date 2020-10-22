#include <Rcpp.h>
using namespace Rcpp;

int g_verbose = 4;
int g_nAdvisors = 5;

/**
* @brief Take a set of trials, run gradient descent on them for a family of models,
* and return the parameters which generate the lowest mean squared error for
* each model, along with the errors associated with those parameters.
*
* Although this function returns error for both advisor choice and advice weight, 
* only advice weight error is used for calculating the best parameters in the model.
*
* @param trials a data frame of trials with 5 columns (names may vary):
*  * initialConfidence - initial confidence rating (standardized within participant)
*  * advisorIndex - index of the advisor chosen (0 or 1; NA if no choice made)
*  * choice0 - index of the first advisor offered in the choice (NA if no choice offered)
*  * choice1 - index of the second advisor offered in the choice (NA if no choice offered)
*  * advisorAgrees - whether the chosen advisor agrees (NA if no advice provided)
*  * confidenceShift - amount confidence shifted from initial to final judgement (standardized within participant)
*
* @param testSetMask mask of length nrow(trials) specifying which trials should be used
*  for calculating the best parameters. Defaults to all.
*
* @param nStartingLocations how many different starting locations should be tried for each model
*
* @param learnRate the learning rate for the models (size of the gradient descent steps)
* 
* @param verbosity set this increasingly high for increased logging output
*
* @return List(parameters, trials, MSE) where trials includes the advisor choice and advice weight errors on each trial
*/
List gradientDescent(DataFrame trials, LogicalVector testSetMask, int nStartingLocations, double learnRate, int verbosity);

/**
 * Structure for storing information from the R dataframe
 */
struct Trials {
	NumericVector initialConf;
	NumericVector advisorIndex;
	NumericVector choice0;
	NumericVector choice1;
	LogicalVector advisorAgrees;
	NumericVector confidenceShift;
};

/**
 * Parameters for the model estimated by the learning process
 */
struct Parameters {
	double confWeight;
	double trustUpdateRate;
	double pickVolatility;
	double trustDecay;
	double advisorTrust[5];
};

/**
 * @brief Turn a Parameter struct into a NumericVector
 * @param params {Parameters} Parameters to convert
 * @return {NumericVector} Parameters as a vector
 */
NumericVector spreadParams(Parameters params);

/**
 * @brief Turn a vector of parameters into a Parameter struct
 * @param params {NumericVector} parameters to convert
 * @return {Paramters} Parameters as a struct
 */
Parameters gatherParams(NumericVector params);

/**
 * Model errors for picking and for final confidence change
 */
struct ModelError {
  NumericVector advisorTrust0;
  NumericVector advisorTrust1;
  NumericVector advisorTrust2;
  NumericVector advisorTrust3;
  NumericVector advisorTrust4;
	NumericVector advisorChoice;
	NumericVector adviceWeight;
};

/**
 * Output of the model - parameters producing the lowest mean squared error, and those errors
 */
struct ModelResult {
	ModelError errors;
	Parameters params;
};

/**
 * Pointer to a given model of trust update. These models return parameters with adjusted advisorTrust
 */
typedef double (*ModelFun)(double initialConf, bool advisorAgrees, Parameters params, int advisorIndex);

/**
 * @brief Function which runs a model on all trials in the dataset and returns the errors
 *
 * @param model the trust update model to be used
 * @param trials the real trial data to be fit
 * @param params the parameters of the model
 * @param learnRate the learning rate of the model
 *
 * @return fit errors for each trial in trials
 */
ModelError doModel(ModelFun model, Trials trials, Parameters params);

/**
 * @brief Find the mean squared error of errors
 *
 * @param errors
 * @param testSetMask vector identifying which errors count towards the MSE
 */
double getMSE(NumericVector errors, LogicalVector testSetMask);

/**
 * @brief Run a specific statistical model against a set of values from trials,
 * adjust the parameters using gradient descent, and return the best parameter values
 * and their model fit error for each trial
 *
 * @param model {ModelFun} model to run
 * @param trials {Trials} trials to run model against
 * @param params {Parameters} parameters to use for model
 * @param learnRate stepsize for the gradient descent search
 * @param testSetMask {LogicalVector} vector specifying which trials to use for scoring errors
 *
 * @return ModelResult model parameters and fit errors on each trial
 */
ModelResult findParams(ModelFun model, Trials trials, Parameters params, double learnRate, LogicalVector testSetMask);

/**
 * @brief Generate a random float [0, 1]
 * @param allowNegative whether to extend the range to [-1, 1]
 */
double rRand(bool allowNegative);

NumericVector spreadParams(Parameters params) {
	NumericVector out = NumericVector::create(
		params.confWeight,
		params.trustUpdateRate,
		params.pickVolatility,
		params.trustDecay
	);
	// Add in the advisor trust variables
	for (int a = 0; a < g_nAdvisors; a++)
		out.push_back(params.advisorTrust[a]);
	return out;
}

Parameters gatherParams(NumericVector params) {
	Parameters out;
	out.confWeight = params[0];
	out.trustUpdateRate = params[1];
	out.pickVolatility = params[2];
	out.trustDecay = params[3];
	// Add in advisor trust variables
	int nNonAdvisorParams = 4;
	for (int a = 0; a < g_nAdvisors; a++)
		out.advisorTrust[a] = params[a + nNonAdvisorParams];
	return out;
}

double rRand(bool allowNegative = true) {
	double r = (double)rand() / RAND_MAX;
	if (!allowNegative)
		return r;
	return ((double)rand() / RAND_MAX) > .5 ? r : r * -1;
}


double getMSE(NumericVector errors, LogicalVector testSetMask = LogicalVector::create(0)) {
	bool includeAll = testSetMask.size() == 1 && testSetMask[0] == 0;

	int len = errors.size();
	int n = 0;
	double sumSq = 0;
	for (int i = 0; i < len; i++) {
		if (!NumericVector::is_na(errors[i])) {
			if (includeAll || (testSetMask[i] > 0)) {
				sumSq += errors[i] * errors[i];
				n++;
			}
		}
	}
	return sumSq / n;
}

ModelError doModel(ModelFun model, Trials trials, Parameters params) {

	int trialCount = trials.initialConf.size();
	ModelError errors;
	double minConf = min(trials.initialConf); // used to prevent confidence z-scores being negative

	// Perform the actual model
	for (int t = 0; t < trialCount; t++) {
	  
	  errors.advisorTrust0.push_back(params.advisorTrust[0]);
	  errors.advisorTrust1.push_back(params.advisorTrust[1]);
	  errors.advisorTrust2.push_back(params.advisorTrust[2]);
	  errors.advisorTrust3.push_back(params.advisorTrust[3]);
	  errors.advisorTrust4.push_back(params.advisorTrust[4]);
	  
	  if (g_verbose == 4)
  	  Rcout << t << " ";
	  
	  if (g_verbose >= 5) {
  	  Rcout << "--- Starting trial " << t << " ---" << std::endl;
  	  Rcout << "iC=" << trials.initialConf[t];
  	  Rcout << "; Adv=" << trials.advisorIndex[t];
  	  Rcout << "; Agr=" << trials.advisorAgrees[t];
  	  Rcout << "; dC=" << trials.confidenceShift[t] << std::endl;
	  }
	  if (NumericVector::is_na(trials.advisorIndex[t])) 
	    continue;
	  
		// If there is a choice, calculate the error on the choice
		if (!NumericVector::is_na(trials.choice0[t]) & 
        !NumericVector::is_na(trials.choice1[t])) {
      // Calculate the probability of picking the first advisor by comparing both
      double a[2] = {
        params.advisorTrust[(int)trials.choice0[t]], 
        params.advisorTrust[(int)trials.choice1[t]]
      };
		  // Sigmoid (softmax) picking function weighted by pickVolatility
		  double pPick0 = exp(a[0] * params.pickVolatility) / 
		    (exp(a[0] * params.pickVolatility) + exp(a[1] * params.pickVolatility));
      // Mark the prediction vs the result
      int result = trials.advisorIndex[t] == trials.choice0[t]? 1 : 0;
      errors.advisorChoice.push_back(result - pPick0);
		}
		else {
			errors.advisorChoice.push_back(NA_REAL);
		}
		
		// Estimate confidence shift
		double shift = 0;

		shift = trials.initialConf[t] * params.confWeight;
		
		// Update trust for advisor giving advice
		int a = trials.advisorIndex[t];
		if (!NumericVector::is_na(trials.advisorAgrees[t])) {
			shift *= trials.advisorAgrees[t];
			shift *= params.advisorTrust[a];
			double curTrust = params.advisorTrust[a];
			// Update trust in advisor
			params.advisorTrust[a] = model(trials.initialConf[t] + minConf, trials.advisorAgrees[t], params, a);
			if (g_verbose >= 5)
  			Rcout << "TrustUpdate: " << curTrust << " -> " << params.advisorTrust[a] << std::endl;
		}
		errors.adviceWeight.push_back(shift - (double)trials.confidenceShift[t]);
		if (g_verbose >= 5) {
		  Rcout << "Errors: weight=" << errors.adviceWeight[t];
		  Rcout << "; choice="<< errors.advisorChoice[t] << std::endl;
		}
	}

	if (g_verbose >= 4)
	  Rcout << std::endl << "Trials complete." << std::endl;
	return errors;
}


/**
* Trust update model in which trust does not change from an initial value
*/
double model0(double initialConf, bool advisorAgrees, Parameters params, int advisorIndex) {
	return params.advisorTrust[advisorIndex];
}


/**
* Trust update model in which trust changes based on agreement
*/
double model1(double initialConf, bool advisorAgrees, Parameters params, int advisorIndex) {

	int trust = params.advisorTrust[advisorIndex] - params.trustDecay;

	if (advisorAgrees <= 0)
		return trust;

	return trust + params.trustUpdateRate;
}


/**
* Trust update model in which trust changes based on agreement, weighted by confidence
*/
double model2(double initialConf, bool advisorAgrees, Parameters params, int advisorIndex) {

	int trust = params.advisorTrust[advisorIndex] - params.trustDecay;

	if (advisorAgrees <= 0)
		return trust;

	return trust + (params.trustUpdateRate* initialConf);
}


ModelResult findParams(ModelFun model, Trials trials, Parameters params,
	double learnRate = 0.05, LogicalVector testSetMask = LogicalVector::create(0)) {

	Parameters bestParams = params;
	Parameters testParams = params;

	double mse;
	double bestMSE = INFINITY;

	ModelError errors;
	ModelError bestErrors;

	int cycles = 0;
	int stalemate = 0;

	// Gradient descent
	while (true) {
	  
	  if (g_verbose >= 2) {
	    Rcout << "### New Params #######################" << std::endl;
	    Rcout << "Conf weight: " << params.confWeight << std::endl;
	    Rcout << "Pick volatility: " << params.pickVolatility << std::endl;
	    Rcout << "Trust decay: " << params.trustDecay << std::endl;
	    Rcout << "Trust volatility: " << params.trustUpdateRate << std::endl;
	    for (int i = 0; i < g_nAdvisors; i++) {
	      Rcout << "Advisor Trust[" << i << "]: " << params.advisorTrust[i] << std::endl;
	    }
	    Rcout << "######################################" << std::endl;
	  }

		// Perform the actual model
		errors = doModel(model, trials, testParams);
		mse = getMSE(errors.adviceWeight, testSetMask);
	  if (g_verbose >= 2) {
	    Rcout << "MSE = " << mse << " vs " << bestMSE << std::endl;
	  }

		// Check for MSE improvement
		if (bestMSE > mse) {
			// Store model
			bestMSE = mse;
			bestErrors = errors;
			bestParams = testParams;

			stalemate = 0;
		}
		else {
			// Check for stalemate overall (i.e. solution)
			if (++stalemate > 10)
				break;
		}

		if (cycles++ > 100000)
			break;

		// Update parameters using partial derivatives
		NumericVector spread = spreadParams(testParams);
		NumericVector gradients(spread.size());
		if (g_verbose >= 3) 
		  Rcout << "Calculating new params with partials" << std::endl;
		// Temporarily silence logging
		int verbose = g_verbose;  
		g_verbose = 0;
		for (int i = 0; i < spread.size(); i++) {
			NumericVector partialParams = spread;

			// add a little to the parameter being tested
			partialParams[i] += learnRate;

			// find the new error
			ModelError partialErrors = doModel(model, trials, gatherParams(partialParams));
			double partialError = getMSE(partialErrors.adviceWeight, testSetMask);

			// Follow the direction of the gradient for this parameter
			// should probably scale this across all parameters to go faster down larger gradients
			gradients[i] = (mse - partialError) / learnRate;
		}
		// Normalise gradients
		double gradSum = sum(gradients);
		for (int i = 0; i < gradients.size(); i++) {
			gradients[i] = gradients[i] / gradSum;
			// Update parameters
			if (gradients[i] > 0)
				spread[i] -= learnRate * gradients[i];
			else
				spread[i] += learnRate * gradients[i];
		}

		testParams = gatherParams(spread);
		g_verbose = verbose;  // Turn detailed logging back on
	}

	ModelResult out = { bestErrors, bestParams };
	return out;
}


// [[Rcpp::export]]
List gradientDescent(
    DataFrame trials, 
    LogicalVector testSetMask = LogicalVector::create(0),
    int nStartingLocations = 5, 
    double learnRate = 0.05,
    int verbosity = 0
) {

  g_verbose = verbosity;
  
	Trials trialData;
	trialData.initialConf = as<NumericVector>(trials[0]);
	trialData.advisorIndex = as<NumericVector>(trials[1]);
	trialData.choice0 = as<NumericVector>(trials[2]);
	trialData.choice1 = as<NumericVector>(trials[3]);
	trialData.advisorAgrees = as<LogicalVector>(trials[4]);
	trialData.confidenceShift = as<NumericVector>(trials[5]);

	ModelFun modelFuns[3] = { model0, model1, model2 };
	ModelResult modelResults[3];

	for (int i = 0; i < nStartingLocations; i++) {
		// Loop through the models and look for the parameters which give the lowest MSE after
		// undergoing gradient descent
		int len = sizeof(modelFuns) / sizeof(modelFuns[0]);
		for (int m = 0; m < len; m++) {
		  if (g_verbose >= 1) {
		    Rcout << "FITTING PARAMS FOR MODEL " << m << " (run " << i << ")" << std::endl;
		  }
		  
			// Randomize starting parameters
			Parameters params;
			params.confWeight = rRand();
			params.pickVolatility = rRand(false);
			params.trustUpdateRate= rRand(false);
			params.trustDecay = rRand(false);
			for (int a = 0; a < g_nAdvisors; a++)
				params.advisorTrust[a] = rRand();

			double bestMSE = INFINITY;

			// // Run the model
			ModelResult tmp = findParams(modelFuns[m], trialData, params, learnRate, testSetMask);

			modelResults[m] = tmp;

			// Save if this is the best model
			if (bestMSE < getMSE(tmp.errors.adviceWeight, testSetMask)) {
				modelResults[m] = tmp;
				bestMSE = getMSE(tmp.errors.adviceWeight, testSetMask);
			}
		}
	}

	DataFrame models = DataFrame::create(
		Named("model") = NumericVector::create(1, 2, 3),
		Named("initialConfidenceWeight") =
  		NumericVector::create(modelResults[0].params.confWeight,
                          modelResults[1].params.confWeight,
                          modelResults[2].params.confWeight),
		Named("pickVolatility") =
		  NumericVector::create(modelResults[0].params.confWeight,
                          modelResults[1].params.confWeight,
                          modelResults[2].params.confWeight),
		Named("trustUpdateRate") =
  		NumericVector::create(modelResults[0].params.trustUpdateRate,
                          modelResults[1].params.trustUpdateRate,
                          modelResults[2].params.trustUpdateRate),
		Named("trustDecay") =
  		NumericVector::create(modelResults[0].params.trustDecay,
                          modelResults[1].params.trustDecay,
                          modelResults[2].params.trustDecay)
	);

	for (int a = 0; a < g_nAdvisors; a++) {
		char name[28];
		sprintf(name, "advisorTrust_%d", a);
		models[name] = NumericVector::create(modelResults[0].params.advisorTrust[a],
			modelResults[1].params.advisorTrust[a],
			modelResults[2].params.advisorTrust[a]);
	}
	
	trials.push_back(modelResults[0].errors.advisorTrust0, "advisorTrust0_model1");
	trials.push_back(modelResults[1].errors.advisorTrust0, "advisorTrust0_model2");
	trials.push_back(modelResults[2].errors.advisorTrust0, "advisorTrust0_model3");
	
	trials.push_back(modelResults[0].errors.advisorTrust1, "advisorTrust1_model1");
	trials.push_back(modelResults[1].errors.advisorTrust1, "advisorTrust1_model2");
	trials.push_back(modelResults[2].errors.advisorTrust1, "advisorTrust1_model3");
	
	trials.push_back(modelResults[0].errors.advisorTrust2, "advisorTrust2_model1");
	trials.push_back(modelResults[1].errors.advisorTrust2, "advisorTrust2_model2");
	trials.push_back(modelResults[2].errors.advisorTrust2, "advisorTrust2_model3");
	
	trials.push_back(modelResults[0].errors.advisorTrust3, "advisorTrust3_model1");
	trials.push_back(modelResults[1].errors.advisorTrust3, "advisorTrust3_model2");
	trials.push_back(modelResults[2].errors.advisorTrust3, "advisorTrust3_model3");
	
	trials.push_back(modelResults[0].errors.advisorTrust4, "advisorTrust4_model1");
	trials.push_back(modelResults[1].errors.advisorTrust4, "advisorTrust4_model2");
	trials.push_back(modelResults[2].errors.advisorTrust4, "advisorTrust4_model3");
	
	trials["err_weight_model1"] = modelResults[0].errors.adviceWeight;
	trials["err_weight_model2"] = modelResults[1].errors.adviceWeight;
	trials["err_weight_model3"] = modelResults[2].errors.adviceWeight;

	trials["err_choice_model1"] = modelResults[0].errors.advisorChoice;
	trials["err_choice_model2"] = modelResults[1].errors.advisorChoice;
	trials["err_choice_model3"] = modelResults[2].errors.advisorChoice;

	DataFrame mse = DataFrame::create(
		Named("Model1") = getMSE(modelResults[0].errors.adviceWeight),
		Named("Model2") = getMSE(modelResults[1].errors.adviceWeight),
		Named("Model3") = getMSE(modelResults[2].errors.adviceWeight)
	);

	return List::create(
		_["parameters"] = models,
		_["trials"] = trials,
		_["MSE"] = mse
	);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
invisible(gradientDescent(
  data.frame(
    initialConfidence = rep(0, sample(10:30, 1)),
    advisorIndex = 0,
    choice0 = NA,
    choice1 = NA,
    advisorAgrees = 1,
    advisorInfluenceRaw = .5
  )
))
*/
