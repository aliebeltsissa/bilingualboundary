'''
This code performs the model fit for Experiment 1.
'''

import numpy as np
import pymc as pm


class BlackBoxLikelihood(pm.aesaraf.at.Op):

	itypes = [pm.aesaraf.at.dvector]
	otypes = [pm.aesaraf.at.dscalar]

	def __init__(self, func):
		self.func = func

	def perform(self, node, inputs, outputs):
		outputs[0][0] = np.array(self.func(inputs[0]))


def compute_p_word_given_target(lexicons, theta, n_words, word_length, n_simulations):
	'''
	Given a bunch of lexicons and candidate parameter values, compute an 8x7x8
	matrix for each lexicon, which gives the probability that the model reader
	would infer word w given t,j.
	'''
	word_inference_matrix_for_each_lexicon = []
	for lexicon in lexicons:
		reader = model.Reader(lexicon, *theta)
		p_w_given_tj = np.zeros((n_words, word_length, n_words), dtype=np.float64)
		for t in range(n_words):
			for j in range(word_length):
				p_w_given_tj[t,j] = reader.p_word_given_target(t, j, decision_rule='MAP', method='fast', n_sims=n_simulations)
		word_inference_matrix_for_each_lexicon.append(p_w_given_tj)
	return word_inference_matrix_for_each_lexicon


def fit_posterior(experiment, chain_i=0, n_samples=1000, n_tuning_samples=200, n_simulations=10000, uniform_priors=False, dataset_override=None):
	dataset, lexicons = experiment.get_CFT_dataset()
	if dataset_override:
		dataset = dataset_override
	n_words = len(lexicons[0])
	word_length = len(lexicons[0][0])

	def log_likelihood_dataset(theta):
		theta = [val * (upper - lower) + lower for val, (lower, upper) in zip(theta, experiment.params.values())]
		# Precompute Pr(w'|t,j) for each lexicon
		p_word_given_target = compute_p_word_given_target(lexicons, theta, n_words, word_length, n_simulations)
		epsilon = theta[-1]
		p_stick_with_w = (1 - epsilon)
		p_switch_to_w = epsilon / (n_words - 1)
		log_likelihood = 0.0
		for l, t, j, w in dataset:
			log_likelihood += np.log(
				# probability of inferring w and sticking to it, plus probability of
				# inferring some other w' but switching to w by mistake
				p_word_given_target[l][t, j, w] * p_stick_with_w + sum([
					p_word_given_target[l][t, j, w_prime] * p_switch_to_w for w_prime in range(n_words) if w_prime != w
				])
			)
		return log_likelihood

	likelihood_func = BlackBoxLikelihood(lambda theta: log_likelihood_dataset(theta))

	with pm.Model() as model:
		if uniform_priors:
			theta = pm.aesaraf.at.as_tensor_variable([
				pm.Uniform(param, 0, 1) for param in experiment.params
			])
		else:
			theta = pm.aesaraf.at.as_tensor_variable([
				pm.Beta(param, *beta_params) for param, (_, beta_params) in experiment.priors.items()
			])
		pm.Potential('likelihood', likelihood_func(theta))
		trace = pm.sample(n_samples, tune=n_tuning_samples, chains=1, cores=1, chain_idx=chain_i)
	for param, (lower, upper) in experiment.params.items():
		trace.posterior[param] = trace.posterior[param] * (upper - lower) + lower
	return trace


def merge_chains(output_file, n_chains):
	from arviz import concat, from_netcdf
	chains = []
	for chain_i in range(n_chains):
		chain_file = output_file + str(chain_i)
		try:
			chains.append(from_netcdf(chain_file))
		except FileNotFoundError:
			continue
	return concat(chains, dim='chain')


if __name__ == '__main__':

	import argparse
	import model
	from experiment import Experiment

	parser = argparse.ArgumentParser()
	parser.add_argument('action', action='store', type=str, help='action to perform (run or merge)')
	parser.add_argument('chain', action='store', type=int, help='chain number (run) or number of chains (merge)')
	parser.add_argument('--n_samples', action='store', type=int, default=2500, help='number of MCMC samples')
	parser.add_argument('--n_tuning_samples', action='store', type=int, default=500, help='number of MCMC tuning samples')
	parser.add_argument('--n_simulations', action='store', type=int, default=10000, help='number of model simulations')
	parser.add_argument('--data_subset', action='store', type=str, default=None, help='fit one subset independently (left or right)')
	parser.add_argument('--uniform_priors', action='store_true', help='use uniform priors')
	parser.add_argument('--output_file', action='store', default=None, help='file to write posterior trace to')
	parser.add_argument('--dataset_override', action='store', default=None, help='use simulated dataset')
	args = parser.parse_args()

	experiment = Experiment('exp1')
	experiment.set_exclusion_threshold(7, 8)
	experiment.set_params({
		'α': ( 0.0625, 0.9999),
		'β': ( 0.0001, 1.0000),
		'γ': (-0.9999, 0.9999),
		'ε': ( 0.0001, 0.9999),
	})
	experiment.set_priors({
		'α': ('beta', (8, 2)),
		'β': ('beta', (2, 8)),
		'γ': ('beta', (4, 2)),
		'ε': ('beta', (2, 16)),
	})

	if args.output_file is None:
		output_file = str(experiment.posterior_file)
	else:
		output_file = args.output_file

	if args.action == 'merge':
		trace = merge_chains(output_file, args.chain)
		trace.to_netcdf(output_file, compress=False)
		exit()
	if args.action != 'run':
		raise ValueError('Invalid action, should be "run" or "merge"')

	if args.data_subset is None:
		exp = experiment
	elif args.data_subset == 'left':
		exp = experiment.left
	elif args.data_subset == 'right':
		exp = experiment.right
	else:
		raise ValueError('condition must be "left", "right" or None')

	if args.dataset_override:
		import json
		with open(args.dataset_override) as file:
			dataset_override = json.load(file)
	else:
		dataset_override = None

	trace = fit_posterior(exp, args.chain, args.n_samples, args.n_tuning_samples, args.n_simulations, args.uniform_priors, dataset_override)
	trace.to_netcdf(output_file + str(args.chain), compress=False)