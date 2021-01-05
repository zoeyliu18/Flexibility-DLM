from sklearn import preprocessing
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.utils import shuffle

import numpy as np 
import pandas as pd

import sys, os, statistics, csv, io, argparse


parser = argparse.ArgumentParser()
parser.add_argument('--input', type = str, help = 'path to directory of data')
parser.add_argument('--output', type = str, help = 'output path')

args = parser.parse_args()

path = args.input
os.chdir(path)

all_model_output = []

for f in os.listdir(path):
	if f.endswith('-data.csv'):

		model_output = [['Language', 'Position', 'Adposition', 'Parameters', 'Mean', 'CI25', 'CI975']]

		data = pd.read_csv(f)
#	data = pd.read_csv(f, header = 0)
#	data = data.dropna()

		len_coeff = []
		arg_coeff = []
		pro_coeff = []
		accuracy = []
	
		for i in range(10000):
			X = data[['Len', 'Arg_status', 'Pronominality']]
			y = data[['Order']]
	#	X = data.iloc[:, -7:-1]
	#	y = data.iloc[:, -1]
			X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.1, random_state = 0)
			model = LogisticRegression()
			model.fit(X_train, y_train)
			accuracy.append(model.score(X_test, y_test))
			len_coeff.append(model.coef_[0][0])
			arg_coeff.append(model.coef_[0][1])
			pro_coeff.append(model.coef_[0][2])
			data = shuffle(data)


		len_coeff.sort()
		arg_coeff.sort()
		pro_coeff.sort()
		accuracy.sort()


		len_mean = statistics.mean(len_coeff)
		len_25 = len_coeff[250]
		len_975 = len_coeff[9750]

		arg_mean = statistics.mean(arg_coeff)
		arg_25 = arg_coeff[250]
		arg_975 = arg_coeff[9750]

		pro_mean = statistics.mean(pro_coeff)
		pro_25 = pro_coeff[250]
		pro_975 = pro_coeff[9750]

		accuracy_mean = statistics.mean(accuracy)
		accuracy_25 = accuracy[250]
		accuracy_975 = accuracy[9750]

		language_info = f.split('-')
		model_output.append([language_info[0], language_info[1], language_info[2], 'Accuracy', round(accuracy_mean, 2), round(accuracy_25, 2), round(accuracy_975, 2)])
		model_output.append([language_info[0], language_info[1], language_info[2], 'Len', round(len_mean, 2), round(len_25, 2), round(len_975, 2)])
		model_output.append([language_info[0], language_info[1], language_info[2], 'Arg_status', round(arg_mean, 2), round(arg_25, 2), round(arg_975, 2)])
		model_output.append([language_info[0], language_info[1], language_info[2], 'Pronominality', round(pro_mean, 2), round(pro_25, 2), round(pro_975, 2)])

		with io.open(args.output + f[ : -9] + '-model_output.txt', 'w', encoding = 'utf-8') as f:
			for tok in model_output:
				f.write(' '.join(str(w) for w in tok) + '\n')

		for tok in model_output:
			all_model_output.append(tok)

with io.open(args.output + 'all-model-output.csv', 'w', newline = '', encoding = 'utf-8') as f:
	writer = csv.writer(f)
	for tok in all_model_output:
		writer.writerow(tok)
