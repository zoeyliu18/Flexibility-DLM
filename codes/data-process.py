##### Verb	Verb_case	Obj_case	NP	PP	NP_len	PP_len	Len_diff	V_NP_d	V_PP_d	ADP	Shift	PrePost #####

import sys, io, csv, random, os, argparse


parser = argparse.ArgumentParser()
parser.add_argument('--input', type = str, help = 'path to directory of data')
parser.add_argument('--output', type = str, help = 'output path')

args = parser.parse_args()

path = args.input
os.chdir(path)



for file in os.listdir(path):
	if file.endswith('-hnpm.csv'):
#with open(sys.argv[1] + '-hnpm.csv', encoding = 'utf-8') as f:
		with io.open(file, encoding = 'utf-8') as f:
			csv_reader = csv.reader(f, delimiter=',')
			verb = []
			np_len = []
			pp_len = []
			len_diff = []
			order = []
			arg_status = []
			np_def = []
			pp_def = []
			np_pro = []
			pp_pro = []

			data = []
			
			for row in csv_reader:
				if row[0] != 'Verb':
					data.append(row)
					verb.append(row[0])
					np_len.append(row[5])
					pp_len.append(row[6])
					if row[-7] == 'definite':
						np_def.append(1)
					else:
						np_def.append(-1)
					if row[-6] == 'definite':
						pp_def.append(1)
					else:
						pp_def.append(-1)
					if row[-5] == 'pronominal':
						np_pro.append(1)
					else:
						np_pro.append(-1)
					if row[-4] == 'pronominal':
						pp_pro.append(1)
					else:
						pp_pro.append(-1)
					if row[-2] == 'nonshifted':
						len_diff.append(str(int(row[7]) * (-1)))
					else:
						len_diff.append(row[7])


			split = round(len(data) / 2) + 1

			len_diff_new = []

			pronominality = []


###### manually changed preverbal dataset, regarding pronominality, codes need rewrite #######

			for i in range(split):
				order.append('1')
				len_diff_new.append(len_diff[i])
				if data[i][-2] == 'nonshifted':
					arg_status.append(1)
					if 'postverbal' in file:
						if np_pro[i] == 1 and pp_pro[i] == -1:
							pronominality.append(1)
						if np_pro[i] == 1 and pp_pro[i] == 1:
							pronominality.append(0)
						if np_pro[i] == -1 and pp_pro[i] == 1:
							pronominality.append(-1)
						if np_pro[i] == -1 and pp_pro[i] == -1:
							pronominality.append(0)
					if 'preverbal' in file:
						if np_pro[i] == 1 and pp_pro[i] == -1:
							pronominality.append(-1)
						if np_pro[i] == 1 and pp_pro[i] == 1:
							pronominality.append(0)
						if np_pro[i] == -1 and pp_pro[i] == 1:
							pronominality.append(1)
						if np_pro[i] == -1 and pp_pro[i] == -1:
							pronominality.append(0)

				if data[i][-2] == 'shifted':
					arg_status.append(-1)
					if 'postverbal' in file:
						if np_pro[i] == 1 and pp_pro[i] == -1:
							pronominality.append(-1)
						if np_pro[i] == 1 and pp_pro[i] == 1:
							pronominality.append(0)
						if np_pro[i] == -1 and pp_pro[i] == 1:
							pronominality.append(1)
						if np_pro[i] == -1 and pp_pro[i] == -1:
							pronominality.append(0)
					if 'preverbal' in file:
						if np_pro[i] == 1 and pp_pro[i] == -1:
							pronominality.append(1)
						if np_pro[i] == 1 and pp_pro[i] == 1:
							pronominality.append(0)
						if np_pro[i] == -1 and pp_pro[i] == 1:
							pronominality.append(-1)
						if np_pro[i] == -1 and pp_pro[i] == -1:
							pronominality.append(0)

			for i in range(split, len(data)):
				order.append('0')
				len_diff_new.append(str(int(len_diff[i]) * (-1)))
				if data[i][-2] == 'nonshifted':
					arg_status.append(-1)
					if 'postverbal' in file:
						if np_pro[i] == 1 and pp_pro[i] == -1:
							pronominality.append(-1)
						if np_pro[i] == 1 and pp_pro[i] == 1:
							pronominality.append(0)
						if np_pro[i] == -1 and pp_pro[i] == 1:
							pronominality.append(1)
						if np_pro[i] == -1 and pp_pro[i] == -1:
							pronominality.append(0)
					if 'preverbal' in file:
						if np_pro[i] == 1 and pp_pro[i] == -1:
							pronominality.append(1)
						if np_pro[i] == 1 and pp_pro[i] == 1:
							pronominality.append(0)
						if np_pro[i] == -1 and pp_pro[i] == 1:
							pronominality.append(-1)
						if np_pro[i] == -1 and pp_pro[i] == -1:
							pronominality.append(0)

				if data[i][-2] == 'shifted':
					arg_status.append(1)
					if 'postverbal' in file:
						if np_pro[i] == 1 and pp_pro[i] == -1:
							pronominality.append(1)
						if np_pro[i] == 1 and pp_pro[i] == 1:
							pronominality.append(0)
						if np_pro[i] == -1 and pp_pro[i] == 1:
							pronominality.append(-1)
						if np_pro[i] == -1 and pp_pro[i] == -1:
							pronominality.append(0)
					if 'preverbal' in file:
						if np_pro[i] == 1 and pp_pro[i] == -1:
							pronominality.append(-1)
						if np_pro[i] == 1 and pp_pro[i] == 1:
							pronominality.append(0)
						if np_pro[i] == -1 and pp_pro[i] == 1:
							pronominality.append(1)
						if np_pro[i] == -1 and pp_pro[i] == -1:
							pronominality.append(0)

			length = []

			for i in range(len(len_diff_new)):
				if int(len_diff_new[i]) > 0:
					length.append(1)
				if int(len_diff_new[i]) < 0:
					length.append(-1)
				if int(len_diff_new[i]) == 0:
					length.append(0)


			features = []

			for i in range(len(data)):
#	features.append([verb[i], np_len[i], pp_len[i], len_diff_new[i], np_def[i], pp_def[i], np_pro[i], pp_pro[i], length[i], arg_status[i], order[i]])
				features.append([verb[i], np_len[i], pp_len[i], len_diff_new[i], length[i], arg_status[i], pronominality[i], order[i]])

#random.shuffle(features)

#header = ['Verb', 'NP_len', 'PP_len', 'Len_diff', 'NP_definiteness', 'PP_definiteness', 'NP_pronominality', 'PP_pronominality', 'Len', 'Arg_status', 'Order']

			header = ['Verb', 'NP_len', 'PP_len', 'Len_diff', 'Len', 'Arg_status', 'Pronominality', 'Order']

			with open(args.output + file[ : -9] + '-data.csv', 'w', newline='', encoding = 'utf-8') as data:
				writer = csv.writer(data)
				writer.writerow(header)
				for tok in features:
					writer.writerow(tok)
	