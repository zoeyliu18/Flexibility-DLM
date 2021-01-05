#usr/bin/env python3
import io, csv, os, glob, math, random, statistics, argparse


parser = argparse.ArgumentParser()
parser.add_argument('--input', type = str, help = 'path to directory of all UD data directories')
parser.add_argument('--output', type = str, help = 'output path')

args = parser.parse_args()

##### Read in CONLL formatted sentence ######

def conll_read_sentence(file_handle):
	sent = []
	for line in file_handle:
		toks = line.split()
		if len(toks) == 0:
			return sent 
		else:
			if toks[0].isdigit() == True:
				sent.append(toks)				
	return None

##### Extract dependents #####

def dependents(index, sentence):
	dependents = []
	for tok in sentence:
		if tok[6] == index:
			dependents.append([tok[0], tok[7]])
	return dependents

##### Has direct object #####

def has_obj(dependents_list, sentence):
	for d in dependents_list:
		if d[1] == 'obj':
			return d[0]
	return None

##### Has PP dependents #####

def has_pp(dependents_list, sentence):
	pps = []
	for d in dependents_list:
		if d[1] == 'obl':
			for d_x in dependents(d[0], sentence):
				adp = []
				if sentence[int(d_x[0]) - 1][3] == 'ADP' and sentence[int(d_x[0]) - 1][7] == 'case':
					if int(d_x[0]) > int(d[0]):
						adp.append('postposition')
					if int(d_x[0]) < int(d[0]):
						adp.append('preposition')
				if len(adp) == 1:
					pps.append([d[0], adp[0]])
	return pps

####### Get the subtree ######

def subtree_generate(index, sentence):
	idxlist = [index]
	min_idx = len(sentence)
	max_idx = 0
	while len(idxlist) != 0:
		i = idxlist.pop()
		if int(i) < min_idx:
			min_idx = int(i)
		if int(i) > max_idx:
			max_idx = int(i)
		if len(dependents(i, sentence)) != 0:
			for j in dependents(i, sentence):
				idxlist.append(j[0])
	subtree = sentence[min_idx - 1 : max_idx]
	subtree_idx = []
	for idx in range(min_idx - 1, max_idx):
		subtree_idx.append(idx)
	return subtree, subtree_idx

##### Extract VP with HNPM #####

def vp_hnpm(index, sentence):
	verb_dependents = dependents(index, sentence)
	if len(verb_dependents) != 0:
		obj = has_obj(verb_dependents, sentence)
		if obj is not None:
			all_pps = has_pp(verb_dependents, sentence)
			if len(all_pps) != 0:
				hnpm = []
				for tok in all_pps:
					if ((int(tok[0]) < int(obj)) and (int(obj) < int(index))) or ((int(obj) < int(tok[0])) and (int(tok[0]) < int(index))) or ((int(index) < int(obj)) and (int(obj) < int(tok[0]))) or ((int(index) < int(tok[0])) and (int(tok[0]) < int(obj))):
						hnpm.append(tok)
				if len(hnpm) == 1:
					np = subtree_generate(obj, sentence)
					pp = subtree_generate(hnpm[0][0], sentence)
					np_lexical = []
					pp_lexical = []
					for w in np[0]:
						if w[7] != 'punct':
							np_lexical.append(w[1])
					for w in pp[0]:
						if w[7] != 'punct':
							pp_lexical.append(w[1])
					verb = sentence[int(index) - 1]
					verb_case = None
					for f in verb[5].split('|'):
						if 'Case=' in f:
							verb_case = f[5 : ]
					obj_case = None
					for f in sentence[int(obj) - 1][5].split('|'):
						if 'Case=' in f:
							obj_case = f[5 : ]

					np_det = 'indefinite'
					pp_det = 'indefinite'

					np_det_check = 0
					pp_det_check = 0

					for d in dependents(obj, sentence):
						if 'Definite=Def' in sentence[int(d[0]) - 1][5].split('|'):
							np_det_check += 1
					if np_det_check > 0:
						np_det = 'definite'

					for d in dependents(hnpm[0][0], sentence):
						if 'Definite=Def' in sentence[int(d[0]) - 1][5].split('|'):
							pp_det_check += 1
					if pp_det_check > 0:
						pp_det = 'definite'

					np_pro = 'nonpronomial'
					pp_pro = 'nonpronomial'

					if sentence[int(obj) - 1][3] == 'PRON':
						np_pro = 'pronominal'
					if sentence[int(hnpm[0][0]) - 1][3] == 'PRON':
						pp_pro = 'pronominal'

					##### Preverbal #####

					if (int(np[1][-1]) + 1 < int(index)):

						##### PP NP V #####

						if int(pp[1][-1]) + 1 == int(np[1][0]):
							return [verb[2], verb_case, obj_case, ' '.join(w for w in np_lexical), ' '.join(w for w in pp_lexical), len(np_lexical), len(pp_lexical), len(np_lexical) - len(pp_lexical), abs(int(index) - int(obj)), abs(int(index) - int(hnpm[0][0])), np_det, pp_det, np_pro, pp_pro, hnpm[0][1], 'nonshifted', 'preverbal']
						if int(pp[1][-1]) + 1 < int(np[1][0]):
							not_punct = 0
							for tok in sentence[int(pp[1][-1]) + 1 : int(np[1][0])]:
								if tok[7] != 'punct':
									not_punct += 1
							if not_punct == 0:
								return [verb[2], verb_case, obj_case, ' '.join(w for w in np_lexical), ' '.join(w for w in pp_lexical), len(np_lexical), len(pp_lexical), len(np_lexical) - len(pp_lexical), abs(int(index) - int(obj)), abs(int(index) - int(hnpm[0][0])), np_det, pp_det, np_pro, pp_pro, hnpm[0][1], 'nonshifted', 'preverbal']

						##### NP PP V #####

						if int(np[1][-1]) + 1 == int(pp[1][0]):
							return [verb[2], verb_case, obj_case, ' '.join(w for w in np_lexical), ' '.join(w for w in pp_lexical), len(np_lexical), len(pp_lexical), len(np_lexical) - len(pp_lexical), abs(int(index) - int(obj)), abs(int(index) - int(hnpm[0][0])), np_det, pp_det, np_pro, pp_pro, hnpm[0][1], 'shifted', 'preverbal']
						if int(np[1][-1]) + 1 < int(pp[1][0]):
							not_punct = 0
							for tok in sentence[int(np[1][-1]) + 1 : int(pp[1][0])]:
								if tok[7] != 'punct':
									not_punct += 1
							if not_punct == 0:
								return [verb[2], verb_case, obj_case, ' '.join(w for w in np_lexical), ' '.join(w for w in pp_lexical), len(np_lexical), len(pp_lexical), len(np_lexical) - len(pp_lexical), abs(int(index) - int(obj)), abs(int(index) - int(hnpm[0][0])), np_det, pp_det, np_pro, pp_pro, hnpm[0][1], 'shifted', 'preverbal']

					##### Postverbal #####

					if int(np[1][0]) + 1 > int(index):

						##### V NP PP #####

						if int(np[1][-1]) + 1 == int(pp[1][0]):
							return [verb[2], verb_case, obj_case, ' '.join(w for w in np_lexical), ' '.join(w for w in pp_lexical), len(np_lexical), len(pp_lexical), len(np_lexical) - len(pp_lexical), abs(int(index) - int(obj)), abs(int(index) - int(hnpm[0][0])), np_det, pp_det, np_pro, pp_pro, hnpm[0][1], 'nonshifted', 'postverbal']
						if int(np[1][-1]) + 1 < int(pp[1][0]):
							not_punct = 0
							for tok in sentence[int(np[1][-1]) + 1 : int(pp[1][0])]:
								if tok[7] != 'punct':
									not_punct += 1
							if not_punct == 0:
								return [verb[2], verb_case, obj_case, ' '.join(w for w in np_lexical), ' '.join(w for w in pp_lexical), len(np_lexical), len(pp_lexical), len(np_lexical) - len(pp_lexical), abs(int(index) - int(obj)), abs(int(index) - int(hnpm[0][0])), np_det, pp_det, np_pro, pp_pro, hnpm[0][1], 'nonshifted', 'postverbal']

						##### V PP NP #####

						if int(pp[1][-1]) + 1 == int(np[1][0]):
							return [verb[2], verb_case, obj_case, ' '.join(w for w in np_lexical), ' '.join(w for w in pp_lexical), len(np_lexical), len(pp_lexical), len(np_lexical) - len(pp_lexical), abs(int(index) - int(obj)), abs(int(index) - int(hnpm[0][0])), np_det, pp_det, np_pro, pp_pro, hnpm[0][1], 'shifted', 'postverbal']
						if int(pp[1][-1]) + 1 < int(np[1][0]):
							not_punct = 0
							for tok in sentence[int(pp[1][-1]) + 1 : int(np[1][0])]:
								if tok[7] != 'punct':
									not_punct += 1
							if not_punct == 0:
								return [verb[2], verb_case, obj_case, ' '.join(w for w in np_lexical), ' '.join(w for w in pp_lexical), len(np_lexical), len(pp_lexical), len(np_lexical) - len(pp_lexical), abs(int(index) - int(obj)), abs(int(index) - int(hnpm[0][0])), np_det, pp_det, np_pro, pp_pro, hnpm[0][1], 'shifted', 'postverbal']

	return None

def sample_hnpm(sample_sentences):
	data = []
	for sent in sample_sentences:
		for tok in sent:
			if tok[3] == 'VERB':
				vp = vp_hnpm(tok[0], sent)
				if vp is not None:
					data.append(vp)
	return data

def all_hnpm(file_handle, directory):
	data = []
	with io.open(directory + '/' + file_handle, encoding = 'utf-8') as f:
		sent = conll_read_sentence(f)
		while sent is not None:
			for tok in sent:
				if tok[3] == 'VERB':
					vp = vp_hnpm(tok[0], sent)
					if vp is not None:
						data.append(vp)
			sent = conll_read_sentence(f)
	return data

def gather_sent(file_handle, directory):
	all_sent = []
	with io.open(directory + '/' + file_handle, encoding = 'utf-8') as f:
		sent = conll_read_sentence(f)
		while sent is not None:
			all_sent.append(sent)
			sent = conll_read_sentence(f)
	return all_sent

path = args.input

os.chdir(path)

header = ['Verb', 'Verb_case', 'Obj_case', 'NP', 'PP', 'NP_len', 'PP_len', 'Len_diff', 'V_NP_d', 'V_PP_d', 'NP_definiteness', 'PP_definiteness', 'NP_pronominality', 'PP_pronominality', 'ADP', 'Shift', 'PrePost']
			


for directory in glob.glob('*'):
	for files in os.listdir(directory):
		if files.endswith('-ud-train.conllu'):
			all_data = all_hnpm(files,directory)
			preposition_preverbal = []
			preposition_postverbal = []
			postposition_preverbal = []
			postposition_postverbal = []

			preposition_preverbal_distribution = []
			preposition_postverbal_distribution = []
			postposition_preverbal_distribution = []
			postposition_postverbal_distribution = []

			totals = []
			counts = []

			H1 = 0
			H2 = 0
			H3 = 0
			H4 = 0

			for tok in all_data:
				if tok[-3] == 'preposition' and tok[-1] == 'preverbal':
					preposition_preverbal.append(tok)
				if tok[-3] == 'preposition' and tok[-1] == 'postverbal':
					preposition_postverbal.append(tok)
				if tok[-3] == 'postposition' and tok[-1] == 'preverbal':
					postposition_preverbal.append(tok)
				if tok[-3] == 'postposition' and tok[-1] == 'postverbal':
					postposition_postverbal.append(tok)

			if len(preposition_preverbal) >= 100:
				c_nonshifted = 0
				c_shifted = 0
				for tok in preposition_preverbal:					
					if tok[-2] == 'nonshifted':
						c_nonshifted += 1
					if tok[-2] == 'shifted':
						c_shifted += 1
				counts.append(c_nonshifted)
				counts.append(c_shifted)
				total = c_nonshifted + c_shifted
				totals.append(total)
				p_nonshifted = c_nonshifted / total
				p_shifted = c_shifted / total
				if p_nonshifted != 0 and p_shifted != 0:
					H1 = -1 * (p_nonshifted * math.log2(p_nonshifted) + p_shifted * math.log2(p_shifted))
			
				with open(args.output + directory[3 : ] + '-preposition-preverbal-hnpm.csv', 'w', newline='', encoding = 'utf-8') as data:
					writer = csv.writer(data)
					writer.writerow(header)
					for tok in preposition_preverbal:
						writer.writerow(tok)

				for i in range(1000000):
					sample_preposition_preverbal = random.choices(preposition_preverbal, k=len(preposition_preverbal))
					
					sample_c_nonshifted = 0
					sample_c_shifted = 0

					for tok in sample_preposition_preverbal:
						if tok[-2] == 'nonshifted':
							sample_c_nonshifted += 1
						if tok[-2] == 'shifted':
							sample_c_shifted += 1

					sample_p_nonshifted = sample_c_nonshifted / len(preposition_preverbal)
					sample_p_shifted = sample_c_shifted / len(preposition_preverbal)

					h1 = 0
					if sample_p_nonshifted != 0 and sample_p_shifted != 0:
						h1 = -1 * (sample_p_nonshifted * math.log2(sample_p_nonshifted) + sample_p_shifted * math.log2(sample_p_shifted))

					preposition_preverbal_distribution.append(h1)

			else:
				counts.append(0)
				counts.append(0)
				totals.append(0)

			if len(preposition_preverbal_distribution) > 0:
				preposition_preverbal_distribution.sort()
				preposition_preverbal_mean = round(statistics.mean(preposition_preverbal_distribution), 2)
				preposition_preverbal_25 = round(preposition_preverbal_distribution[25000], 2)
				preposition_preverbal_975 = round(preposition_preverbal_distribution[975000], 2)

			else:
				preposition_preverbal_mean = 0
				preposition_preverbal_25 = 0
				preposition_preverbal_975 = 0

			if len(preposition_postverbal) >= 100:
				c_nonshifted = 0
				c_shifted = 0
				for tok in preposition_postverbal:					
					if tok[-2] == 'nonshifted':
						c_nonshifted += 1
					if tok[-2] == 'shifted':
						c_shifted += 1
				counts.append(c_nonshifted)
				counts.append(c_shifted)
				total = c_nonshifted + c_shifted
				totals.append(total)
				p_nonshifted = c_nonshifted / total
				p_shifted = c_shifted / total
				if p_nonshifted != 0 and p_shifted != 0:
					H2 = -1 * (p_nonshifted * math.log2(p_nonshifted) + p_shifted * math.log2(p_shifted))

				with open(args.output + directory[3 : ] + '-preposition-postverbal-hnpm.csv', 'w', newline='', encoding = 'utf-8') as data:
					writer = csv.writer(data)
					writer.writerow(header)
					for tok in preposition_postverbal:
						writer.writerow(tok)

				for i in range(1000000):
					sample_preposition_postverbal = random.choices(preposition_postverbal, k=len(preposition_postverbal))
					
					sample_c_nonshifted = 0
					sample_c_shifted = 0

					for tok in sample_preposition_postverbal:
						if tok[-2] == 'nonshifted':
							sample_c_nonshifted += 1
						if tok[-2] == 'shifted':
							sample_c_shifted += 1

					sample_p_nonshifted = sample_c_nonshifted / len(preposition_postverbal)
					sample_p_shifted = sample_c_shifted / len(preposition_postverbal)

					h2 = 0
					if sample_p_nonshifted != 0 and sample_p_shifted != 0:
						h2 = -1 * (sample_p_nonshifted * math.log2(sample_p_nonshifted) + sample_p_shifted * math.log2(sample_p_shifted))

					preposition_postverbal_distribution.append(h2)

			else:
				counts.append(0)
				counts.append(0)
				totals.append(0)

			if len(preposition_postverbal_distribution) > 0:
				preposition_postverbal_distribution.sort()
				preposition_postverbal_mean = round(statistics.mean(preposition_postverbal_distribution), 2)
				preposition_postverbal_25 = round(preposition_postverbal_distribution[25000], 2)
				preposition_postverbal_975 = round(preposition_postverbal_distribution[975000], 2)

			else:
				preposition_postverbal_mean = 0
				preposition_postverbal_25 = 0
				preposition_postverbal_975 = 0

			if len(postposition_preverbal) >= 100:
				c_nonshifted = 0
				c_shifted = 0
				for tok in postposition_preverbal:					
					if tok[-2] == 'nonshifted':
						c_nonshifted += 1
					if tok[-2] == 'shifted':
						c_shifted += 1
				counts.append(c_nonshifted)
				counts.append(c_shifted)
				total = c_nonshifted + c_shifted
				totals.append(total)
				p_nonshifted = c_nonshifted / total
				p_shifted = c_shifted / total
				if p_nonshifted != 0 and p_shifted != 0:
					H3 = -1 * (p_nonshifted * math.log2(p_nonshifted) + p_shifted * math.log2(p_shifted))

				with open(args.output + directory[3 : ] + '-postposition-preverbal-hnpm.csv', 'w', newline='', encoding = 'utf-8') as data:
					writer = csv.writer(data)
					writer.writerow(header)
					for tok in postposition_preverbal:
						writer.writerow(tok)

				for i in range(1000000):
					sample_postposition_preverbal = random.choices(postposition_preverbal, k=len(postposition_preverbal))
					
					sample_c_nonshifted = 0
					sample_c_shifted = 0

					for tok in sample_postposition_preverbal:
						if tok[-2] == 'nonshifted':
							sample_c_nonshifted += 1
						if tok[-2] == 'shifted':
							sample_c_shifted += 1

					sample_p_nonshifted = sample_c_nonshifted / len(postposition_preverbal)
					sample_p_shifted = sample_c_shifted / len(postposition_preverbal)

					h3 = 0
					if sample_p_nonshifted != 0 and sample_p_shifted != 0:
						h3 = -1 * (sample_p_nonshifted * math.log2(sample_p_nonshifted) + sample_p_shifted * math.log2(sample_p_shifted))

					postposition_preverbal_distribution.append(h3)

			else:
				counts.append(0)
				counts.append(0)
				totals.append(0)

			if len(postposition_preverbal_distribution) > 0:
				postposition_preverbal_distribution.sort()
				postposition_preverbal_mean = round(statistics.mean(postposition_preverbal_distribution), 2)
				postposition_preverbal_25 = round(postposition_preverbal_distribution[25000], 2)
				postposition_preverbal_975 = round(postposition_preverbal_distribution[975000], 2)

			else:
				postposition_preverbal_mean = 0
				postposition_preverbal_25 = 0
				postposition_preverbal_975 = 0

			if len(postposition_postverbal) >= 100:
				c_nonshifted = 0
				c_shifted = 0
				for tok in postposition_postverbal:					
					if tok[-2] == 'nonshifted':
						c_nonshifted += 1
					if tok[-2] == 'shifted':
						c_shifted += 1
				counts.append(c_nonshifted)
				counts.append(c_shifted)
				total = c_nonshifted + c_shifted
				totals.append(total)
				p_nonshifted = c_nonshifted / total
				p_shifted = c_shifted / total
				if p_nonshifted != 0 and p_shifted != 0:
					H4 = -1 * (p_nonshifted * math.log2(p_nonshifted) + p_shifted * math.log2(p_shifted))

				with open(args.output + directory[3 : ] + '-postposition-postverbal-hnpm.csv', 'w', newline='', encoding = 'utf-8') as data:
					writer = csv.writer(data)
					writer.writerow(header)
					for tok in postposition_postverbal:
						writer.writerow(tok)

				for i in range(1000000):
					sample_postposition_postverbal = random.choices(postposition_postverbal, k=len(postposition_postverbal))
					
					sample_c_nonshifted = 0
					sample_c_shifted = 0

					for tok in sample_postposition_postverbal:
						if tok[-2] == 'nonshifted':
							sample_c_nonshifted += 1
						if tok[-2] == 'shifted':
							sample_c_shifted += 1

					sample_p_nonshifted = sample_c_nonshifted / len(postposition_postverbal)
					sample_p_shifted = sample_c_shifted / len(postposition_postverbal)

					h4 = 0
					if sample_p_nonshifted != 0 and sample_p_shifted != 0:
						h4 = -1 * (sample_p_nonshifted * math.log2(sample_p_nonshifted) + sample_p_shifted * math.log2(sample_p_shifted))

					postposition_postverbal_distribution.append(h4)

			else:
				counts.append(0)
				counts.append(0)
				totals.append(0)

			if len(postposition_postverbal_distribution) > 0:	
				postposition_postverbal_distribution.sort()
				postposition_postverbal_mean = round(statistics.mean(postposition_postverbal_distribution), 2)
				postposition_postverbal_25 = round(postposition_postverbal_distribution[25000], 2)
				postposition_postverbal_975 = round(postposition_postverbal_distribution[975000], 2)

			else:
				postposition_postverbal_mean = 0
				postposition_postverbal_25 = 0
				postposition_postverbal_975 = 0


			check = 0
			for t in totals:
				if t >= 100:
					check += 1

			if check >= 1:
				with open(args.output + directory[3 : ] + '.txt', 'w', encoding = 'utf-8') as f:
					f.write('Preposition preverbal total: ' + str(totals[0]) + '\n')
					f.write('preposition preverbal nonshifted: ' + str(counts[0]) + '\n')
					f.write('preposition preverbal shifted: ' + str(counts[1]) + '\n')
					f.write('\n')
					f.write('Preposition postverbal total: ' + str(totals[1]) + '\n')
					f.write('preposition postverbal nonshifted: ' + str(counts[2]) + '\n')
					f.write('preposition postverbal shifted: ' + str(counts[3]) + '\n')
					f.write('\n')
					f.write('Postposition preverbal total: ' + str(totals[2]) + '\n')
					f.write('postposition preverbal nonshifted: ' + str(counts[4]) + '\n')
					f.write('postposition preverbal shifted: ' + str(counts[5]) + '\n')
					f.write('\n')
					f.write('Postposition postverbal total: ' + str(totals[3]) + '\n')
					f.write('postposition postverbal nonshifted: ' + str(counts[6]) + '\n')
					f.write('postposition postverbal shifted: ' + str(counts[7]) + '\n')


			with open(args.output + directory[3 : ] + '-statistics.txt', 'w', encoding = 'utf-8') as f:
				f.write('Preposition preverbal whole: ' + str(round(H1, 2)) + '\n')
				f.write('Preposition preverbal mean: ' + str(preposition_preverbal_mean) + '\n')
				f.write('Preposition preverbal se: ' + str(preposition_preverbal_se) + '\n')
				f.write('\n')
				f.write('Preposition postverbal whole: ' + str(round(H2, 2)) + '\n')
				f.write('Preposition postverbal mean: ' + str(preposition_postverbal_mean) + '\n')
				f.write('Preposition postverbal se: ' + str(preposition_postverbal_se) + '\n')
				f.write('\n')
				f.write('Postposition preverbal whole: ' + str(round(H3, 2)) + '\n')
				f.write('Postposition preverbal mean: ' + str(postposition_preverbal_mean) + '\n')
				f.write('Postposition preverbal se: ' + str(postposition_preverbal_se) + '\n')
				f.write('\n')
				f.write('Postposition postverbal whole: ' + str(round(H4, 2)) + '\n')
				f.write('Postposition postverbal mean: ' + str(postposition_postverbal_mean) + '\n')
				f.write('Postposition postverbal se: ' + str(postposition_postverbal_se) + '\n')


