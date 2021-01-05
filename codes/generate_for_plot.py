import os, csv, io

path = '/Users/Silverlining/Desktop/Experiments/hnpm/compare/'
os.chdir(path)


preposition_postverbal_lan = ['Arabic', 'Hebrew', 'Indonesian', 'Greek', 'Wolof', 'Irish','Latvian', 'Danish', 'English', 'Swedish', 'Norwegian', 'Portuguese', 'Romanian', 'French', 'Galician', 'Italian', 'Bulgarian', 'Serbian', 'Ukrainian']
preposition_preverbal_lan = ['Afrikaans', 'Persian']
preposition_mix_lan = ['Catalan', 'Spanish', 'Dutch', 'German', 'Croatian', 'Czech', 'Polish', 'Russian', 'Slovak', 'Slovenian']

postposition_postverbal_lan = ['Finnish']
postposition_preverbal_lan = ['Japanese', 'Hindi', 'Urdu']
postposition_mix_lan = ['Estonian']

preposition_postverbal = [['Language', 'Position', 'Adposition', 'Parameters', 'Mean', 'CI25', 'CI975']]
preposition_preverbal = [['Language', 'Position', 'Adposition', 'Parameters', 'Mean', 'CI25', 'CI975']]
preposition_mix = [['Language', 'Position', 'Adposition', 'Parameters', 'Mean', 'CI25', 'CI975']]

postposition_postverbal = [['Language', 'Position', 'Adposition', 'Parameters', 'Mean', 'CI25', 'CI975']]
postposition_preverbal = [['Language', 'Position', 'Adposition', 'Parameters', 'Mean', 'CI25', 'CI975']]
postposition_mix = [['Language', 'Position', 'Adposition', 'Parameters', 'Mean', 'CI25', 'CI975']]

for file in os.listdir(path):
	file_name = file.split('-')
	if file_name[0] in preposition_postverbal_lan and file_name[-1] == 'model_output.txt':
		with open(os.path.join(path, file), encoding = 'utf-8') as f:
			for line in f:
				if line.startswith('Language') is False:
					toks = line.split()
					preposition_postverbal.append(toks)

	if file_name[0] in preposition_preverbal_lan and file_name[-1] == 'model_output.txt':
		with open(os.path.join(path, file), encoding = 'utf-8') as f:
			for line in f:
				if line.startswith('Language') is False:
					toks = line.split()
					preposition_preverbal.append(toks)

	if file_name[0] in preposition_mix_lan and file_name[-1] == 'model_output.txt':
		with open(os.path.join(path, file), encoding = 'utf-8') as f:
			for line in f:
				if line.startswith('Language') is False:
					toks = line.split()
					preposition_mix.append(toks)

	if file_name[0] in postposition_postverbal_lan and file_name[-1] == 'model_output.txt':
		with open(os.path.join(path, file), encoding = 'utf-8') as f:
			for line in f:
				if line.startswith('Language') is False:
					toks = line.split()
					postposition_postverbal.append(toks)

	if file_name[0] in postposition_preverbal_lan and file_name[-1] == 'model_output.txt':
		with open(os.path.join(path, file), encoding = 'utf-8') as f:
			for line in f:
				if line.startswith('Language') is False:
					toks = line.split()
					postposition_preverbal.append(toks)

	if file_name[0] in postposition_mix_lan and file_name[-1] == 'model_output.txt':
		with open(os.path.join(path, file), encoding = 'utf-8') as f:
			for line in f:
				if line.startswith('Language') is False:
					toks = line.split()
					postposition_mix.append(toks)


with io.open('preposition-postverbal.csv', 'w', newline = '', encoding = 'utf-8') as f:
	writer = csv.writer(f)
	for tok in preposition_postverbal:
		writer.writerow(tok)

with io.open('preposition-preverbal.csv', 'w', newline = '', encoding = 'utf-8') as f:
	writer = csv.writer(f)
	for tok in preposition_preverbal:
		writer.writerow(tok)

with io.open('preposition-mix.csv', 'w', newline = '', encoding = 'utf-8') as f:
	writer = csv.writer(f)
	for tok in preposition_mix:
		writer.writerow(tok)

with io.open('postposition-postverbal.csv', 'w', newline = '', encoding = 'utf-8') as f:
	writer = csv.writer(f)
	for tok in postposition_postverbal:
		writer.writerow(tok)

with io.open('postposition-preverbal.csv', 'w', newline = '', encoding = 'utf-8') as f:
	writer = csv.writer(f)
	for tok in postposition_preverbal:
		writer.writerow(tok)

with io.open('postposition-mix.csv', 'w', newline = '', encoding = 'utf-8') as f:
	writer = csv.writer(f)
	for tok in postposition_mix:
		writer.writerow(tok)
