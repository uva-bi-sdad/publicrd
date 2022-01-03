import pandas as pd
import pickle
import numpy as np
import time
import re

import spacy
from spacy.lang.en.stop_words import STOP_WORDS
import sys

# only run one time to download spacy model -- SPECIFICALLY A NOTEBOOK COMMAND
#!{sys.executable} -m spacy download en_core_web_sm

# load saved df.  df['working_abstract'] contains cleaned text.

df = pd.read_pickle("../../../data/prd/Paper/FR_clean_22DEC21.pkl")
df.reset_index(inplace = True)
df.rename(columns={'index':'original index'}, inplace=True)

# Tokenize, POS, Lemmatize, Stop word removal

nlp = spacy.load('en_core_web_sm', disable=['parser', 'ner'])

sentences = df['working_abstract']

print("Lemmatizing...")

t1 = time.time()
lemmas = []

for doc in nlp.pipe(sentences):
    tokens = []
    for token in doc:
        if token.pos_ in ['NOUN', 'VERB', 'ADJ', 'ADV', 'PROPN', 'INTJ', 'NUM', 'X'] and not token.is_stop:
            tokens.append(token.lemma_)
    lemmas.append(tokens)

t2 = time.time()
print(t2-t1)

file = open("../../../data/prd/Paper/FR_lemmas_22DEC21.pkl", 'wb')
pickle.dump(lemmas, file)
file.close()
