import pandas as pd
pd.set_option('display.max_columns', 50)

import numpy as np
import pickle
import time

from sklearn.decomposition import NMF
from sklearn.feature_extraction.text import TfidfVectorizer


# full corpus data ingestion

df = pd.read_pickle("../../../data/prd/Tech-Report/FR_meta_and_final_tokens_21SEPT14.pkl")


# input needed for LDA, NMF (all from Scikit-Learn) is one string per document (not a list of strings)

text = []
docs = df["final_tokens"]

for abstract in docs:
    text.append(" ".join(abstract))


# create term document matrix
stop_wds = ['research', 'study', 'project']  # use will be eliminated by max_df

tfidf_vectorizer = TfidfVectorizer(max_df=0.6, min_df=20, lowercase=False, stop_words=stop_wds)
tf_idf = tfidf_vectorizer.fit_transform(text)


# first time topic model run: 50 - seed 1, 100 - seed ?, 150 - seed 3, 200 - seed 4
num_topics = 150

nmf_model = NMF(n_components = num_topics, random_state = 3)
doc_topic = nmf_model.fit_transform(tf_idf)
topic_term = nmf_model.components_


# save the model
with open("/project/biocomplexity/sdad/projects_data/ncses/prd/Tech-Report/nmf_full_150.pkl","wb") as f:
    pickle.dump((doc_topic, topic_term), f)