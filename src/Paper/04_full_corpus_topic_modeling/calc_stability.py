import os, sys
import time
import logging as log
import numpy as np
import term_similarity
from sklearn.metrics.cluster import normalized_mutual_info_score
from sklearn.feature_extraction.text import TfidfVectorizer
import pandas as pd
from sklearn.decomposition import NMF
import matplotlib.pyplot as plt
    
# Function to format topics as a "list of list of strings".
# Needed for topic coherence function in Gensim

# function modified from https://nlpforhackers.io/topic-modeling/

def list_topics(model, vectorizer, top_n):

    #input. top_n: how many words to list per topic.  If -1, then list all words.
       
    topic_words = []
    
    for idx, topic in enumerate(model.components_):  # loop through each row of H.  idx = row index.  topic = actual row
            
        if top_n == -1:   
            topic_words.append([vectorizer.get_feature_names()[i] for i in topic.argsort()[::-1]])
        else:
            topic_words.append([vectorizer.get_feature_names()[i] for i in topic.argsort()[:-top_n - 1:-1]])
    
    return topic_words

def calc_stability_measures(num_topics, text_mat, vectorizer, num_run, n_word):
    
    all_descriptor_set = []
    topic_words = []
    all_partition = []
    
    for run in range(num_run):

        nmf_model = NMF(n_components = num_topics,
                        random_state = run + 14)
        doc_topic = nmf_model.fit_transform(text_mat)

        doc_topic_partition = pd.DataFrame(doc_topic).idxmax(axis = 1).values.tolist()
        all_partition.append(doc_topic_partition)

        topics = list_topics(nmf_model, vectorizer, top_n = n_word)
        topic_words.append(topics)

        all_topic_words = [item for sublist in topics for item in sublist]
        all_descriptor_set.append(all_topic_words)
        
    ### descriptor set difference ###

    # get the set of all terms used in the top terms for specified model
    all_model_terms = []
    for descrip_set in all_descriptor_set:
        model_terms = set()
        for term in descrip_set:
            model_terms.add(term)
        all_model_terms.append(model_terms)

    # perform pairwise comparisons to get DSD
    all_dsd = []
    for i in range(num_run):
        for j in range(i+1,num_run):
            diff = len(all_model_terms[i].symmetric_difference(all_model_terms[j]))
            ndiff = float(diff)/(num_topics*n_word)
            all_dsd.append(ndiff)

    avg_dsd = sum(all_dsd)/(len(all_dsd))
    
    ### term stability ###

    metric = term_similarity.JaccardBinary()
    matcher = term_similarity.RankingSetAgreement(metric)

    # calculate TS score on each pair of models
    all_ts = []
    for i in range(num_run):
        for j in range(i+1,num_run):
            score = matcher.similarity(topic_words[i], topic_words[j])
            all_ts.append(score)

    avg_ts = sum(all_ts)/(len(all_ts))
    
    ### partition stability ###

    # calculate NMI on each pair of model partitions
    all_nmi = []
    for i in range(num_run):
        for j in range(i+1,num_run):
            score = normalized_mutual_info_score(all_partition[i], all_partition[j])
            all_nmi.append(score)

    p_nmi = sum(all_nmi)/(len(all_nmi))
    
    return avg_dsd, avg_ts, p_nmi 



# full corpus
df = pd.read_pickle("../../../prd/Paper/FR_meta_and_final_tokens_23DEC21.pkl")
df.reset_index(inplace = True, drop = True)
    
text = df["final_tokens"]
    
stop_wds = ['research', 'aim', 'project'] 

tfidf_vectorizer = TfidfVectorizer(max_df=0.6, min_df=20, lowercase=False, stop_words=stop_wds)
tf_idf = tfidf_vectorizer.fit_transform(text)

## NMF
num_topics_lst = [20, 50]
num_run = 10
n_word = 10

stability_res = []

for num_topics in num_topics_lst:
    
    avg_dsd, avg_ts, p_nmi = calc_stability_measures(num_topics, tf_idf, tfidf_vectorizer, 
                                                     num_run, n_word)
    
    stability_res.append({'Number of topics': num_topics,
                          'Top terms': n_word,
                          'ADSD': avg_dsd,
                          'ATS': avg_ts,
                          'PNMI': p_nmi})

stability_df = pd.DataFrame(stability_res)

stability_df.to_csv('/project/biocomplexity/sdad/projects_data/ncses/prd/full_stability_measures_paper.csv', index = False)