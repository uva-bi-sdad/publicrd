import pandas as pd
#import numpy as np
import pickle
import time

from sklearn.decomposition import NMF
from sklearn.feature_extraction.text import TfidfVectorizer

# data needed for coherence calculation

# import entire dataset
f = open('../../../data/prd/Paper/coherence_vars.sav', 'rb')
[id2word, docs] = pickle.load(f)
f.close()

print("data ingested--------------------------", flush = True)

# corpus - word frequency in docs - not needed for coherence calculation
# id2word - dictionary
# docs - df["final_tokens"].str.split()

# input needed for LDA, NMF (from Scikit-Learn) is one string per document (not a list of strings)

text = []

for abstract in docs:
    text.append(" ".join(abstract))
        
        
# Function to format topics as a "list of list of strings".
# Needed for topic coherence function in Gensim

# function modified from https://nlpforhackers.io/topic-modeling/

def list_topics(topic_term_dist, vectorizer, top_n=10):

    #input. top_n: how many words to list per topic.  If -1, then list all words.
       
    topic_words = []
    
    for idx, topic in enumerate(topic_term_dist):  # loop through each row of H.  idx = row index.  topic = actual row
            
        if top_n == -1:   
            topic_words.append([vectorizer.get_feature_names()[i] for i in topic.argsort()[::-1]])
        else:
            topic_words.append([vectorizer.get_feature_names()[i] for i in topic.argsort()[:-top_n - 1:-1]])
        
    return topic_words


# create document-term matrix - TFIDF 

stop_wds = ['research', 'aim', 'project']  # study will be eliminated by max_df

tfidf_vectorizer = TfidfVectorizer(max_df=0.6, min_df=20, lowercase=False, stop_words=stop_wds)
tf_idf = tfidf_vectorizer.fit_transform(text)

print("doc term matrix computed------------", flush = True)


# run once so start up time isn't factored into first iteration time
nmf_model = NMF(n_components=1, random_state = 0)
nmf_model.fit_transform(tf_idf)

print("model loop beginning-----------", flush = True)


# function adapted from https://datascienceplus.com/evaluation-of-topic-modeling-topic-coherence/

def nmf_models(doc_term_matrix, n_topics, vectorizer, rand_start):
    """
    Compute NMF model, save topics list for coherence calc

    Parameters:
    ----------
    tf_idf
    n_topics : list of number of topics

    """
    
    nmf_time = []
    topics_list = []
    
    i = rand_start
    for num_topics in n_topics:

        # create model
        t1 = time.time()
        nmf_model = NMF(n_components=num_topics, random_state = i)
        nmf_model.fit_transform(doc_term_matrix)
        t2 = time.time()
        nmf_time.append(t2-t1)
        print(f"  Model time: {t2-t1}", flush=True)
        
        # create list of topics
        topics = list_topics(nmf_model.components_, vectorizer, top_n=10)
        topics_list.append(topics)
        
        # output completion message
        i = i+1
        print('Number of topics =', num_topics, "complete.", flush=True)

    return nmf_time, topics_list


# code copied from https://datascienceplus.com/evaluation-of-topic-modeling-topic-coherence/
# minor alterations made

n_topics = list(range(5,51,5))
num_runs = 3

batch = 1

col_names = [f"iteration {i+batch}" for i in range(num_runs)]
nmf_t = pd.DataFrame(index = n_topics, columns = col_names)
nmf_topics = pd.DataFrame(index = n_topics, columns = col_names)

for i in range(num_runs):
    
    print(f"Iteration {i}", flush=True)
    
    # run models
    [t, topic_terms] = nmf_models(doc_term_matrix=tf_idf, n_topics=n_topics, vectorizer=tfidf_vectorizer, rand_start = (i+batch)*len(n_topics))
    
    # save results
    nmf_t[f"iteration {i+batch}"] = t
    nmf_topics[f"iteration {i+batch}"] = topic_terms   

# save results 

nmf_t.to_pickle("./results/NMF/nmf_t1-3.pkl")
nmf_topics.to_pickle("./results/NMF/nmf_topics1-3.pkl")
