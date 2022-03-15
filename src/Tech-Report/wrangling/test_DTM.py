import pandas as pd
import numpy
import pickle
import time
import gensim

#from sklearn.decomposition import LatentDirichletAllocation
#from sklearn.feature_extraction.text import CountVectorizer
#from gensim.models.coherencemodel import CoherenceModel

# setting up our imports
from gensim.models import ldaseqmodel
from gensim.corpora import Dictionary, bleicorpus
from gensim.matutils import hellinger

# Import the final tokens
f = open('/project/biocomplexity/sdad/projects_data/ncses/prd/Tech-Report/FR_meta_and_final_tokens_21SEPT14.pkl', 'rb')
df = pickle.load(f)
f.close()

# Select a subset of Abstract to run the DTM. Select randomly 1/6 of abstracts
df_test = df.sample(n =10000, replace=False)
len(df_test)

def createLDAvars(docs):

    # Create the variables needed for LDA from df[final_frqwds_removed]: dictionary (id2word), corpus
    
    # Create Dictionary
    id2word = gensim.corpora.Dictionary(docs)

    #Filter words to only those found in at least a set number of documents (min_appearances)
    id2word.filter_extremes(no_below=20, no_above=0.6)
    
    # filter out stop words - "use" already filtered out by previous line
    id2word.filter_tokens(bad_ids=[id2word.token2id['research'], id2word.token2id['study'], \
                               id2word.token2id['project']])

    # Create Corpus (Term Document Frequency)

    #Creates a count for each unique word appearing in the document, where the word_id is substituted for the word
    # corpus not need for c_v coherence
    corpus = [id2word.doc2bow(doc) for doc in docs]

    return id2word, corpus
    
# build the dictionary id2word
docs = df_test["final_tokens"]
[dictionary, corpus] = createLDAvars(docs)

# Create the time slice
df_test['Year'] = df_test['PROJECT_START_DATE'].str[-4:]
time_slice = df_test['PROJECT_ID'].groupby(df_test['Year']).count()

# Run the DMT. Pre-training model
ldaseq = ldaseqmodel.LdaSeqModel(corpus=corpus, id2word=dictionary, time_slice=time_slice, num_topics=30)

# save to file
pickle.dump(ldaseq, open('ldaseq.slurm','wb'))