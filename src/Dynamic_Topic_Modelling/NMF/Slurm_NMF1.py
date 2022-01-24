import pandas as pd
import numpy
import pickle
import time
import joblib
import gensim
import matplotlib.pyplot as plt
import unsupervised.nmf, unsupervised.rankings


from sklearn.decomposition import NMF
from sklearn.feature_extraction.text import TfidfVectorizer
from gensim.corpora import Dictionary, bleicorpus
from gensim.matutils import hellinger
from gensim.models.coherencemodel import CoherenceModel

# Modify the pre-processing function
def preprocess(df, stopwords):
    # Append all the final tokens
    text = []
    docs = df['final_tokens']
    
    for abstract in docs:
        text.append(' '.join(abstract))
        
    # Create the term-document matrix
    tfidf_vectorizer = TfidfVectorizer(max_df=1.0, min_df=0, lowercase=False, stop_words=stop_wds)
    tf_idf = tfidf_vectorizer.fit_transform(text)
    
    # store the vocabulary map
    v = tfidf_vectorizer.vocabulary_
    terms = []
    for i in range(len(v)):
        terms.append("")
    for term in v.keys():
        terms[ v[term] ] = term
        
    return (tf_idf,terms)


# Function to compute the corpus and dictionary for the DTM LDA
def createLDAvars(docs):

    # Create the variables needed for LDA from df[final_frqwds_removed]: dictionary (id2word), corpus
    
    # Create Dictionary
    id2word = gensim.corpora.Dictionary(docs)

    #Filter words to only those found in at least a set number of documents (min_appearances)
    id2word.filter_extremes(no_below=20, no_above=0.6)
    
    # filter out stop words - "use" already filtered out by previous line
    id2word.filter_tokens(bad_ids=[id2word.token2id['research'], id2word.token2id['project']])

    # Create Corpus (Term Document Frequency)

    #Creates a count for each unique word appearing in the document, where the word_id is substituted for the word
    # corpus not need for c_v coherence
    corpus = [id2word.doc2bow(doc) for doc in docs]

    return id2word, corpus

# Load the dataset. Apply the DTM on the coronavirus corpus (1000 abstracts)
f = open('/project/biocomplexity/sdad/projects_data/ncses/prd/Tech-Report/case_studies/coronavirus_corpus.pkl', 'rb')
df = pickle.load(f)
f.close()
df.head()

# build the dictionary id2word
docs = df["final_tokens"]
[dictionary, corpus] = createLDAvars(docs)

# PREPROCESSING

# Transform dataset as in Greene. Create a data sample for each year
year_list = df['FY'].unique()

for year in year_list:
    df_subset = df[df['FY']==year]
    
    # save the pickle file
    pickle.dump(df_subset, open('/project/biocomplexity/sdad/projects_data/ncses/prd/Dynamic_Topics_Modelling/NMF/data/Coronavirus_'+str(year)+'.pkl','wb'))
    

# Create the term-document matrix tfidf for each pkl file
stop_wds = ['research', 'study', 'project']  # use will be eliminated by max_df

for year in year_list:
    # Load the sample for a given year
    f = open('/project/biocomplexity/sdad/projects_data/ncses/prd/Dynamic_Topics_Modelling/NMF/data/Coronavirus_'+str(year)+'.pkl', 'rb')
    df = pickle.load(f)
    f.close()
            
    # Pre-processing the pkl file
    (tf_idf,terms) = preprocess(df, stop_wds)
    
    # Save the term-document matrix
    joblib.dump((tf_idf,terms,df), '/project/biocomplexity/sdad/projects_data/ncses/prd/Dynamic_Topics_Modelling/NMF/Term_docs_'+str(year)+'.pkl' )
    

# Run DTM-NMF for each number of topic
topic_vec = list(range(10,100,10))
path0='/project/biocomplexity/sdad/projects_data/ncses/prd/Dynamic_Topics_Modelling/NMF'
#path = /project/biocomplexity/sdad/projects_data/ncses/prd/Dynamic_Topics_Modelling/NMF/out/

for topic in topic_vec:
    # Create a windows-topics by applying NMF on each tfidf pkl file
    # Number of topic per windows k= 30
    Topic=topic
    python find-window-topics.py $path0/*.pkl -k topic,topic -o path0/out
    
    # Solve for NMF-DTM
    # number of topics for the 2nd step k=30 
    python $path0/out/find-dynamic-topics.py $path0/Term_docs_2008_windowtopics_k30.pkl $path0/Term_docs_2009_windowtopics_k30.pkl $path0/Term_docs_2010_windowtopics_k30.pkl $path0/Term_docs_2011_windowtopics_k30.pkl $path0/Term_docs_2012_windowtopics_k30.pkl $path0/Term_docs_2013_windowtopics_k30.pkl $path0/Term_docs_2014_windowtopics_k30.pkl $path0/Term_docs_2015_windowtopics_k30.pkl $path0/Term_docs_2016_windowtopics_k30.pkl $path0/Term_docs_2017_windowtopics_k30.pkl $path0/Term_docs_2018_windowtopics_k30.pkl $path0/Term_docs_2019_windowtopics_k30.pkl -k $Topic,$Topic -o /out
    
    # Compute the coherence
    coherence_vec = []
    
    # The coherence pattern at the first step
    # Comments: All topics from the first step has been cluster into k groups
    # change observed within a cluster over time represents the topic evolution
    df['Year'] = df['FY']
    time_slice = df['PROJECT_ID'].groupby(df['Year']).count()
    time = list(time_slice.index)
    time_coherence = []

    for t in time:
        d = int(t)
        window_res = unsupervised.nmf.load_nmf_results('/out/Term_docs_%d_windowtopics_k30.pkl' %d )
        window_k = len(window_res[2])
        window_term_rankings = unsupervised.rankings.truncate_term_rankings( window_res[2], 10 )
    
        # compute the coherence
        cm = CoherenceModel(topics=window_term_rankings, dictionary=dictionary, texts=docs, coherence='c_v', processes=30)
        time_coherence.append(cm.get_coherence())
    
    coherence_tm = pd.Series(time_coherence, index =time)
    coherence_vec.append(coherence_tm.mean())
    
pickle.dump(coherence_vec, open('/project/biocomplexity/sdad/projects_data/ncses/prd/Dynamic_Topics_Modelling/NMF/coherence_topics_NMF.pkl','wb'))

