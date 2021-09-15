import pandas as pd
import pickle
import nltk
import numpy as np
import time

import stanza
import gensim
import re

# Running Lemmatization from 02-Lemmatizing Jupyter Notebook.  It is ported here to be run as a SLURM script.

# load saved df.  df['working_abstract'] contains cleaned text.

df = pd.read_pickle("./abs_to_lemmatize.pkl")


# Create a stanza pipeline: this pipeline will tokenize, determine pos, 
# and then lemmatize the token appropriately.

nlp = stanza.Pipeline(lang='en',processors='tokenize,pos,lemma',tokenize_batch_size=500,lemma_batch_size=500,
                      use_gpu = True)

# Note - I thought about keeping the pos PART if the lemma was "not", ex) isn't --> be, not.  But due to the fact
# that we are using a bag-of-words model that does not take into account word order, this won't matter in the
# topic model.  So, this is not kept.


def token_pos_lemma(doc, pretokened=False, keep_numbers=True):
    
    # This function uses the pipeline to tokenize, find POS, and lemmatize a document
    
    """if pretokened, dont use this function, as it hasnt been adapted for it"""
    
    assert not pretokened #If these are already tokened per another pipeline, this function won't work correctly
    
    new_tokens=[]
     
    processed=nlp(doc)  # this is the line that does the tokenizing, pos, and lemmatizing
    
    for sent in processed.sentences:
        for word in sent.words:
            
            #If its a regular noun, verb, adj, or adverb, keep lemmatized form
            if word.pos in ['NOUN','VERB','ADJ','ADV']:
                new_tokens.append(word.lemma)
            
            #If you decided to retain numbers, their lemma is kept here. 
            #Note that number catching isnt perfect by this lemmatizing.
            elif word.pos=='NUM' and keep_numbers:
                new_tokens.append(word.lemma)
            
            #Exact phrases are kept here with no attempt at lemmatization: e.g. mars does not become mars, 
            #and hopefully scientific words e.g. chemicals will be tagged as propn, x, or intj if needed
            elif word.pos in ['PROPN','X','INTJ']: 
                new_tokens.append(word.text)
            
            #Note that no other tokens are kept        
       
    return new_tokens


t1 = time.time()
df = df.assign(lemma_abstract = df['working_abstract'].apply(lambda x: token_pos_lemma(x)))
t2 = time.time()

print(f"time: {t2-t1}")

df.to_pickle("./lemmatized_abs.pkl")