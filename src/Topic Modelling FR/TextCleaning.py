import pandas as pd
import gensim
import time
import nltk
import re


def remove_nulls(df, column_name):
    
    # Remove nulls based on a certain column

    l1 = len(df)
    df = df.loc[pd.notnull(df[column_name])]
    l2 = len(df)
    
    print(l1-l2, "nulls in ", column_name, ". These rows removed.")
    return df
    
    
def remove_duplicates(df):

    ##############
    #Remove duplicates
    #Currently removes only duplicates based on ABSTRACTS and only in the same YEAR
    #The rationale here is that we may do year-by-year modelling and don't want to exclude projects
    #But if we do all-in-one modelling (e.g. across all years), we will want to reconsider
    #Also will want to do additional duplicate check once abstracts are cleaned
    ###############
    
    l1 = len(df)
    df = df.drop_duplicates(subset=['ABSTRACT','FY']) #Drop projects with identical abstracts and year. Different year
                                                          # could indicate additional funding sent to this project.
    l2 = len(df)
    
    print(l1-l2, "duplicate abstracts removed")

    ####################
    #Check for additional duplicates
    #Note that the project id isnt necessarily identical for each transaction on same grant--e.g. one number could be added, 
    #so this isnt that strict and why checking astract is needed
    #####################
    vc=df['PROJECT_ID'].value_counts()
    print(len(vc[vc>1]), 'project ID duplicates - not removed')

    # no output means no duplicate IDs 

    return df


def create_working_abstract_col(df):
    
    # Define a new series which is an abstract that keeps the raw text, but can be continuously manipulated.
    # Also strips white space from beginning and end of abstracts and again removes empty abstracts.

    df['working_abstract']=df['ABSTRACT'].apply(str.strip)
    df.drop(df[df['working_abstract'].apply(len)==0].index,axis=0,inplace=True)
    
    return df


def remove_short_abstracts(df, limit):

    # Remove abstracts with length < limit. 150 seems like a good cutoff, but it does lose some useful information.
    
    df['nchar']=df['working_abstract'].apply(len)
    df=df.loc[df['nchar']>=limit]
    
    return df


def remove_phrase(x, phrase,loc='Start'):
    
    # returns x with phrase removed. location can be "Start" of string, "End" of string, or 
    # "Anywhere_All"--anywhere will remove all instances and Anywhere_First will remove the first instance
    # CASE SENSITIVE
    
    assert loc in ['Start','End','Anywhere_All','Anywhere_First']
    if loc=='End':
        if x.endswith(phrase):
            return x[:-1*len(phrase)].strip()
        else:
            return x
    elif loc=='Start':
        if x.startswith(phrase):
            return x[len(phrase):].strip()
        else:
            return x
    elif loc=='Anywhere_All':
        return x.replace(phrase,'')
    elif loc=='Anywhere_First':
        return x.replace(phrase,'',1)
    else:
        return 'Error'
    

def remove_junk_start(df, col, start_phrases):
    
    # Removes junk phrases from the start of abstracts.  Abstracts are contained in df[col]

    # strip symbols from start
    df[col]=df[col].apply(str.lstrip,args=[' ?-_^. :,!;¿|()[]#%>﻿&\''])

    # Remove found phrases
    for phrase in start_phrases:
        print(phrase)
        df[col]=df[col].apply(remove_phrase,args=[phrase,'Start']).apply(str.lstrip,args=[' :./)'])
        
    #Repeated in case the order of project summary/abstract varies
    for phrase in start_phrases:
        df[col]=df[col].apply(remove_phrase,args=[phrase,'Start']).apply(str.lstrip,args=[' :./)'])
        
    # drop empty abstracts 
    df.drop(df[df[col].apply(len)==0].index,axis=0,inplace=True)
    
    # Often sentences will start with - or *, but they indicate other quality issues & don't end with them,
    # so it's okay to remove them
    df[col]=df[col].apply(str.lstrip,args=['?-*_^. :,!;=¿|]#%>&-\t\n']) 
    
    # update Start Char column in df
    df['Start Char']=df[col].apply(lambda x: x[0])
    
    return df


def remove_junk_middle(df, col):

    # Removal of "junk" found in text body, not at the start or end.  Uses regular expressions.
    # Abstracts in df[col]
    
    expression=re.compile('Enter the text here that.*lines of text')
    df[col]=df[col].apply(lambda x: re.sub(expression,'',x))

    expression=re.compile('PHS .*?Continuation Format Page')
    df[col]=df[col].apply(lambda x: re.sub(expression,'',x))
    expression=re.compile('OMB No .*?Continuation Format Page')
    df[col]=df[col].apply(lambda x: re.sub(expression,'',x))

    df[col]=df[col].replace('Project Summary/Abstract','')
        
    
    return df


def remove_junk_end(df, col, end_phrases):

    # Removes junk phrases from the end of abstracts.  Abstracts are contained in df[col]

    # Remove found phrases
    for phrase in end_phrases:      
        print(phrase)
        df[col]=df[col].apply(remove_phrase,args=[phrase,'End'])

    # drop empty abstracts         
    df.drop(df[df[col].apply(len)==0].index,axis=0,inplace=True)
    
    # update 'LAST_CHAR' column in df
    df['LAST_CHAR']=df[col].apply(lambda x: x[-1])
    
    return df


def tokenize(docs):

    #Converts each document in docs into a list of lowercase tokens
    # - ignores tokens too long (>25) or short (<3) 
    # - remove accents, puncutation, and numbers

    t1 = time.time()
    tokened_docs=[gensim.utils.simple_preprocess(x, deacc=True,min_len=3, max_len=25) for x in docs]
    t2 = time.time()
    print("Time to tokenize abstracts", t2-t1, 'seconds')
    
    return tokened_docs


def create_stopwords():
    
    # stop words include the general English list and those specific to the corpus that do not aid in meaning
    
    stopWords = set(nltk.corpus.stopwords.words('english'))
    
    # format stop words the same way we formatted our corpus, ie. without apostrophes.  
    stop_wds = stopWords.copy()
    for word in stopWords:
        if "\'" in word:
            stop_wds.discard(word)
            stop_wds.add(word.replace("\'",""))
    
    # more stop words that do not add meaning to topics
    additional_stopwords=['another','well','funding','addition','require','grant', 'thus','measure', 'protocol','project',
                      'specifically', 'federal','institution', 'similar','found','find','investigator','including',
                      'funded', 'via','within', 'thus', 'particular', 'furthermore','study','studie','include','also',
                      'includes','however','whether','due', 'investigators','may','studies','overall', 'subproject','whether','could',
                      'many','finally', 'award', 'several', 'specific', 'aim', 'additional', 'therefore', 'either', 'various','address', 
                      'description', 'applicant', 'aims', 'proposal', 'within', 'among', 'would', 'form'] 
        
    sw = stop_wds.union(additional_stopwords)
    
    return sw
    


def remove_stopwords(texts, stop_words):

    # remove stopwords
    
    return [[word for word in doc if word not in stop_words] for doc in texts]
  

def add_bi_tri_grams(docs):

    #Calculate bi and tri grams in tokenized docs with stop words removed

    # Build the bigram and trigram models
    bigram = gensim.models.Phrases(docs, min_count=5, threshold=100) # higher threshold fewer phrases.
    bi_docs = bigram[docs]
    trigram = gensim.models.Phrases(bi_docs, threshold=100)  

    tri_docs = trigram[bi_docs]

    return tri_docs



def apply_lemmatizer(x):
    
    # apply lemmatizer with correct pos parameter for each word. Takes a tuple of length 2 e.g. ("avocado",'NN')
    
    #Create lemmatizer
    lemmatizer= nltk.stem.wordnet.WordNetLemmatizer()
    
    #The POS tags created using pos_tag method are abbreviations like 'FW' or 'JJ'.
    """['FW',#Foreign word
     'JJ','JJR','JJS', #Adjectives
     'NN','NNS','NNP','NNPS','POS',#Nouns, POS is a possessive noun
     'RB','RBR','RBS','RP', #Adverbs
     'VB','VBD','VBG','VBN','VBP','VBZ','MD']]#Verbs, MD is modal, 'could','will'"""
    #We are ignoring all other categories: numbers, prepositions,pronouns etc.

    y=x[1] #x is a tuple of length 2
    if y in ['JJ','JJR','JJS']:
        return lemmatizer.lemmatize(x[0],pos='a')
    elif y in ['NN','NNS','NNP','NNPS','POS']:
        return lemmatizer.lemmatize(x[0],pos='n')
    elif y in ['RB','RBR','RBS','RP']: #This doesn't actually work
        return lemmatizer.lemmatize(x[0],pos='r')
    elif y in ['VB','VBD','VBG','VBN','VBP','VBZ','MD']:
        return lemmatizer.lemmatize(x[0],pos='v')
    elif y=='FW':
        return x[0]
    elif '_' in x[0]: #Bi and trigrams
        return x[0]


    
def lemmatize(texts):
    
    # Function to lemmatize by part of speech
    # Retain only adjectives, nouns, verbs, and adverbs

    #Traverse each document, tag its part of speech
    #and return tokens that are lemmatized and either a noun, verb, adverb, adj, or foreign work
    #This works REALLY slowly

    lemma_docs=[] #Final lemmatized list of documents, i.e. list of lists of tokens
    t1 = time.time()
    for x in range(len(texts)):
 
        doc=texts[x]
        tagged_sentence = nltk.pos_tag(doc) #output is a list of tuples: [('game', 'NN'), ('explore','VB')]
        tokens_kept=[] #Tokens we are retaining
  
        for token_tuple in tagged_sentence:
            tokens_kept.append(apply_lemmatizer(token_tuple))
    
        #Removes tokens that aren't nouns, verbs, adverbs, adjectives or foreign words
        lemma_docs.append([x for x in tokens_kept if x is not None]) 
    
    t2 = time.time()    
    print("Time to lemmatize:", t2-t1, 'seconds')
    
    return lemma_docs

