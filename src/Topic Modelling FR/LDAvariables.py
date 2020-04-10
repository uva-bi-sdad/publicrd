import gensim

def createLDAvars(docs):

    # Create the variables needed for LDA from the lemmatized text: dictionary (id2word), corpus
    
    # Create Dictionary
    id2word = gensim.corpora.Dictionary(docs)

    keep_only_most_common=int(len(docs)/2) #LDA works best with less features than documents
    #Filter words to only those found in at least a set number of documents (min_appearances)
    id2word.filter_extremes(no_below=3, no_above=0.4, keep_n=keep_only_most_common)


    # Create Corpus (Term Document Frequency)

    #Creates a count for each unique word appearing in the document, where the word_id is substituted for the word
    corpus = [id2word.doc2bow(doc) for doc in docs]

    return id2word, corpus


