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
    #so this isnt that strict and why checking abstract is needed
    #####################
    vc=df['PROJECT_ID'].value_counts()
    print(len(vc[vc>1]), 'project ID duplicates - not removed')

    # no output means no duplicate IDs 

    return df


