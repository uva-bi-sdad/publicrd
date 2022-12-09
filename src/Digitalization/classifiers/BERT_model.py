print("In BERT_model.py")

import pandas as pd
import numpy as np
from transformers import BertTokenizer
from transformers import BertModel
import torch
from torch import nn
from torch.optim import Adam
from tqdm import tqdm

#data frame with only labeled abstracts
df_lab = pd.read_csv("/home/kno5cac/git/publicrd/data/prd/Digital_abstract_labelled/labelled_abstracts.csv")

#data frame with all of the abstracts
df = pd.read_pickle("/home/kno5cac/git/publicrd/data/prd/Paper/FR_meta_and_final_tokens_23DEC21.pkl")

# Remove Null Abstracts. Reset the index and create a variable index to link with PROJECT_ID
df = df[~df.ABSTRACT.isnull()]
df.reset_index(inplace = True, drop = True)
df['index'] = df.index
df.head()

df_lab = df_lab[['PROJECT_ID','ABSTRACT','Is it related to Big-Data','label']]
#df_lab
df_lab["Is it related to Big-Data"][343] = '0'
df_lab["Is it related to Big-Data"][1077] = '0'

# merge with the labelled data using PROJECT_ID.
df['PROJECT_ID'] = pd.to_numeric(df['PROJECT_ID'])
df_merge = df.merge(df_lab[['PROJECT_ID','Is it related to Big-Data','label']], how='left', on='PROJECT_ID')
len(df_merge)

df_merge

# Save project ID of labelled data
project_id_lab = list(df_lab['PROJECT_ID'])

# Get the index of labelled abstract
subset_df = df_merge.loc[df_merge['PROJECT_ID'].isin(project_id_lab),['index', 'PROJECT_ID']]
index_lab = list(subset_df['index'])

df_merge['ABSTRACT'][1]

tokenizer = BertTokenizer.from_pretrained('bert-base-cased')

example_text = df_merge['ABSTRACT'][1]
bert_input = tokenizer(example_text,padding='max_length', max_length = 512, 
                       truncation=True, return_tensors="pt")

print(bert_input['input_ids'])
print(bert_input['token_type_ids'])
print(bert_input['attention_mask'])

example_text = tokenizer.decode(bert_input.input_ids[0])
print(example_text)

df_model = df_lab

df_model['Is it related to Big-Data'] = pd.to_numeric(df_model['Is it related to Big-Data'])

class Dataset(torch.utils.data.Dataset):

    def __init__(self, df_model):
        self.labels = df_model["Is it related to Big-Data"].values.tolist()
        self.texts = [tokenizer(text, 
                               padding='max_length', max_length = 512, truncation=True,
                                return_tensors="pt") for text in df_model['ABSTRACT']]

    def classes(self):
        return self.labels

    def __len__(self):
        return len(self.labels)

    def get_batch_labels(self, idx):
        # Fetch a batch of labels
        return np.array(self.labels[idx])

    def get_batch_texts(self, idx):
        # Fetch a batch of inputs
        return self.texts[idx]

    def __getitem__(self, idx):

        batch_texts = self.get_batch_texts(idx)
        batch_y = self.get_batch_labels(idx)

        return batch_texts, batch_y
    
    
    
np.random.seed(112)
df_train, df_val, df_test = np.split(df_lab.sample(frac=1, random_state=42), 
                                     [int(.8*len(df_lab)), int(.9*len(df_lab))])

print(len(df_train),len(df_val), len(df_test))



class BertClassifier(nn.Module):

    def __init__(self, dropout=0.5):

        super(BertClassifier, self).__init__()

        self.bert = BertModel.from_pretrained('bert-base-cased')
        self.dropout = nn.Dropout(dropout)
        self.linear = nn.Linear(768, 5)
        self.relu = nn.ReLU()

    def forward(self, input_id, mask):

        _, pooled_output = self.bert(input_ids= input_id, attention_mask=mask,return_dict=False)
        dropout_output = self.dropout(pooled_output)
        linear_output = self.linear(dropout_output)
        final_layer = self.relu(linear_output)

        return final_layer
    
    
def train(model, train_data, val_data, learning_rate, epochs):

    train, val = Dataset(train_data), Dataset(val_data)

    train_dataloader = torch.utils.data.DataLoader(train, batch_size=8, shuffle=True)
    val_dataloader = torch.utils.data.DataLoader(val, batch_size=8)

    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda" if use_cuda else "cpu")

    criterion = nn.CrossEntropyLoss()
    optimizer = Adam(model.parameters(), lr= learning_rate)

    if use_cuda:

            model = model.cuda()
            criterion = criterion.cuda()

    for epoch_num in range(epochs):

            total_acc_train = 0
            total_loss_train = 0

            for train_input, train_label in tqdm(train_dataloader):

                train_label = train_label.to(device)
                mask = train_input['attention_mask'].to(device)
                input_id = train_input['input_ids'].squeeze(1).to(device)

                output = model(input_id, mask)
                
                batch_loss = criterion(output, train_label.long())
                total_loss_train += batch_loss.item()
                
                acc = (output.argmax(dim=1) == train_label).sum().item()
                total_acc_train += acc

                model.zero_grad()
                batch_loss.backward()
                optimizer.step()
            
            total_acc_val = 0
            total_loss_val = 0

            with torch.no_grad():

                for val_input, val_label in val_dataloader:

                    val_label = val_label.to(device)
                    mask = val_input['attention_mask'].to(device)
                    input_id = val_input['input_ids'].squeeze(1).to(device)

                    output = model(input_id, mask)

                    batch_loss = criterion(output, val_label.long())
                    total_loss_val += batch_loss.item()
                    
                    acc = (output.argmax(dim=1) == val_label).sum().item()
                    total_acc_val += acc
            
            print(
                f'Epochs: {epoch_num + 1} | Train Loss: {total_loss_train / len(train_data): .3f} \
                | Train Accuracy: {total_acc_train / len(train_data): .3f} \
                | Val Loss: {total_loss_val / len(val_data): .3f} \
                | Val Accuracy: {total_acc_val / len(val_data): .3f}')
            
            
            
def evaluate(model, test_data):

    test = Dataset(test_data)

    test_dataloader = torch.utils.data.DataLoader(test, batch_size=8)

    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda" if use_cuda else "cpu")

    if use_cuda:

        model = model.cuda()

    total_acc_test = 0
    with torch.no_grad():

        for test_input, test_label in test_dataloader:

              test_label = test_label.to(device)
              mask = test_input['attention_mask'].to(device)
              input_id = test_input['input_ids'].squeeze(1).to(device)

              output = model(input_id, mask)

              acc = (output.argmax(dim=1) == test_label).sum().item()
              total_acc_test += acc
    
    print(f'Test Accuracy: {total_acc_test / len(test_data): .3f}')
    print(total_acc_test)
    print(len(test_data))
    
    
EPOCHS = 4
model = BertClassifier()
learningrate = 5e-5
       
train(model, df_train, df_val, learningrate, EPOCHS)

evaluate(model, df_test)



df_corpus = df_merge

#proxy replaces NaN with zeroes 
#labelled gives 0 for not labelled and 1 for labelled

df_corpus["proxy"] = df_merge["Is it related to Big-Data"]
df_corpus["proxy"] = df_corpus["proxy"].replace(np.nan, '0')
df_corpus["proxy"]=pd.to_numeric(df_corpus.proxy)


df_corpus["labelled"] = df_merge["Is it related to Big-Data"]
df_corpus["labelled"] = df_corpus["labelled"].replace('0', '1')
df_corpus["labelled"] = df_corpus["labelled"].replace('1', '1')
df_corpus["labelled"] = df_corpus["labelled"].replace(np.nan, '0')
df_corpus["labelled"]=pd.to_numeric(df_corpus.labelled)



pd.value_counts(df_corpus["proxy"])
pd.value_counts(df_corpus["labelled"])
pd.crosstab(df_corpus["proxy"],df_corpus["labelled"])

class Dataset(torch.utils.data.Dataset):

    def __init__(self, df_corpus):
        self.labels = df_corpus["proxy"].values.tolist()
        self.texts = [tokenizer(text, 
                               padding='max_length', max_length = 512, truncation=True,
                                return_tensors="pt") for text in df_corpus['ABSTRACT']]

    def classes(self):
        return self.labels

    def __len__(self):
        return len(self.labels)

    def get_batch_labels(self, idx):
        # Fetch a batch of labels
        return np.array(self.labels[idx])

    def get_batch_texts(self, idx):
        # Fetch a batch of inputs
        return self.texts[idx]

    def __getitem__(self, idx):

        batch_texts = self.get_batch_texts(idx)
        batch_y = self.get_batch_labels(idx)

        return batch_texts, batch_y
    
    
def extrapolate(model, test_data):

    results = test_data
    results["classification"] = 'not classified'
    test = Dataset(test_data)

    test_dataloader = torch.utils.data.DataLoader(test, batch_size=50)
    print(test_dataloader)
    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda" if use_cuda else "cpu")

    if use_cuda:

        model = model.cuda()

    round = 0
    with torch.no_grad():

        for test_input, test_label in test_dataloader:

              mask = test_input['attention_mask'].to(device)
              input_id = test_input['input_ids'].squeeze(1).to(device)

              output = model(input_id, mask)
              ##print(output)
              ##print(output.argmax(dim=1))
              print(output.argmax(dim=1)[1])
              output2 = pd.DataFrame(output.argmax(dim=1)).astype("float")
                
              for i in range(50):
                if (50*round)+i<1143869:
                    results["classification"][(50*round)+i] = output2[0][i]
                    
              round = round + 1  
              print(round)
    results.to_csv('BERTClassifer_results.csv')
    
extrapolate(model, df_corpus)

