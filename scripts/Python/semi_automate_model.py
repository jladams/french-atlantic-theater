#!/usr/bin/env python
# coding: utf8
"""Example of training an additional entity type

This script shows how to add a new entity type to an existing pretrained NER
model. To keep the example short and simple, only four sentences are provided
as examples. In practice, you'll need many more — a few hundred would be a
good start. You will also likely need to mix in examples of other entity
types, which might be obtained by running the entity recognizer over unlabelled
sentences, and adding their annotations to the training set.

The actual training is performed by looping over the examples, and calling
`nlp.entity.update()`. The `update()` method steps through the words of the
input. At each word, it makes a prediction. It then consults the annotations
provided on the GoldParse instance, to see whether it was right. If it was
wrong, it adjusts its weights so that the correct action will score higher
next time.

After training your model, you can save it to a directory. We recommend
wrapping models as Python packages, for ease of deployment.

For more details, see the documentation:
* Training: https://spacy.io/usage/training
* NER: https://spacy.io/usage/linguistic-features#named-entities

Compatible with: spaCy v2.1.0+
Last tested with: v2.2.4
"""
from __future__ import unicode_literals, print_function

import plac
import random
import warnings
from pathlib import Path
import spacy
from spacy.util import minibatch, compounding

import os
from os.path import isfile, join
import glob
import pandas as pd

dataframe_path = '/path/to/your/master/csv/file'
trained_path = "/path/to/your/training/documents/"
os.chdir(trained_path)

extension = 'csv'
#iterate through all files
all_filenames = [i for i in glob.glob('*.{}'.format(extension))]
#combine all files in the list
combined_csv = pd.concat([pd.read_csv(f) for f in all_filenames ], sort=False)

PRE_LABEL = list(combined_csv['token_tag_'].unique())

# new entity label

# training data
# Note: If you're using an existing model, make sure to mix in examples of
# other entity types that spaCy correctly recognized before. Otherwise, your
# model might learn the new type, but "forget" what it previously knew.
# https://explosion.ai/blog/pseudo-rehearsal-catastrophic-forgetting

final_df = pd.read_csv(dataframe_path)
final_df['DATE'] = pd.to_datetime(final_df['DATE'], infer_datetime_format=True)


#This section of code generates training data from the tagged csv files
TRAIN_DATA = []
corpus = (f for f in os.listdir(trained_path) if not f.startswith('.') and isfile(join(trained_path, f)))
for filename in corpus:
    try:
        date = filename[-14:-4]
        #create a new dataframe from the trained csv file
        df = pd.read_csv(filename)
        #generate a dataframe that aligns an entity tag with the starting and ending position of the word
        tags = df.groupby(['token_pos_','token_tag_']).agg({'token_start': 'min', 'token_end': 'max'})
        tags = tags.reset_index()
        entity_list = []
        #turn the dataframe into a tuple with the starting, ending position, and token entity tag
        tags['Tuple'] = list(zip(tags.token_start, tags.token_end, tags.token_tag_))
        series = tags['Tuple']
        #retrieve the text from the date
        text = final_df['TEXT'][final_df['DATE'] == date].values
        #iterate through entities to create a training set that spaCy
        #training model requires to create a custom nlp model
        for start, end, tag in series:
            tup = (start, end,tag)
            entity_list.append(tup)

            train = (text,{"entities": entity_list})
        TRAIN_DATA.append(train)
    except:
        print('Encountered error with', filename)
        continue
print('Generated Training Set')

#create a list of entity labels and then choose a language models
#for your training model
LABEL = [x for x in PRE_LABEL if type(x) is not float]
@plac.annotations(
    model=("fr_core_news_sm", "option", "m", str),
    new_model_name=("performance", "option", "nm", str),
    output_dir=("/path/to/your/training/model", "option", "o", Path),
    n_iter=("100", "option", "n", int),
)
def main(model=None, new_model_name="performance", output_dir=None, n_iter=200):
    """Set up the pipeline and entity recognizer, and train the new entity."""
    random.seed(0)
    if model is not None:
        nlp = spacy.load(model)  # load existing spaCy model
        print("Loaded model '%s'" % model)
    else:
        nlp = spacy.blank("fr")  # create blank Language class
        print("Created blank 'fr' model")
    # Add entity recognizer to model if it's not in the pipeline
    # nlp.create_pipe works for built-ins that are registered with spaCy
    if "ner" not in nlp.pipe_names:
        ner = nlp.create_pipe("ner")
        nlp.add_pipe(ner)
    # otherwise, get it, so we can add labels to it
    else:
        ner = nlp.get_pipe("ner")

    for lbl in LABEL:
        ner.add_label(lbl)  # add new entity label to entity recognizer
    # Adding extraneous labels shouldn't mess anything up
    if model is None:
        optimizer = nlp.begin_training()
    else:
        optimizer = nlp.resume_training()
    move_names = list(ner.move_names)
    # get names of other pipes to disable them during training
    pipe_exceptions = ["ner", "trf_wordpiecer", "trf_tok2vec"]
    other_pipes = [pipe for pipe in nlp.pipe_names if pipe not in pipe_exceptions]
    # only train NER
    with nlp.disable_pipes(*other_pipes), warnings.catch_warnings():
        # show warnings for misaligned entity spans once
        warnings.filterwarnings("once", category=UserWarning, module='spacy')

        sizes = compounding(1.0, 4.0, 1.001)
        # batch up the examples using spaCy's minibatch
        for itn in range(n_iter):
            random.shuffle(TRAIN_DATA)
            batches = minibatch(TRAIN_DATA, size=sizes)
            losses = {}
            for batch in batches:
                texts, annotations = zip(*batch)
                nlp.update(texts, annotations, sgd=optimizer, drop=0.35, losses=losses)
            print("Losses", losses)

    # test the trained model
    test_text = "Place your test text here"
    doc = nlp(test_text)
    print("Entities in '%s'" % test_text)

    for ent in doc.ents:
        print(ent.label_, ent.text)

    # save model to output directory
    nlp.to_disk('/Users/f00230z/repertoire/model')
    if output_dir is not None:
        output_dir = Path(output_dir)
        if not output_dir.exists():
            output_dir.mkdir()
        nlp.meta["name"] = new_model_name  # rename model
        nlp.to_disk(output_dir)
        print("Saved model to", output_dir)

        # test the saved model
        print("Loading from", output_dir)
        nlp2 = spacy.load(output_dir)
        # Check the classes have loaded back consistently
        assert nlp2.get_pipe("ner").move_names == move_names
        doc2 = nlp2(test_text)
        for ent in doc2.ents:
            print(ent.label_, ent.text)





if __name__ == "__main__":
    plac.call(main)
