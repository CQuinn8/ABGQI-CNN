#!/usr/bin/env python
# coding: utf-8

import matplotlib
matplotlib.use('Agg') # Cam Bodine's genius solution to output plots!!
matplotlib.rcParams['figure.dpi'] = 72 #dpi manually set to match Shree's machine

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import IPython.display as ipd
import os
import joblib
import glob
import time
from datetime import date
'''
%env CUDA_DEVICE_ORDER=PCI_BUS_ID
%env CUDA_VISIBLE_DEVICES=1
'''
import tensorflow as tf
import IPython.display as display
from PIL import Image
import pathlib
import sys

# ON WINDOWS OS10 with TF 2.3.0 + mobilenet make the GPU 'invisible'
os.environ["CUDA_VISIBLE_DEVICES"] = "-1"
AUTOTUNE = tf.data.experimental.AUTOTUNE
tf.__version__


# USER INPUTS
os.chdir('published_github_repo')
melspec_dirs = [os.path.normpath(i) for i in glob.glob('5_ABGQI-CNN_deployment/data/melspecs/*')]
results_dir = '5_ABGQI-CNN_deployment/results/predictions/'
model_path = 'ABGQI-CNN'


# FUNCTIONS
# function that interpretes image after being provided a path in process_path
def decode_img(img, IMG_HEIGHT, IMG_WIDTH):
  # convert the compressed string to a 3D uint8 tensor
  img = tf.image.decode_jpeg(img, channels=3)
  # Use `convert_image_dtype` to convert to floats in the [0,1] range.
  img = tf.image.convert_image_dtype(img, tf.float32)
  # resize the image to the desired size.
  return tf.image.resize(img, [IMG_HEIGHT, IMG_WIDTH])

# function to read in a file path
def process_path(file_path, IMG_HEIGHT, IMG_WIDTH):
    # load the raw data from the file as a string
    img = tf.io.read_file(file_path)
    img = decode_img(img, IMG_HEIGHT, IMG_WIDTH)
    return img #, label


# Load in model (not checkpoint)
model = tf.keras.models.load_model(model_path)
IMG_HEIGHT = 224
IMG_WIDTH = 224

model.summary()
print('Number of folders in the parent Mel-Spec directory:',len(melspec_dirs))


# iterate through each folder/wav file which contains mfccs
for i in range(len(melspec_dirs)):

    temp_wav = melspec_dirs[i].split('\\')[-1] # get wav name
    out_path_temp = os.path.join(results_dir, temp_wav + '.csv')
    print("Wav name:",temp_wav)

    # does the prediction exist already?
    if os.path.isfile(out_path_temp):
        print("WAV ALREADY PREDICTED") 
        continue                  
    else:
        # if not, what is the mel spec directory          
        mel_store = melspec_dirs[i]
        print("MFCC dir:", mel_store)

        mel_store_lst = os.listdir(mel_store) # temp dir with the PNGs
        loop_run = len(mel_store_lst)
        print("Number of mfccs in current dir =", loop_run)

        sigmoid_pred_lst = []
        mel_names = []
        # iterate through each melspec in current file directory
        for j in range(loop_run):
            fname = str(j*2)+'.png'
            mel_names.append(fname)

            img_ = process_path(os.path.join(mel_store, fname), IMG_HEIGHT = 224, IMG_WIDTH = 224)
            img_ = tf.reshape(img_, shape= (1, IMG_HEIGHT, IMG_WIDTH, 3))

            # get predictions
            pred = model.predict(img_, verbose=0, steps=1, callbacks=None, max_queue_size=10, 
                                 workers=1, use_multiprocessing=False)

            sigmoid_pred = tf.math.sigmoid(pred).numpy() # sigmoid here            
            sigmoid_pred_lst.append(sigmoid_pred)

        # format predictions
        flat_sigmoid = [item for sublist in sigmoid_pred_lst for item in sublist]
        df_sigmoid = pd.DataFrame(flat_sigmoid)
        df_sigmoid = pd.concat([pd.Series(mel_names), df_sigmoid], axis=1)
        df_sigmoid.columns = ["melspec", "Anthropophony", "Biophony", "Geophony", "Interference", "Quiet"]

        # save predictions
        df_sigmoid.to_csv(out_path_temp, index = False)        
        print('Saved this file at:', out_path_temp, "Length was:",len(sigmoid_pred_lst))

