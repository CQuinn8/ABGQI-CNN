#!/usr/bin/env python
# coding: utf-8

# # Build spectrograms from wavs
# - Uses ROI_generation.yml env 
# - As is: builds 30 spectrogram pngs in directory with wav file name from a common wav directory (e.g., a filder with 20 wav files will result in 20 mel spec folders, each with 30 pngs)


import matplotlib
matplotlib.use('Agg')
matplotlib.rcParams['figure.dpi'] = 72 #dpi manually set to match S2L machines

import matplotlib.pyplot as plt
import numpy as np
import librosa
import librosa.display
import pandas as pd
import IPython.display as display

import pathlib
import os
import joblib
import sys
import glob

# S2L functions
from melspec_functions import *

# User inputs
# os.chdir('published_github_repo') # where github repo is cloned
out_dir = '5_ABGQI-CNN_deployment/data/melspecs/' # parent mel spec folder to write to
audio_dir = '5_ABGQI-CNN_deployment/data/wavs/' # folder containing wav files to read
print("Results parent dir = ", out_dir)
print('Audio wav dir = ', audio_dir)

# find out how many wavs to process
audio_files = [os.path.normpath(i) for i in glob.glob(audio_dir + '*')]
print("Length of audio file list:", len(audio_files))

# PROCESSING: MELSPEC GENERATION
# iterate through each wav file 
for no in range(len(audio_files)): 
    wav_nm = audio_files[no].split('\\')[-1] # get wav name
    mel_store = os.path.join(out_dir, wav_nm) # melspec folder with wav name
    wav_pth = os.path.join(audio_dir, wav_nm + '.wav') # full wav path name
    
    print('\nwav file', no, ':', wav_pth)   
    print('Mel dir name:', mel_store)

    if(os.path.exists(mel_store)):
        print(mel_store,'Already exists!')
        
        # check if there are 30 mels
        mel_files = glob.glob(mel_store + "/*.png")
        print("There are ",len(mel_files), "melspecs in the directory")
        
        # if there are not 30 specs, build them
        if len(mel_files) != 30 :
            print("calculating mel specs")
            clipped_wav = clip_wav(wav_path = wav_pth, clip_duration = 2.000, sample_rate= 22050, offset = None)
            create_melSpecs(audio_samples = clipped_wav, FMIN = 0, FMAX = 11025, HOP_SIZE = 128, N_MELS = 128, N_FFT = 1024+512, 
                              sample_rate = 22050, save_pth = mel_store, mel_fname = "", offset = False)
                              
        else:
            print("The directory is up to date")
    else:
        os.mkdir(mel_store)
    
        # even seconds (e.g. 0, 2, ..., 58)
        clipped_wav = clip_wav(wav_path = wav_pth, clip_duration = 2.000, sample_rate= 22050, offset = None)
        create_melSpecs(audio_samples = clipped_wav, FMIN = 0, FMAX = 11025, HOP_SIZE = 128, N_MELS = 128, N_FFT = 1024+512, 
                            sample_rate = 22050, save_pth = mel_store, mel_fname = "", offset = False)
                            
