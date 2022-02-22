#!/usr/bin/env python
# coding: utf-8

import matplotlib
matplotlib.use('Agg')
matplotlib.rcParams['figure.dpi'] = 72 #dpi manually set to match S2L machines

import matplotlib.pyplot as plt
import numpy as np
import librosa
import librosa.display
import pandas as pd
import IPython.display as display
import os

# MEL SPEC FXs
# FUNCTION 1
# define a function to clip a long recording using CNN params (SR = 22.05kHz)
# output should a array of shape: (n_clips, sr)
def clip_wav(wav_path = None, clip_duration = None, sample_rate= 22050, offset = None):
    
    x, sr = librosa.load(wav_path, sr = sample_rate)
    print(np.shape(x))
    no_clips = int(np.shape(x)[0] / (clip_duration*sample_rate))
    print('The number of clips here are:', no_clips)
    
    if(offset == None):
        x = x[:int(no_clips*clip_duration * sample_rate)]
    else:
        x = x[offset:int(no_clips*clip_duration*sample_rate)]
        no_clips = int(np.shape(x)[0] / (clip_duration*sample_rate))
        x = x[:int(no_clips*clip_duration * sample_rate)]
        
    #print(np.shape(x))
    no_clips = int(np.shape(x)[0] / (clip_duration*sample_rate))
    #print('The number of clips here are:', no_clips)
    
    o_p = np.reshape(x, (no_clips, int(clip_duration * sample_rate)))
    
    print('Final Shape:', np.shape(o_p))    
    return o_p


# FUNCTION 2
# take clipped wavs (or non-clipped) and produce mel spec for each sample clip with CNN params
def create_melSpecs(audio_samples = None, FMIN = 0, FMAX = 11025, HOP_SIZE = 128, N_MELS = 128, N_FFT = 1024+512, 
                    sample_rate = 22050, save_pth = None, mel_fname = None, offset = False):

    SR = sample_rate
    
    for i in range(len(audio_samples)): 
        
        y_21k = audio_samples[i]
    
        fig = plt.figure(1, frameon=False)
        fig.set_size_inches(6, 6)
        ax = plt.Axes(fig, [0., 0., 1., 1.])
        ax.set_axis_off()
        fig.add_axes(ax)

  
        S = librosa.feature.melspectrogram(y=y_21k, sr=22050, n_mels=128,fmin = 0,
                                     fmax=11025, n_fft=728, hop_length=32, htk = True) # hop length lesser the better

        librosa.display.specshow(librosa.power_to_db(S ** 1, ref=np.max), fmin=0, y_axis='linear')# , cmap = 'gray')
        
        if offset == True:
            directory = os.path.join(save_pth, mel_fname+str(i*2+1)+'.png')
        else:
            directory = os.path.join(save_pth, mel_fname+str((i)*2)+'.png')
        #print(directory)
        fig.savefig(directory)
        #plt.show()
        fig.clear()
        ax.cla()
        plt.clf()
        plt.close('all')

    print('No. of melSpecs created:', i+1)
