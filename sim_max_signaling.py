import numpy as np
from numpy import random as random
from Tkinter import *
from tkSimpleDialog import *

import copy
import csv
from datetime import datetime
import sys
import math

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from reportlab.pdfbase import cidfonts

from joblib import Memory
memory = Memory(cachedir='tmp', verbose=0)

def read_csv_file(filename, cols, skip_header=True):
    table = []
    with open(filename, 'r') as f:
        reader = csv.reader(f)
        if skip_header:
            reader.next()
        for row in reader:
            table.append([ float(row[col]) for col in cols ])
    return(np.array(table))

def write_csv_file(table, cols, filename, header=None):
    with open(filename, 'w') as file:
        writer = csv.writer(file)
        if header != None:
            writer.writerow(header)
        for r in xrange(len(table)):
            writer.writerow([ table[r, c] for c in cols ])

def similarity(x1, x2):
    L1 = x1[0]
    L2 = x2[0]
    a1 = x1[1]
    a2 = x2[1]
    b1 = x1[2]
    b2 = x2[2]
    dist12 = np.sqrt((L1 - L2) ** 2 + (a1 - a2) ** 2 + (b1 - b2) ** 2)
    return(np.exp(-0.001 * (dist12 ** 2)))

@memory.cache
def similarity_matrix(Lab1, Lab2):
    print 'Calculating similarity matrix of size ' + str(len(Lab1)) + 'x' + str(len(Lab2)) + ' (can take a while)...'
    Sim = np.zeros((len(Lab1), len(Lab2)))
    for x1 in xrange(len(Lab1)):
        for x2 in xrange(len(Lab2)):
            Sim[x1][x2] = similarity(Lab1[x1], Lab2[x2])
    return(Sim)

def speaker_mode_map(Lab1, Strategy, Lab2=None, Similarity12=None):
    if Lab2 == None:
        Lab2 = Lab1
    NPoints = len(Lab2)
    ModeMap = np.zeros(NPoints, int)
    for x in xrange(NPoints):
        if Similarity12 == None:
            y = x
        else:
            y = np.argmax(Similarity12[x])
        ModeMap[x] = np.argmax(Strategy[y])
    return(ModeMap)

def plot(PerceptualSpace, Priors, MunsellPalette, PerceptualModeMap, MunsellModeMap, block=False):
    plt.clf()
#    fig = plt.figure()
    ax1 = plt.subplot(2, 3, 1, projection='3d')
    ax1.scatter(PerceptualSpace[:, 1], PerceptualSpace[:, 2], PerceptualSpace[:, 0], c=Priors)
    ax2 = plt.subplot(2, 3, 2, projection='3d')
    ax2.scatter(PerceptualSpace[:, 1], PerceptualSpace[:, 2], PerceptualSpace[:, 0], c=PerceptualModeMap)
    ax3 = plt.subplot(2, 3, 3, projection='3d')
    ax3.scatter(MunsellPalette[:, 1], MunsellPalette[:, 2], MunsellPalette[:, 0], c=MunsellModeMap)
    ax3.set_xlim(ax1.get_xlim())
    ax3.set_ylim(ax1.get_ylim())
    ax3.set_zlim(ax1.get_zlim())
#        for m in xrange(NForms):
#            ax = plt.subplot(3, NForms, NForms + 1 + m, projection='3d')
#            ax.scatter(PerceptualSpace[:, 1], PerceptualSpace[:, 2], PerceptualSpace[:, 0], c=Speakers[:, m])
#            ax = plt.subplot(3, NForms, 2 * NForms + 1 + m, projection='3d')
#            ax.scatter(PerceptualSpace[:, 1], PerceptualSpace[:, 2], PerceptualSpace[:, 0], c=Hearers[m])
    tmpModeMap = MunsellModeMap[range(1, len(MunsellModeMap) - 1)]
    tmpModeMap.shape = (8, 41)
    ax4 = plt.subplot(2, 1, 2)
    plt.imshow(tmpModeMap, interpolation='none')
    plt.show(block=block)
    plt.pause(0.01)

def run_simulation(PerceptualSpace, PMeaning, NForms, NGenerations, output_filename):
    MunsellPalette = read_csv_file('perceptual-space-with-sphere.csv', [7, 8, 9])
    
    NMeanings = len(PerceptualSpace)
    
    Sim = similarity_matrix(PerceptualSpace, PerceptualSpace)
    Sim2 = similarity_matrix(MunsellPalette, PerceptualSpace)

    Speakers = random.random((NMeanings, NForms))
    Hearers = random.random((NForms, NMeanings))
    
    for i in xrange(NMeanings):
        Speakers[i] /= np.sum(Speakers[i])
    for i in xrange(NForms):
        Hearers[i] /= np.sum(Hearers[i])

    for g in xrange(NGenerations):
    
        print 'Generation: ', str(g)
        
        PerceptualModeMap = speaker_mode_map(PerceptualSpace, Speakers)
        MunsellModeMap = speaker_mode_map(PerceptualSpace, Speakers, MunsellPalette, Sim2)
        
        plot(PerceptualSpace, PMeaning, MunsellPalette, PerceptualModeMap, MunsellModeMap)
    
        SpeakersBefore = copy.deepcopy(Speakers)
        HearersBefore = copy.deepcopy(Hearers)
    
        # Calculate Expected Utilities
        uS = np.zeros((NMeanings, NForms))
        uH = np.zeros((NForms, NMeanings))
        for m in xrange(NMeanings):
            for f in xrange(NForms):
                uS[m][f] = np.dot(Hearers[f], Sim[m])
    
        for f in xrange(NForms):
            for m in xrange(NMeanings):
                uH[f][m] = np.dot(PMeaning * Speakers[:, f], Sim[m])
    
        # Replicator Dynamics on Behavioral Strategies
        for t in xrange(NMeanings):
            for m in xrange(NForms):
                Speakers[t, m] = Speakers[t, m] * (uS[t, m] / (np.sum(uS[t]) / NForms))
                Hearers[m, t] = Hearers[m, t] * (uH[m, t] / (np.sum(uH[m]) / NMeanings))
    
        # Readjusting, Normalizing
        for i in xrange(NMeanings):
            if np.sum(Speakers[i]) == 0: 
                Speakers[i] += 1
            Speakers[i] /= np.sum(Speakers[i])
        for i in xrange(NForms):
            if np.sum(Hearers[i]) == 0: Hearers[i] += 1
            Hearers[i] /= np.sum(Hearers[i])
    
        if np.sum(abs(Speakers - SpeakersBefore)) < 0.01 and np.sum(abs(Hearers - HearersBefore)) < 0.01:
            print 'Converged!\a'
            break
    
    plot(PerceptualSpace, PMeaning, MunsellPalette, PerceptualModeMap, MunsellModeMap, block=True)
    
    MunsellModeMap = speaker_mode_map(PerceptualSpace, Speakers, MunsellPalette, Sim2)
    
    NHues = 41  # number of hues
    NValues = 10  # number of values
    
    with open(output_filename, 'a') as mode_map_file:
        writer = csv.writer(mode_map_file)
        time = str(datetime.today())
        writer.writerow([time, 0, 'A', 't' + str(MunsellModeMap[0])])
        for r in xrange(NValues - 2):
            for c in xrange(NHues):
                writer.writerow([time, c, ['B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'][r], 't' + str(MunsellModeMap[1 + r * NHues + c])])
        writer.writerow([time, 0, 'J', 't' + str(MunsellModeMap[329])])


if __name__ == "__main__":
    if len(sys.argv) < 4:
        print "Usage: python", sys.argv[0], "<perceptual space filename> <number of messages> <maximum number of generations> <output filename>"
        sys.exit(1)
    else:
        PerceptualSpaceFilename = sys.argv[1]
        NForms = int(sys.argv[2])
        NGenerations = int(sys.argv[3])
        output_filename = sys.argv[4]

        PerceptualSpace = read_csv_file(PerceptualSpaceFilename, [0, 1, 2])
        PMeaning = read_csv_file(PerceptualSpaceFilename, [3])[:,0]
        
        run_simulation(PerceptualSpace, PMeaning, NForms, NGenerations, output_filename)
