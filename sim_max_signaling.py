from scipy import *
from Tkinter import *
from tkSimpleDialog import *

import copy
import csv
from datetime import datetime
import sys
import math

def max_index(l): # returns the index with maximal value
    # if there are several maxima, one is picked at random
    m = [i for i in range(len(l)) if l[i]==max(l)]
    if len(m)==1:
        return m[0]
    return m[random.randint(len(m))]

NHues = 41 # number of hues
NValues = 10 # number of values
NMeanings = 330 # values 0 and 9 have only 1 hue (0)

if len(sys.argv) < 4:
    print "Usage: python", sys.argv[0], "<number of messages> <maximum number of generations> <output filename>"
    sys.exit(1)

NForms = int(sys.argv[1])
NGenerations = int(sys.argv[2])
output_filename = sys.argv[3]

# read CIELAB coordinates from file
CIELABCoord = []
CIELABFile = 'perceptual-space-with-sphere.csv'
with open(CIELABFile, 'r') as f:
    reader = csv.reader(f)
    reader.next() # skip header
    for row in reader:
        # proper CIELAB coordinates
        CIELABCoord.append((float(row[7]),float(row[8]),float(row[9])))
        # sphere coordinates
        #CIELABCoord.append((float(row[15]),float(row[14]),float(row[13])))

Sim = zeros((NMeanings,NMeanings))
for x1 in range(NMeanings):
    for x2 in range(NMeanings):
        coord1 = CIELABCoord[x1]
        coord2 = CIELABCoord[x2]
        L1 = coord1[0]
        L2 = coord2[0]
        a1 = coord1[1]
        a2 = coord2[1]
        b1 = coord1[2]
        b2 = coord2[2]
        dist12 = sqrt((L1-L2)**2 + (a1-a2)**2 + (b1-b2)**2)
        Sim[x1][x2] = exp(-0.001 * (dist12**2))

# uniform priors
PMeaning = ones(NMeanings)

PMeaning /= sum(PMeaning)

Speakers = random.random((NMeanings,NForms))
Hearers = random.random((NForms,NMeanings))

for i in range(NMeanings):
    Speakers[i] /= sum(Speakers[i])
for i in range(NForms):
    Hearers[i] /= sum(Hearers[i])

for g in xrange(NGenerations):

    print 'Generation: ', str(g)

    SpeakersBefore = copy.deepcopy(Speakers)
    HearersBefore = copy.deepcopy(Hearers)

    # Calculate Expected Utilities
    uS = zeros((NMeanings,NForms))
    uH = zeros((NForms,NMeanings))
    for m in range(NMeanings):
        for f in range(NForms):
            uS[m][f] = dot(Hearers[f],Sim[m])

    for f in range(NForms):
        for m in range(NMeanings):
            uH[f][m]= dot(PMeaning*Speakers[:,f],Sim[m])

    # Replicator Dynamics on Behavioral Strategies
    for t in range(NMeanings):
        for m in range(NForms):
            Speakers[t,m] = Speakers[t,m] * (uS[t,m] / (sum(uS[t]) / NForms))
            Hearers[m,t] = Hearers[m,t] * (uH[m,t] / (sum(uH[m]) / NMeanings))

    # Readjusting, Normalizing
    for i in range(NMeanings):
        if sum(Speakers[i]) == 0: 
            Speakers[i] += 1
        Speakers[i] /= sum(Speakers[i])
    for i in range(NForms):
        if sum(Hearers[i]) == 0: Hearers[i] += 1
        Hearers[i] /= sum(Hearers[i])

    if sum(abs(Speakers - SpeakersBefore)) < 0.1 and sum(abs(Hearers - HearersBefore)) < 0.1:
        print 'Converged!\a'
        break

ModeMap = zeros(NMeanings,int)
for x in range(NMeanings):
        ModeMap[x] = max_index(Speakers[x])

with open(output_filename, 'a') as mode_map_file:
    writer = csv.writer(mode_map_file)
    time = str(datetime.today())
    writer.writerow([time, 0, 'A', 't' + str(ModeMap[0])])
    for r in range(NValues-2):
        for c in range(NHues):
            writer.writerow([time, c, ['B','C','D','E','F','G','H','I'][r], 't' + str(ModeMap[1+r*NHues+c])])
    writer.writerow([time, 0, 'J', 't' + str(ModeMap[329])])

