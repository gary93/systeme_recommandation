#!/usr/bin/env python
# -*- coding: utf-8 -*-

from urllib.request import urlopen
import gzip

nom_fichier_donnees = 'dataset.json'

data = urlopen("http://snap.stanford.edu/data/amazon/productGraph/categoryFiles/reviews_Digital_Music_5.json.gz")

with open(nom_fichier_donnees,'wb') as fichier_final:
    print("Recupération du dataset archivé...")
    fichier_compresse = data.read()

    print("Sauvegarde du dataset decompressé...")
    contenu_fichier = gzip.decompress(fichier_compresse)
    fichier_final.write(contenu_fichier)
