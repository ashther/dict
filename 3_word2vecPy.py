# -*- coding: utf-8 -*-
"""
Created on Fri Mar 31 13:39:29 2017

@author: slj
"""

import logging
import sys
import os
import getopt
from gensim.models import Word2Vec
from gensim.models.word2vec import LineSentence

def main():
    inp = None
    outp = None
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hi:o:', 
                                   ['help', 'input=', 'output='])
    except getopt.GetoptError as err:
        print(err)
        sys.exit(2)
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            print('word2vecPy.py -i <input> -o <output>')   
            sys.exit()
        elif opt in ('-i', '--input'):
            inp = arg
        elif opt in ('-o', '--output'):
            outp = arg
    
    model = Word2Vec(LineSentence(inp))
    model.save(outp)
    

if __name__ == '__main__':
    program = os.path.basename(sys.argv[0])
    logger = logging.getLogger(program)
    
    logging.basicConfig(format='%(asctime)s:%(levelname)s:%(message)s', 
                        level=logging.INFO)
    # logging.root.setLevel(level=logging.INFO)
    logger.info('start running {}'.format(' '.join(sys.argv)))
    
    main()
    