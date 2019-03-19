# a simple utility to save to file and load back 
import pickle

def save(obj,file):
    """
    :param obj: object to be saved
    :param file: file name
    """
    with open(file,'w') as f:
       pickle.dump(obj,f,protocol=-1)

def load(file):
    """
    :param file: file name
    :return: the object read
    """
    with open(file) as f:
       return pickle.load(f)
    

    
