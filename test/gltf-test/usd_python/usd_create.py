from pxr import Usd
import argparse 

parser = argparse.ArgumentParser(description='Process some integers.')
parser.add_argument('usdPath', metavar='N', type=str, nargs='+',
                    help='an integer for the accumulator')

args    = parser.parse_args()
usdPath =  f'{args.usdPath}.usd'
Usd.Stage.CreateNew(usdPath)  
Usd.Stage.Save(f"./exports/{usdPath}")

#To save a loaded Stage use Usd.Stage.Save(path)
