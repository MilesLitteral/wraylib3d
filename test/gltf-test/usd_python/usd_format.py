from pxr import Usd

stage = Usd.Stage.Open('a_usda_file.usda')
# do something to the stage

# This function allows you to transition between serialization formats (usd, usda or usdc) as well
# based on the file extension provided.
stage.Export('a_usdc_file.usdc') 
