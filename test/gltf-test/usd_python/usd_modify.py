from pxr import Usd

stage = Usd.Stage.Open('path_to_file.usd')
# do something to the stage
stage.Save()  
