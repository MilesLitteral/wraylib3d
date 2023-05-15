
{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.GuideWriter (demo) where

    -- Use this module to write guides, it will display it as LaTeX (.pdfs)
    -- Maybe this could be used as a basis for a Text system? rendering wise.
    import Data.Text
    import Text.LaTeX
    
    demo :: IO ()
    demo = execLaTeXT simple >>= renderFile "simple.tex"
    
    -- It's a good idea to separate the preamble of the body.
    simple :: Monad m => LaTeXT_ m
    simple = do
     thePreamble
     document theBody
    
    -- Preamble with some basic info.
    thePreamble :: Monad m => LaTeXT_ m
    thePreamble = do
     documentclass [] article
     author "Daniel Diaz"
     title "Simple example"
    
    -- Body with a section.
    theBody :: Monad m => LaTeXT_ m
    theBody = do
     maketitle
     section "Hello"
     "This is a simple example using the "
     hatex
     " library. "
     -- 'textbf' turns characters to bold font (as you already may know).
     textbf "Enjoy!"
     " "
     -- This is how we nest commands.
     textbf (large "Yoohoo!")