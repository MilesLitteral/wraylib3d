
{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Utils.GuideWriter (demo) where

    import Text.LaTeX ( 
        article,
        author,
        document,
        documentclass,
        hatex,
        large,
        maketitle,
        section,
        textbf,
        title,
        renderFile,
        execLaTeXT,
        LaTeXT_ 
      )
    
    -- Use this module to write guides, it will display it as LaTeX (.pdfs)
    -- Maybe this could be used as a basis for a Text system? rendering wise.
    -- It needs a generic version of preamble and body to generate text as needed.
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
     title  "Simple example"
     hatex
    
    -- Body with a section.
    theBody :: Monad m => LaTeXT_ m
    theBody = do
     maketitle
     section "Hello"
     "This is a simple example using the "
     "hatex library. "
     -- 'textbf' turns characters to bold font (as you already may know).
     textbf "Enjoy!"
     " "
     -- This is how we nest commands.
     textbf (large "Yoohoo!")