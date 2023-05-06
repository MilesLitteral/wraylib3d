module HRayLib3d.GuideWriter (demo) where

    import Text.LaTeX

    demo =
        documentclass [] article
        <> title "A short message"
        <> author "John Short"
        <> document (maketitle <> "This is all.")