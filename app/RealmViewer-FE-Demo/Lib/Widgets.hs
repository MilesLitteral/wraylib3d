module Lib.Widgets where

--2d renderer pipeline
twoDimensionalRendererWidget :: BooksModel -> ALens' BooksModel Color -> ALens' BooksModel Color -> ALens' BooksModel Color -> ALens' BooksModel Color -> WidgetNode BooksModel BooksEvt
twoDimensionalRendererWidget model col1 col2 col3 col4 = vstack [
        hgrid [
        openGLWidget (model ^. color1) `styleBasic` [padding 20],
        openGLWidget (model ^. color2) `styleBasic` [padding 20]
        --{- scroll () `styleBasic` [width 800, height 800] -}
        ],
        hgrid [
        openGLWidget (model ^. color3) `styleBasic` [padding 20],
        openGLWidget (model ^. color4) `styleBasic` [padding 20]
        ],
        -- 2d Renderer Pipeline
        -- big kahuna goes here
        -- Directly Control The Rendering Viewports with a widget
        hstack [
        label "Color 1:",
        spacer,
        colorDropdown col1,
        spacer,
        label "Color 2:",
        spacer,
        colorDropdown col2,
        spacer,
        label "Color 3:",
        spacer,
        colorDropdown col3,
        spacer,
        label "Color 4:",
        spacer,
        colorDropdown col4
        ]
    ]

threeDimensionalRendererWidget :: WidgetNode BooksModel BooksEvt
threeDimensionalRendererWidget = hgrid [ codenameBigKahuna ]