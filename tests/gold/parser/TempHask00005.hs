{-# LANGUAGE QuasiQuotes #-}
module TempHask00005 where

instance Yesod App where
    defaultLayout widget = do
        -- Parse names starting with a single quote in splices.
        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
