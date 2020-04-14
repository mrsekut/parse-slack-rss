{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Text.HTML.Scalpel       hiding ( URL )
import           Data.Maybe
import           Text.Printf


main :: IO ()
main = do
    html <- readFile "index.html"
    let lists = scrapeStringLike html allBlog

    printf "組織\n"
    mapM_ print $ maybe [] (filter (\(Blog _ _ t) -> t)) lists

    printf "\n個人\n"
    mapM_ print $ maybe [] (filter (\(Blog _ _ t) -> not t)) lists


type Title = String
type URL = String
type IsIndividual = Bool -- "tech-blog" or "tech-blog-individual"

data Blog = Blog Title URL Bool

instance Show Blog where
    show (Blog title url _) = printf " [%s %s]" url title


allBlog :: Scraper String [Blog]
allBlog = chroots
    (  "div"
    @: [ hasClass "col"
       , hasClass "span_5_of_12"
       , hasClass "small"
       , hasClass "bottom_margin"
       , hasClass "no_padding"
       ]
    )
    blog
  where
    blog :: Scraper String Blog
    blog = do
        title <- attr "title" $ "a" @: [hasClass ""]
        url   <- attr "href" $ "a" @: [hasClass ""]
        tag   <- text $ ("div" @: [hasClass "show_on_mobile"]) // "strong"
        return $ Blog title url $ tag == "#tech-blog-individualに投稿します"


