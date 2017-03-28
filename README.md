!! NOT USABLE AS IS !!

This project is a work in progress.

# λ-tube

## Screenshot

## What is λ-tube?

λ-tube is a video sharing website engine written in Haskell using the Scotty web framework.

## Features

* Multiple channels
* Basic account system
* View counting

Features coming soon:

* Comments
* Automatic thumbnails
* RSS feeds
* Admin tools and video flagging

## To install

First, get ghc and cabal and then install the required modules:

  cabal install scotty hastache sqlite-simple sha

ffmpeg is required for thumbnailing.

Finally, rename Database.db.empty to Database.db.

## To use

Simply run:

  runhaskell Main.hs
