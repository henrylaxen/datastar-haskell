# Haskell SDK for Datastar

After familiarizing yourself with the functionality of Datastar, this
haskell interface basically comes down to a few main functions
specified in the Datastar sdK

    mergeFragments  :: [Text] -> Selector -> MergeMode -> FragmentOptions -> Options -> Text
    removeFragments :: Selector  -> FragmentOptions -> Options -> Text
    mergeSignals    :: Text -> Bool -> Options -> Text
    removeSignals   :: [Text] -> Options -> Text
    executeScript   ::  [Text] -> [Text] -> Bool -> Options -> Text
    send :: Text -> Client is **Not** defined

additionally you, dear user, will need to implement a web server
dependent function sends the text you created to the client. I have
included a sample implementation for the Snap server, in the SnapSSE
module, please have a look at it.

You will notice a Bool named debug, which is currently set to False.
Setting it to True will enable debug messages printed to stdout so
you can see what is being sent to the client (web browser)

Finally, the executable, which you can try out by typing "cabal run"
and pointing your browser at:
  http://localhost:8000/
will give you a simple demo of some Datastar features and show that
streaming SSE events to the browser works. 

The code itself is extensively documented, with many doctest examples
that show up in the Haddock files.  
