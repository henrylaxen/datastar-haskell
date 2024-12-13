# Haskell SDK for Datastar

After familiarizing yourself with the functionality of Datastar, the
haskell interface basically comes down to one main function, **makeDatastar**

makeDatastar takes as its first argument a Command, one of:
  MergeFragments, RemoveFragments, MergeSignals, RemoveSignals, or ExecuteScript

it combines this with a non empty list of ByteStrings, which specify
the arguments of the Command above, and an optional set of arguments,
enclosed in a Maybe.  That's all.  The result of **makeDatastar** is a
byteString ready to be sent to the Browser.

In the words of Hamlet, "aye, there's the rub".  For in the Haskell
software-verse, there are many choices for web servers:
  Snap, Scotty, Servent, Warp, Yesod, Mighttpd2, Wai, Happstack

I'm most familiar with Snap, and have included code that uses Snap and
Warp to send a stream of messages to the browser, using SSE events.

You might also have a look at the *Main.hs* file located in the test
directory.  I've included several examples of Datastar commands that
are properly formatted for your amusement and edification.

Finally, the executable, which you can try out by typing "cabal run"
and pointing your browser at:
  http://localhost:3000/
will give you a simple demo of some Datastar features and show that
streaming SSE events to the browser works.

