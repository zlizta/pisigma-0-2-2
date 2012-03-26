
** Release notes for users (PiSigma Version 0.2.2)
   -----------------------------------------------
  
* Two interactive commands (:dr and :dw) have been added to interactively modify the ribbon and page width in an interactive PiSigma session.
  This allows to enhance the layout of printed PiSigma expressions in some situations.

* Simple syntax highlighting of expressions in PiSigma interactive session is restored. It works well within OS X terminals and in WinGHCi. 
  It is ugly in Windows consoles.

* Other minor changes have been made to the pretty printer to enhance the layout (e.g. smart printing \ x y x -> ... instead of \ x -> \ y -> \ z -> ...).


* WINDOWS CONSOLE and UTF8
  There has been issues with using UTF8 text encoding in Windows consoles. One which still exists at the time of writing (January 2012) is reported there:
  
  http://hackage.haskell.org/trac/ghc/ticket/4471
 
  
  At the moment, PiSigma for Windows is able to read files encoded in UTF8 correctly but when printing expressions, 
  it converts characters in identifier names which have a code higher than 127 to some string and underlines the names that have been modified.
  The conversion is inspired by Agda shortcuts used to write symbols in Emacs.
  
** Release notes for developpers (PiSigma Version 0.2.2)
   -----------------------------------------------------

* Modules have been reorgnized so as to avoid circularity and bootstrapping.

* Module Internal.hs which relied Data.ByteString to support UTF8 strings has been dropped and strings are now all native GHC ones 
  (GHC fully supports strings encoded in UTF8).

* More for debugging purposes, a PiSigma session starts by displaying information on the system environment.
  A function displayTestStrings is also available which attempts to print a string of symbols and
  warns the user if printing these symbols fails. You can uncomment its call in Mains.hs/main to perform the test each time you start PiSigma.
  This will be removed when issues with UTF8 support in Windows consoles are sorted out.
  
* The pretty printer is now based on Text.PrettyPrint.ANSI.Leijen which is well documented and supports basic ANSI commands. 
  This has been used to implement basic syntax hightlighting when outputting PiSigma terms. 
  Text.PrettyPrint.MPPPC.OneDim is no longer used.
  
* You might need to install some cabal packages manually at the moment, e.g.: 
  
  cabal install --global wl-pprint
  cabal install --global wl-pprint-text
  cabal install --global ansi-wl-pprint
  cabal install --global haskeline
  cabal install --global haskeline-class
  
* Many variables and functions in the Haskell source code have been renamed so as to better introduce the code to new developpers (whom we crucially need).

  