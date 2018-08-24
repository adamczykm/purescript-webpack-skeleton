# purescript-web-pack-skeleton

[TODO] 

## Requirements

Automatic purescript builds are done using inotifywait(linux)/fswatch(osx).
You need to have them installed.


## Usage

First install node deps:

    $ npm install
    
Then bowers:

    $ bower install


Start automatic purescript + webpack builds with defined npm script:

If you have purs-ide working go with:

    $ npm run dev

Otherwise use automatic rebuilding with inotifywait/fswatch prepared in scripts:

    $ npm run dev-nopurside
    
  or

    $ npm run dev-nopurside-osx
