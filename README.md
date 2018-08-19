# purescript-web-pack-skeleton

[TODO] 

## Requirements

Automatic purescript builds are done using fswatch. You need to have it installed.


## Usage

First install node deps:

    $ npm install
    
Then bowers:

    $ bower install

Start automatic purescript + webpack builds with defined npm script:

    $ npm run dev

Which is equivalent to these commands:

1. automatic incremental purescript builds

       $ fswatch -o src -r | xargs -n 1 -I {} pulp build

2. automatic js bundling

       $ ./node_modules/webpack/bin/webpack.js --watch 
