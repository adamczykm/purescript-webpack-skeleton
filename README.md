# purescript-web-pack-skeleton

[TODO]


-- automatic incremental purescript builds

    $ fswatch -o src -r | xargs -n 1 -I {} pulp build

-- automatic js bundling

    $ ./node_modules/webpack/bin/webpack.js --watch 
