'use strict';

const path = require('path');
const webpack = require('webpack');
const HardSourceWebpackPlugin = require('hard-source-webpack-plugin');

const isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');

const isWatch = process.argv.some(a => a === '--watch');

const plugins =
  isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
;

module.exports = {
  mode: 'development',


  devtool: 'original-source',
  // devtool: 'eval-source-map',

  devServer: {
    contentBase: '.',
    port: 4008,
    stats: 'errors-only'
  },

  entry: './app.js',

  output: {
    path: path.resolve(__dirname, 'dist'),
    pathinfo: true,
    filename: 'bundle.js'
  },
  plugins: [
    new HardSourceWebpackPlugin()
  ]

  // module: {
  //   rules: [
  //     {
  //       test: /\.purs$/,
  //       use: [
  //         {
  //           loader: 'purs-loader',
  //           options: {
  //             src: [
  //               'bower_components/purescript-*/src/**/*.purs',
  //               'src/**/*.purs'
  //             ],
  //             bundle: false,
  //             psc: 'psa',
  //             watch: isWebpackDevServer || isWatch,
  //             pscIde: true
  //           }
  //         }
  //       ]
  //     },
  //   ]
  // },

  // resolve: {
  //   modules: [ 'node_modules', 'bower_components' ],
  //   extensions: [ '.purs', '.js']
  // },

  // plugins: [
  //   new webpack.LoaderOptionsPlugin({
  //     debug: false
  //   })
  // ].concat(plugins)
};
