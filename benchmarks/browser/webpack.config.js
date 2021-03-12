const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');

module.exports =
  [ {
      entry: path.resolve(__dirname, './src/index.ts' ),
      output: {
          path: path.resolve(__dirname, 'public_html'),
          filename: 'script.js',
      },
      module: {
        rules: [{
          test: /\.(tsx|ts)$/,
          use: [{
            loader: 'ts-loader'
          }],
          exclude: [
            /(node_modules)/,
          ]
        },
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: {
            loader: 'elm-webpack-loader',
            options: {}
          }
        },
        {
          type: 'javascript/auto', // This makes sure Webpack doesn't complain about .wasm files
          test: /\.wasm$/i,
          loader: 'file-loader',
          options: {
            outputPath: 'wasm',
          }
        }]
      },
      resolve: {
        extensions: [ '.tsx', '.ts', '.js', '.elm', '.wasm' ]
      , alias: {
          '@microwasm': path.resolve( __dirname, '../../data/micro' ),
        }
      },
      plugins: [
          new HtmlWebpackPlugin( {
            template: './public_html/index.html'
          } )
      ],
      mode: 'production',
      devServer: {
        contentBase: path.join(__dirname, 'public_html'),
        compress: true,
        port: 8080
      }
    },
    {
      entry: path.resolve(__dirname, './src_worker/worker.ts' ),
      output: {
          path: path.resolve(__dirname, 'public_html'),
          filename: 'worker.js',
      },
      module: {
        rules: [{
          test: /\.(tsx|ts)$/,
          use: [{
            loader: 'ts-loader'
          }],
          exclude: [
            /(node_modules)/,
          ]
        }]
      },
      resolve: {
        extensions: [ '.tsx', '.ts', '.js' ]
      },
      mode: 'production'
    }
  ];