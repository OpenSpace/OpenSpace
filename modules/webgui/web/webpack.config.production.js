const { resolve } = require('path');
const webpack = require('webpack');

module.exports = {
    entry: './src/index.jsx',

    output: {
        filename: 'static/bundle.js',
        // the output bundle

        path: resolve(__dirname, 'dist'),

        publicPath: '/'
        // necessary for HMR to know where to load the hot update chunks
    },

    devtool: 'source-map',

    module: {
      rules: [
        // Load JS!
        {
          test: /\.jsx?$/,
          use: ['babel-loader'],
          exclude: /node_modules/,
        },
        // Load SASS!
        {
          test: /\.scss$/,
          exclude: /node_modules/,
          use: [
            {
              loader: 'style-loader',
            }, {
              loader: 'css-loader?modules',
              options: {
                sourceMap: true,
                importLoaders: 2,
                modules: true,
                localIdentName: '[path][name]-[local]',
                minimize: false,
              },
            }, {
              loader: 'sass-loader',
              options: {
                sourceMap: true,
              },
            },
          ],
        },
        {
          test: /\.css$/,
          use: ['style-loader', 'css-loader'],
        },
        {
          test: /\.(woff|woff2|ttf|eot)$/,
          loader: 'file-loader',
          options: {
            name: 'fonts/[name].[ext]',
          },
        },
        {
          test: /\.(png)$/,
          loader: 'file-loader',
          options: {
            name: 'img/[name].[ext]',
          },
        },
      ],
    },

    resolve: {
        extensions: ['.js', '.jsx'],
    }
};
