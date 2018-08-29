const { resolve } = require('path');
const webpack = require('webpack');

const PublicPath = '/';

module.exports = {
  mode: 'development',
  context: resolve(__dirname, 'src'),
  entry: [
    'react-hot-loader/patch',
    'webpack-dev-server/client?http://localhost:8080',
    'webpack/hot/only-dev-server',
    './index.jsx',
  ],
  output: {
    filename: 'static/bundle.js',
    path: resolve(__dirname, 'dist'),
    publicPath: PublicPath,
  },
  devtool: 'inline-source-map',
  devServer: {
    historyApiFallback: true,
    hot: true,
    contentBase: resolve(__dirname, 'dist'),
    publicPath: PublicPath,
  },
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
  },
  plugins: [
    // enable HMR globally
    new webpack.HotModuleReplacementPlugin(),
    // prints more readable module names in the browser console on HMR updates
    new webpack.NamedModulesPlugin(),
  ],
};
