const path = require('path');

module.exports = {
  module: {
    rules: [
      {
        test: /\.scss$/,
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
        include: path.resolve(__dirname, '../')
      }, {
        test: /\.css$/,
        use: ['style-loader', 'css-loader'],
      }, {
        test: /\.(woff|woff2|ttf|eot)$/,
        loader: 'file-loader',
        options: {
          name: 'fonts/[name].[ext]',
        },
      }
    ]
  }
}
