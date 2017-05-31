const { resolve } = require('path');
const webpack = require('webpack');

module.exports = {
    entry: './src/index.js',

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
            {
                test: /\.jsx?$/,
                use: [ 'babel-loader', ],
                exclude: /node_modules/
            },
            {
                test: /\.css$/,
                use: [ 'style-loader', 'css-loader?modules', ],
            },
        ],
    },

    plugins: [
        new webpack.optimize.UglifyJsPlugin({
            sourceMap: true,
            comments: false
        })
    ]
};