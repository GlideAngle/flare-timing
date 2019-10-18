var webpack = require("webpack");
var path = require('path');

module.exports = {
    mode: 'development',
    entry: {
        'app-vincenty': path.join(__dirname, '.', 'app-vincenty.js'),
        'app-spherical': path.join(__dirname, '.', 'app-spherical.js')
    },
    resolve: {
        alias: {
            'jquery': path.join(__dirname, 'node_modules/jquery/dist/jquery'),
        },
        extensions: ['.webpack.js', '.js', '.css' ],
        modules: ['node_modules']
    },
    devtool: 'source-map',
    output: {
        path: path.resolve(__dirname, '../__www/test-js'),
        filename: '[name].js'
    },
    devServer: {
        contentBase: path.resolve(__dirname, "../__www/test-js"),
        compress: false,
        port: 9000
    },
    module: {
        rules: [ {
            test: /\.html$/,
            exclude: /node_modules/,
            loader: 'file-loader?name=[name].[ext]'
        }, {
            test: /\.(ttf|eot|svg|jpg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
            loader: 'file-loader' 
        }]
    }
};
