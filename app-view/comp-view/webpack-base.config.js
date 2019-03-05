var webpack = require("webpack");
var path = require('path');
var ExtractTextPlugin = require('extract-text-webpack-plugin'); 
var extractScss = new ExtractTextPlugin({ filename: 'styles.css' });

module.exports = {
    mode: 'production',
    entry: {
        app: path.join(__dirname, '.', 'app.js')
    },
    externals: /(all|rts|lib|out|runmain).js$/,
    resolve: {
        extensions: ['.webpack.js', '.js', '.css', '.less', '.scss' ],
        modules: ['node_modules']
    },
    devtool: 'source-map',
    output: {
        path: path.resolve(__dirname, '../../__www-dist-ghcjs/task-view'),
        filename: '[name].js'
    },
    devServer: {
        contentBase: path.resolve(__dirname, "../../__www-dist-ghcjs/task-view"),
        compress: true,
        port: 9000
    },
    module: {
        noParse: /(all|rts|lib|out|runmain).js$/,
        rules: [ {
            test: /\.html$/,
            exclude: /node_modules/,
            loader: 'file-loader?name=[name].[ext]'
        }, {
            test: /\.css$/,
            loader: ExtractTextPlugin.extract([ 'style-loader', 'css-loader' ])
        }, {
            test: /\.sass$/,
            loader: extractScss.extract([ 'css-loader', 'sass-loader' ])
        }, {
            test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
            loader: 'url-loader?limit=10000&minetype=application/font-woff'
        }, {
            test: /(all|rts|lib|out|runmain).js$/,
            loader: 'file-loader' 
        }, {
            test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
            loader: 'file-loader' 
        }]
    },
    plugins: [
        extractScss
    ]
};
