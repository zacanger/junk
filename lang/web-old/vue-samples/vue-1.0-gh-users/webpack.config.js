module.exports = {
	entry: './src/index.js',
	output: {
		path: __dirname + '/build/',
		publicPath: 'build/',
		filename: 'build.js'
	},
	module: {
		loaders: [
			{test:  /\.vue$/, loader: 'vue'},
			{test: /\.js$/, loader: 'babel', exclude: /node_modules/}
		]
	},
	babel: {
		presets: ['es2015'],
		plugins: ['transform-runtime']
	}
}

