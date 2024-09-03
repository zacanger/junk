module.exports = {
  entry: {
    app: './app/App.js'
  },
  module: {
    loaders: [
      {
        test: /\.js$/,
        include: /app/,
        exclude: /node_modules/,
        loader: 'babel-loader'
      }
    ]
  },
  output: {
    filename: './public/js/bundle.js'
  }
}
