module.exports = {
  entry: './app/App.js',
  output: {
    path: './public',
    filename: 'bundle.js'
  },
  module: {
    loaders: [
      {
        test: /\.js$/,
        include: /app/,
        exclude: /node_modules/,
        loader: 'babel?presets[]=react'
      }
    ]
  }
};