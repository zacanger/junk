module.exports = {
    entry: './src/App.jsx'
  , output: {
      path: './public/js'
    , filename: 'bundle.js'
  },
  module: {
    loaders: [{
      test: /\.js$/, // all files that end in '.js' will be run through this loader
      include: /src/, // only run through files located in the `src` directory
      exclude: /node_modules/, // ignore the `node_modules` directory
      loader: 'babel?presets[]=react'
    }]
  }
}

