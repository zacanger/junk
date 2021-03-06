'use strict'

var appName = 'IonicTest'
  , gulp = require('gulp')
  , plugins = require('gulp-load-plugins')()
  , del = require('del')
  , beep = require('beepbeep')
  , express = require('express')
  , path = require('path')
  , open = require('open')
  , stylish = require('jshint-stylish')
  , connectLr = require('connect-livereload')
  , streamqueue = require('streamqueue')
  , runSequence = require('run-sequence')
  , merge = require('merge-stream')
  , ripple = require('ripple-emulator')
  , browserify = require('browserify')
  , source = require('vinyl-source-stream')
  , buffer = require('vinyl-buffer')
  , uglify = require('gulp-uglify')
  , sourcemaps = require('gulp-sourcemaps')
  , gutil = require('gulp-util')

var args = require('yargs')
  .alias('e', 'emulate')
  .alias('b', 'build')
  .alias('r', 'run')
  .alias('release', 'strip-debug')
  .default('build', false)
  .default('port', 9000)
  .default('strip-debug', false)
  .argv

var build = !!(args.build || args.emulate || args.run)
  , emulate = args.emulate
  , run = args.run
  , port = args.port
  , stripDebug = !!args.stripDebug
  , targetDir = path.resolve(build ? 'www' : '.tmp')

// if we just use emualate or run without specifying platform, we assume iOS
// in this case the value returned from yargs would just be true
if (emulate === true) {
  emulate = 'ios'
}
if (run === true) {
  run = 'ios'
}

// global error handler
var errorHandler = function(error){
  if (build) {
    throw error
  } else {
    beep(2, 170)
    plugins.util.log(error)
  }
}

// clean target dir
gulp.task('clean', function(done){
  del([targetDir, 'app/scripts/bundle.js', 'app/scripts/bundle.js.map'], done)
})

// precompile .scss and concat with ionic.css
gulp.task('styles', function(){
  var options = build ? {style: 'compressed'} : {style: 'expanded'}

  var sassStream = gulp.src('app/styles/main.scss')
    .pipe(plugins.sass(options))
    .on('error', function (err){
      console.log('err: ', err)
      beep()
    })

  // build ionic css dynamically to support custom themes
  var ionicStream = gulp.src('app/styles/ionic-styles.scss')
    .pipe(plugins.cached('ionic-styles'))
    .pipe(plugins.sass(options))
    // cache and remember ionic .scss in order to cut down re-compile time
    .pipe(plugins.remember('ionic-styles'))
    .on('error', function(err){
      console.log('err: ', err)
      beep()
    })

  return streamqueue({objectMode: true}, ionicStream, sassStream)
    .pipe(plugins.autoprefixer('last 1 Chrome version', 'last 3 iOS versions', 'last 3 Android versions'))
    .pipe(plugins.concat('main.css'))
    .pipe(plugins.if(build, plugins.stripCssComments()))
    .pipe(plugins.if(build && !emulate, plugins.rev()))
    .pipe(gulp.dest(path.join(targetDir, 'styles')))
    .on('error', errorHandler)
})

gulp.task('browserify', function(){
  var b = browserify({
    entries: './app/src/app.js',
    debug: !build
  })

  return b.bundle()
    .pipe(source('bundle.js'))
    .pipe(buffer())
    .pipe(sourcemaps.init({loadMaps: true}))
    .pipe(uglify())
    .on('error', gutil.log)
    .pipe(plugins.if(!build, sourcemaps.write('./')))
    .pipe(gulp.dest('./app/scripts/'))
})

gulp.task('scripts', ['browserify'], function(){
  var dest = path.join(targetDir, 'scripts')

  var minifyConfig = {
    collapseWhitespace: true,
    collapseBooleanAttributes: true,
    removeAttributeQuotes: true,
    removeComments: true
  }

  var templateStream = gulp
    .src('**/*.html', {cwd: 'app/templates'})
    .pipe(plugins.angularTemplatecache('templates.js', {
      root: 'templates/',
      module: appName,
      htmlmin: build && minifyConfig
    }))

  var scriptStream = gulp
    .src(['bundle.js', 'bundle.js.map', 'configuration.js', 'templates.js'], {cwd: 'app/scripts'})
    .pipe(plugins.if(!build, plugins.changed(dest)))

  return streamqueue({objectMode: true}, scriptStream, templateStream)
    .pipe(plugins.if(build, plugins.ngAnnotate()))
    .pipe(plugins.if(stripDebug, plugins.stripDebug()))
    .pipe(plugins.if(build, plugins.concat('app.js')))
    .pipe(plugins.if(build, plugins.uglify()))
    .pipe(plugins.if(build && !emulate, plugins.rev()))
    .pipe(gulp.dest(dest))
    .on('error', errorHandler)
})

gulp.task('fonts', function(){
  return gulp
    .src(['app/fonts/*.*', 'bower_components/ionic/release/fonts/*.*'])
    .pipe(gulp.dest(path.join(targetDir, 'fonts')))
    .on('error', errorHandler)
})

gulp.task('templates', function(){
  return gulp.src('app/templates/**/*.*')
    .pipe(gulp.dest(path.join(targetDir, 'templates')))
    .on('error', errorHandler)
})

gulp.task('iconfont', function(){
  return gulp.src('app/icons/*.svg', {
    buffer: false
  })
    .pipe(plugins.iconfontCss({
      fontName: 'ownIconFont',
      path: 'app/icons/own-icons-template.css',
      targetPath: '../styles/own-icons.css',
      fontPath: '../fonts/'
    }))
    .pipe(plugins.iconfont({
      fontName: 'ownIconFont'
    }))
    .pipe(gulp.dest(path.join(targetDir, 'fonts')))
    .on('error', errorHandler)
})

gulp.task('images', function(){
  return gulp.src('app/images/**/*.*')
    .pipe(gulp.dest(path.join(targetDir, 'images')))
    .on('error', errorHandler)
})

gulp.task('jsHint', function(){
  return gulp
    .src('app/src/**/*.js')
    .pipe(plugins.jshint())
    .pipe(plugins.jshint.reporter(stylish))
    .on('error', errorHandler)
})

gulp.task('vendor', function(){
  var vendorFiles = require('./vendor.json')

  return gulp.src(vendorFiles)
    .pipe(plugins.concat('vendor.js'))
    .pipe(plugins.if(build, plugins.uglify()))
    .pipe(plugins.if(build, plugins.rev()))
    .pipe(gulp.dest(targetDir))
    .on('error', errorHandler)
})

gulp.task('index', ['jsHint', 'scripts'], function(){
  var cssNaming = 'styles/main*'

  var _inject = function (src, tag) {
    return plugins.inject(src, {
      starttag: '<!-- inject:' + tag + ':{{ext}} -->',
      read: false,
      addRootSlash: false
    })
  }

  var _getAllScriptSources = function(){
    var scriptStream = gulp.src(['scripts/app.js', 'scripts/**/*.js'], {cwd: targetDir})
    return streamqueue({objectMode: true}, scriptStream)
  }

  return gulp.src('app/index.html')
    // inject css
    .pipe(_inject(gulp.src(cssNaming, {cwd: targetDir}), 'app-styles'))
    // inject vendor.js
    .pipe(_inject(gulp.src('vendor*.js', {cwd: targetDir}), 'vendor'))
    // inject app.js (build) or all js files indivually (dev)
    .pipe(plugins.if(build,
      _inject(gulp.src('scripts/app*.js', {cwd: targetDir}), 'app'),
      _inject(_getAllScriptSources(), 'app')
    ))

    .pipe(gulp.dest(targetDir))
    .on('error', errorHandler)
})


gulp.task('serve', function(){
  express()
    .use(!build ? connectLr() : function(){})
    .use(express.static(targetDir))
    .listen(port)
  open('http://127.0.0.1:' + port + '/')
})

gulp.task('ionic:emulate', plugins.shell.task([
  'ionic emulate ' + emulate + ' --livereload --consolelogs'
]))


gulp.task('ionic:run', plugins.shell.task([
  'ionic run ' + run
]))

gulp.task('icon', plugins.shell.task([
  'ionic resources --icon'
]))
gulp.task('splash', plugins.shell.task([
  'ionic resources --splash'
]))
gulp.task('resources', plugins.shell.task([
  'ionic resources'
]))

gulp.task('select', plugins.shell.task([
  './helpers/emulateios'
]))

gulp.task('ripple', ['scripts', 'styles', 'watchers'], function(){
  var options = {
    keepAlive: false,
    open: true,
    port: 4444
  }

  ripple.emulate.start(options)
  open('http://127.0.0.1:' + options.port + '?enableripple=true')
})

gulp.task('watchers', function () {
  plugins.livereload.listen()
  gulp.watch('app/styles/**/*.scss', ['styles'])
  gulp.watch('app/fonts/**', ['fonts'])
  gulp.watch('app/icons/**', ['iconfont'])
  gulp.watch('app/images/**', ['images'])
  gulp.watch(['app/scripts/**/*.js', '!app/scripts/bundle.js'], ['index'])
  gulp.watch('./vendor.json', ['vendor'])
  gulp.watch('app/templates/**/*.html', ['index'])
  gulp.watch('app/index.html', ['index'])
  gulp.watch('app/src/**/*.js', ['scripts'])
  gulp.watch(targetDir + '/**')
    .on('change', plugins.livereload.changed)
    .on('error', errorHandler)
})

gulp.task('noop', function(){})

gulp.task('default', function(done){
  runSequence(
    'clean',
    'iconfont',
    [
      'fonts',
      'templates',
      'styles',
      'images',
      'vendor'
    ],
    'index',
    build ? 'noop' : 'watchers',
    build ? 'noop' : 'serve',
    emulate ? ['ionic:emulate', 'watchers'] : 'noop',
    run ? 'ionic:run' : 'noop',
    done)
})
