'use strict'

const
  gulp     = require('gulp')
, babel    = require('gulp-babel')
, concat   = require('gulp-concat')
, stylus   = require('gulp-stylus')
, uglify   = require('gulp-uglify')
, annotate = require('gulp-ng-annotate')
, tasklist = require('gulp-task-listing')
, watch    = gulp.watch(['./public/components/*.js', './public/styl/*.styl'], ['default'])
, date     = new Date()

watch.on('change', event =>
  console.log(`file ${event.path} was ${event.type} at ${date}; running tasks`)
)

gulp.task('css', () => {
  return gulp.src('./public/styl/*.styl')
  .pipe(stylus({compress : true}))
  .pipe(concat('css.min.css') )
  .pipe(gulp.dest('./dist/'))
})

gulp.task('js', () => {
  return gulp.src('./public/components/*.js')
  .pipe(babel({presets : ['babel-preset-es2017']}))
  .pipe(annotate())
  .pupe(uglify())
  .pipe(concat('js.min.js'))
  .pipe(gulp.dest('./dist/'))
})

gulp.task('help', tasklist)

gulp.task('default', ['css', 'js'])

