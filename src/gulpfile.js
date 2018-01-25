// dependencies
var gulp = require('gulp');
var gulptsc = require('gulp-typescript');
var sourcemaps = require('gulp-sourcemaps');
var browsersync = require('browser-sync');
var superstatic = require('superstatic');

// project directories
var tsProject = gulptsc.createProject('./tsconfig.json');
var config = {
    lcalcFiles: './*.lcalc',
    tsFiles: './*.ts',
    typings: './typings/**/*.d.ts',
    outputPath: './js/'
};

// compile all .ts files for app
gulp.task('compile', () =>
    gulp.src(config.tsFiles)
        .pipe(sourcemaps.init())
        .pipe(tsProject())
        .pipe(sourcemaps.write('.'))
        .pipe(gulp.dest(config.outputPath))
);

gulp.task('serve', ['compile'], () => {

    gulp.watch([config.tsFiles, config.lcalcFiles], ['compile']);

    // serve app to the browser and reload on change
    browsersync({
        port: 3000,
        files: ['index.html', '**/*.js'],
        injectChanges: true,
        logFileChanges: false,
        logLevel: 'silent',
        notify: true,
        reloadDelay: 0,
        server: {
            baseDir: './',
            middleware: superstatic({
                debug: false
            })
        }
    });

});

gulp.task('default', ['serve']);
