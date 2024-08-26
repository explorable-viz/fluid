#!/usr/bin/env node
const { program } = require('commander');
const fs = require('node:fs');
const filenameRegex = /\w+\.yml/ //may need to expand character set

//NOTE: serve html file in dist/app/fluid/ using express e.g. app.use(serve(__dirname + '/dist/app'));
program.command('load')
  .arguments('<file>')
  .action(function(file)
  {
    if (file.match(filenameRegex) != null)
    {
      try
      {
        const data = fs.readFileSync(__dirname + "/" + file, 'utf8'); //read data stream if file is large
        console.log(data);
      }
      catch (err)
      {
        console.error(err);
      }
    }
    else
    {
      console.error("Invalid input file: Must be a .yml file.")
    }
  });

program.parse()
