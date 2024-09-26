#!/usr/bin/env node
const { program } = require('commander');
const fs = require('node:fs');
const webFS = require('fs').promises;
const filenameRegex = /\w+\.yml/ //may need to expand character set
const express = require('express');
const serve = require('express-static');

const app = express();
const host = '127.0.0.1';
const port = 8080;

program.command('load')
  .arguments('<file>')
  .action(function(file)
  {
    if (file.match(filenameRegex) != null)
    {
      try
      {
        //read data stream if file is large
        //const data = fs.readFileSync(__dirname + "/" + file, 'utf8');
        const data = fs.readFileSync(process.cwd() + "/" + file, 'utf8'); //assuming file is located at command line's current directory
        console.log(data);

        app.use(serve(__dirname + '/dist/article'));

        const server = app.listen(port, host, function()
        {
        console.log("Server running: " + __dirname + '/dist/article\n' +
                    `Access on http://${host}:${port}`
                   );
        })
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