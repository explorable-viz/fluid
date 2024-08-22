#!/usr/bin/env node
const { program } = require('commander');
const fs = require('node:fs');
const filenameRegex = /\w+\.yml/ //may need to expand character set

program.command('load')
  .arguments('<file>')
  .action(function(file) {
    if (program.args.length > 2)
    {
      console.error("Only 2 commands allowed: load <filename>.yml")
    }
    else
    {
      if (file.match(filenameRegex) != null)
      {
        try
        {
          const data = fs.readFileSync(__dirname + "/" + file, 'utf8');
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
    }
  });

program.parse()
