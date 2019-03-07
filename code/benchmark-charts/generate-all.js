const fs = require('fs')
const { columnChart, lineChart, writeResult } = require('./charts');
const { extname, basename, join } = require('path');

if (process.argv.length < 4) {
  console.log('Usage: generate-all.js [benchmark-folder] [output-folder]')
  process.exit(1)
}

function list (dir) {
  const files = fs.readdirSync(dir);
  return files
    .map(file => join(dir, file))
    .filter(file => fs.statSync(file).isFile())
    .filter(file => extname(file) === '.json')
};


function base (file) {
  return basename(file, extname(file))
}

const input = process.argv[2]
const output = process.argv[3]


const files = list(input)

files.forEach(file => {

  const data = JSON.parse(fs.readFileSync(file).toString())
  
  const columnPath = join(output, `${base(file)}-column.html`)
  const linePath = join(output, `${base(file)}-line.html`)

  writeResult(columnPath, columnChart(data), () => console.log(`Generated column charts for ${file} to ${columnPath}`))
  writeResult(linePath, lineChart(data), () => console.log(`Generated line charts for ${file} to ${linePath}`))
})



