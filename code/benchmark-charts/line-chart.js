const fs = require('fs')

if (process.argv.length < 4) {
  console.log('Usage: line-chart.js [input-data] [output-chart]')
  process.exit(1)
}

const input = process.argv[2]
const output = process.argv[3]

// TODO: check if input is relative path, otherwise it will fail
const data = require(input)

const template = fs.readFileSync('template/line-chart.html').toString()
const result = template.replace('{ /** DATA **/ }', JSON.stringify(data, null, 4))


// TODO create folder recursively
fs.writeFileSync(output, result)