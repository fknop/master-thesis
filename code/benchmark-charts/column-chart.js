const fs = require('fs')
const { columnChart, writeResult } = require('./charts')

if (process.argv.length < 4) {
  console.log('Usage: column-chart.js [input-data] [output-chart]')
  process.exit(1)
}

const input = process.argv[2]
const output = process.argv[3]

const data = fs.readFileSync(input).toString()
const result = columnChart(JSON.parse(data))

writeResult(output, result, (err) => {
  if (!err) {
    console.log('Generated column charts to ' + output)
  }
})