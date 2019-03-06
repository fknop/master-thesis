const fs = require('fs')
const { lineChart, writeResult } = require('./charts')

if (process.argv.length < 4) {
  console.log('Usage: line-chart.js [input-data] [output-chart]')
  process.exit(1)
}

const input = process.argv[2]
const output = process.argv[3]

const data = fs.readFileSync(input).toString()
const result = lineChart(JSON.parse(data))

writeResult(output, result, (err) => {
  if (!err) {
    console.log('Generated line charts to ' + output)
  }
})